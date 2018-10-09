# Controlling monadic effects with type classes

---

# Motivation

Looking at Haskell function signatures usually gives us some information about what a function can or can't do. Examples:

- `const :: a -> b -> a` - we know that it returns the first argument and ignores the second.
- `fun :: Int -> Int` - we don't exactly know what it does, but we know that it can't do any I/O.

However, things complicate as soon as we start interacting with the IO monad.

`handler :: Request -> IO Response`

We don't know at all what this function can't do from looking at its signature.

But we would like to.

---

# Question

Let's say we are trying to develop an SQL database library. The core will be a function that executes a query:

    !haskell
    data Connection
    data SQL

    runSql :: Connection -> SQL -> IO ()
    runSql = ...

We want to end up in a situation where this function:

- Does not enforce usage of IO / MonadIO constraint on its callers.
- Seamlessly integrates with existing monadic code.

How do we do that?

---

# Answer

We define a type class that captures the functionality needed to implement database operations.

    !haskell
    class Monad m => MonadDB m where
      runSql :: SQL -> m ()

We provide a primitive version of the functions captured by the type class.

    !haskell
    runSqlIO :: Connection -> SQL -> IO ()
    runSqlIO = ...

We define associated monad transfomer that implements an instance of `MonadDB` using primitive version of the functions.

    !haskell
    newtype DBT m a = DBT { unDBT :: ReaderT Connection m a }
      deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

    runDBT :: Connection -> DBT m a -> m a
    runDBT conn m = runReaderT (unDBT m) conn

    instance MonadIO m => MonadDB (DBT m) where
      runSql sql = DBT $ do
        conn <- ask
        liftIO $ runSqlIO conn sql

---

# Answer

Then, if we write monadic code in the following manner:

    !haskell
    doThings :: MonadDB m => String -> m Int
    doThings = ...

We statically know that `doThings` can only interact with a database.

This approach:

- Restricts usage of IO operations since usage of `runSql` requires only `MonadDB` constraint (in particular, it doesn't need `MonadIO`).
- Easily integrates with existing monadic code since `runSql` can be run in any monad that implements `MonadDB` instance.

In addition, as written code is polymorphic in a monad, we can:

- Swap concrete implementations between production and testing environments.
- Flatten deep transformer stack for performance boost if necessary.
- Swap out monad transfomers for effect handlers altogether if/when the right time comes.

---

# Problems

Performance loss if polymorphic code is not specialized at a call site.

Solvable by annotating functions polymorphic in a monad with `INLINABLE` pragma:

    !haskell
    {-# INLINABLE doThings #-}
    doThings :: MonadDB m => String -> m Int
    doThings = ...

If `doThings` is then called in a context where `m` is concrete (usually near the top level of an application), GHC will try its best to generate a specialized version of `doThings` to avoid unnecessary dictionary passing and optimize away the multitude of calls to monadic binds.

---

# Problems

Having N type classes seem to require us to write N² instances and either make independent libraries know about each other or use orphan instances.

Solvable by writing generic, overlappable type class instances using `MonadTrans`.

    !haskell
    class Monad m => MonadDB m where
      runSql :: SQL -> m ()

    instance {-# OVERLAPPABLE #-} (
        MonadTrans t
      , MonadDB m
      , Monad (t m)
      ) => MonadDB (t m) where
        runSql = lift . runSql

    instance MonadIO m => MonadDB (DBT m) where
      ...

Now GHC will auto-derive MonadDB instance for any transformer stack that contains DBT.

---

# Problems

What if the class definition is more complicated?

    !haskell
    class Monad m => MonadDB m where
      runSql            :: SQL -> m ()
      withNewConnection :: m a -> m a

    instance {-# OVERLAPPABLE #-} (
        MonadTrans t
      , MonadDB m
      , Monad (t m)
      ) => MonadDB (t m) where
        runSql                   = lift . runSql
        withNewConnection action = ?

We want to use underlying `withNewConnection :: m a -> m a`, but `action` has type `t m a`.

We need:

- A function `t m a -> m (StT t a)`, where `StT t a` is a state of the transformer `t` to thread `action` through underlying `withNewConnection`.
- A function `m (StT t a) -> t m a` to restore the transformer state at the end.

---

# Problems

`MonadTransControl` class from `monad-control` package does exactly that!

    !haskell
    class MonadTrans t => MonadTransControl t where
      type StT t a :: *
      liftWith :: Monad m => (Run t -> m a) -> t m a
      restoreT :: Monad m => m (StT t a) -> t m a

    type Run t = forall n r. Monad n => t n r -> n (StT t r)

    instance {-# OVERLAPPABLE #-} (
        MonadTransControl t
      , MonadDB m
      , Monad (t m)
      ) => MonadDB (t m) where
        runSql = lift . runSql
        withNewConnection m = liftWith (\run -> run m)
          >>= restoreT . return

Instances of `MonadTransControl` are easy to define for wrappers over standard transformers.

---

# Problems

How powerful is `MonadTransControl`?

    !haskell
    class Monad m => MonadStore k v m | m -> k v where
      update :: k -> (Maybe v -> m (Maybe v, r)) -> m r

We can't write a generic instance for `MonadStore (t m)`, because there is no way to thread the state of `t` through underlying `update`.

But we can generalize the definition of `MonadStore` to make it possible.

    !haskell
    class Monad m => MonadStore k v m | m -> k v where
      gupdate :: k -> (Maybe v -> (Maybe v -> r -> m r) -> m r) -> m r

    instance {-# OVERLAPPABLE #-} (
        MonadTransControl t
      , MonadStore k v m
      , Monad (t m)
      ) => MonadStore k v (t m) where
      gupdate k f = liftWith (\run -> gupdate k (\mv c ->
          run $ f mv (\v r -> restoreT $ c v =<< run (return r))
        )) >>= restoreT . return

    update :: MonadMap k v m => k -> (Maybe v -> m (Maybe v, r)) -> m r
    update k f = snd <$> gupdate k (\mv c -> f mv >>= \r -> c (fst r) r)

---

# Problems

It's also worth noting that `StT t a` doesn't mention `m`, so if the transformer state depends on the underlying monad, in order to define a `MonadTransControl` instance a trick needs to be used.

    !haskell
    data Env (m :: * -> *)

    newtype DBT_ m n a = DBT { unDBT :: ReaderT (Env m) n a }
      deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

    type DBT m a = DBT_ m m a

    instance MonadTransControl (DBT_ m) where
      type StT (DBT_ m) a = StT (ReaderT (Env m)) a
      liftWith = defaultLiftWith DBT unDBT
      restoreT = defaultRestoreT DBT

    instance (m ~ n, MonadIO m) => MonadDB (DBT_ m n) where
      ...

---

# Summary

1. We can structure libraries in a way that makes it possible to easily compose functionality they provide and expose only parts of I/O we need.
2. We can solve issues that arise from such representation, in particular we can avoid quadratic instance blowup that used to be its main limitation.

In the end we get a system of composable effects that is proven to work in practice, as the following libraries make use of the presented ideas:

- `hpqtypes` - PostgreSQL database library
- `log` - structured logging library

and both of them are used in production.
