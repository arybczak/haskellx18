==============================
Profunctors and data accessors
==============================

----

===========
Profunctor
===========

Represented by a type class. In order to arrive at the definition, we need to backtrack a little bit.

----

=================
Covariant functor
=================

A type class we all know and appreciate.

::

    !haskell
    class Functor (f :: Type -> Type) where
      fmap :: (a -> b) -> f a -> f b


- Intuitively `f a` will **output** values of type `a` in a context specific to `f`.
- `fmap` allows us to map over `f` to change the type of its **output**.
- We say that the argument `a` in `f a` is covariant.

Examples: `Identity`, `Maybe`, `IO`, `Either a`.

----

=====================
Contravariant functor
=====================

Significantly less popular.

::

   !haskell
   class Contravariant (f :: Type -> Type) where
     contramap :: (b -> a) -> f a -> f b

- Intuitively `f a` will take values of type `a` as an **input** in a context specific to `f`.
- `contramap` allows us to map over `f` to change the type of its **input**.
- We say that the argument `a` in `f a` is contravariant.

Example:

::

   !haskell
   newtype Pred a = Pred { runPred :: a -> Bool }

   instance Contravariant Pred where
     contramap f (Pred p) = Pred (p . f)

   isEven :: Pred Int
   isEven = Pred ((== 0) . (`rem` 2))

   isLengthEven :: Pred String
   isLengthEven = contramap length isEven

----

=========
Profunctor
=========

::

   !haskell
   class Profunctor (p :: Type -> Type -> Type) where
     dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'

An instance of a profunctor is a type `p a b` where the first argument is contravariant and the second argument is covariant.

Intuitively it is a transformer that consumes values of type `a` and produces values of type `b`.

Canonical example:
::

   !haskell
   instance Profunctor (->) where
     dimap i o f = o . f . i

More interesting example (foldl package):

::

   !haskell
   data Fold a b = forall s. Fold (s -> a -> s) s (s -> b)

   instance Profunctor Fold where
     dimap f g (Fold step begin extract) =
       Fold (\s a -> step s (f a)) begin (g . extract)

----

=========================
Functors that went pro
=========================

::

   !haskell
   newtype P1 a b = P1 (a -> (b, b))
   newtype P2 a b = P2 (a -> (Bool, b))
   newtype P3 a b = P3 (a -> ())

Which of the above are profunctors?

----

=========================
Functors that went pro
=========================

::

   !haskell
   newtype P1 a b = P1 (a -> (b, b))
   newtype P2 a b = P2 (a -> (Bool, b))
   newtype P3 a b = P3 (a -> ())

Which of the above are profunctors?

All of them:

::

   !haskell
   instance Profunctor P1 where
     dimap i o (P1 f) = P1 $ (\(b1, b2) -> (o b1, o b2)) . f . i

   instance Profunctor P2 where
     dimap i o (P2 f) = P2 $ (\(p, r) -> (p, o r)) . f . i

   instance Profunctor P3 where
     dimap i _ (P3 f) = P3 $ f . i

This can be generalized to any functor `f`:

::

   !haskell
   newtype UpStar f a b = UpStar { unUpStar :: a -> f b }

   instance Functor f => Profunctor (UpStar f) where
     dimap i o (UpStar f) = UpStar $ fmap o . f . i
