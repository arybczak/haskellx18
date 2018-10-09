Profunctors and data accessors
==============================

------------------------------------------------------------------------

Data accessors - motivation
===========================

The problem:

Haskell doesn't have user friendly, available out of the box set of
features for modification and access of nested data structures.

Built-in record syntax is clunky to use and doesn't compose well.

Solution: lenses

...and adapters, prisms, traversals etc.

The whole family of these is known as optics.

------------------------------------------------------------------------

Data accessors - encodings
==========================

-   Concrete (tutorials)
    -   Easy to understand
    -   Don't compose well (what is composition of a lens and a prism?)
-   Van Laarhoven (lens library)
    -   Composable
    -   Lenses and traversals don't require anything other than base
        library
    -   Convoluted types
    -   The hierarchy of optics is not clear from the types
-   Profunctor (purescript-profunctor-lenses library)
    -   Composable
    -   Require profunctors
    -   Types still somewhat convoluted
    -   Hierarchy of optics is clear from the types

------------------------------------------------------------------------

Optics - adapter
================

    !haskell
    data Adapter a b s t = Adapter { from :: s -> a
                                   , to   :: b -> t
                                   }

    _text :: Adapter T.Text T.Text String String
    _text = Adapter T.pack T.unpack

-   Also known as Iso (lens library).
-   Provides a way to convert back and forth between the types.
-   Usually an isomorphism, although we don't enforce it here.

------------------------------------------------------------------------

Optics - lens
=============

Lens is a pair of two functions, a getter and a setter (updater).

    !haskell
    data Lens a b s t = Lens { view   :: s -> a
                             , update :: s -> b -> t
                             }

    _fst :: Lens a b (a, c) (b, c)
    _fst = Lens fst (\(_, c) b -> (b, c))

    _snd :: Lens a b (c, a) (c, b)
    _snd = Lens snd (\(c, _) b -> (c, b))

-   Allows us to
    -   view a value contained within a larger data structure,
    -   update a specific part of it.
-   Deals with product types.

------------------------------------------------------------------------

Optics - prism
==============

    !haskell
    data Prism a b s t = Prism { match :: s -> Either t a
                               , build :: b -> t
                               }

    _Left :: Prism a b (Either a c) (Either b c)
    _Left = Prism match Left
      where
        match (Left a)  = Right a
        match (Right t) = Left (Right t)

    _Right :: Prism a b (Either c a) (Either c b)
    _Right = Prism match Right
      where
        match (Left t)  = Left (Left t)
        match (Right a) = Right a

-   Allows us to
    -   match on a value possibly contained within a larger data
        structure,
    -   build the data structure back from a specific value.
-   Deals with sum types.

------------------------------------------------------------------------

Optics - traversal
==================

    !haskell
    newtype Traversal a b s t =
      Traversal (forall f. Applicative f => (a -> f b) -> s -> f t)

    traversed :: Traversable t => Traversal a b (t a) (t b)
    traversed = Traversal traverse

    both :: Traversal a b (a, a) (b, b)
    both = Traversal $ \t (a1, a2) -> (,) <$> t a1 <*> t a2

-   Generalization of `traverse` from `Data.Traversable`.
-   Allows us to
    -   match and/or modify multiple values possibly contained within a
        larger data structure in a predefined order,
    -   apply effects specific to `f`.
-   Deals with a sequence of values of any type.

------------------------------------------------------------------------

Optics - hierarchy
==================

How do these optics relate to each other?

![image](hierarchy.svg)

-   every Adapter is a Lens
-   every Adapter is a Prism
-   every Lens is a Traversal
-   every Prism is a Traversal

How can we see that?

------------------------------------------------------------------------

Profunctor
==========

Represented by a type class. In order to arrive at the definition, we
need to backtrack a little bit.

------------------------------------------------------------------------

Covariant functor
=================

A type class we all know and appreciate.

    !haskell
    class Functor (f :: Type -> Type) where
      fmap :: (a -> b) -> f a -> f b

-   Intuitively `f a` will **output** values of type `a` in a context specific
    to `f`.
-   `fmap` allows us to map over `f` to change the type of its **output**.
-   We say that the argument `a` in `f a` is covariant.

Examples: `Identity`, `Maybe`, `IO`, `Either a`.

------------------------------------------------------------------------

Contravariant functor
=====================

Significantly less popular.

    !haskell
    class Contravariant (f :: Type -> Type) where
      contramap :: (b -> a) -> f a -> f b

-   Intuitively `f a` will take values of type
    `a` as an **input** in a context specific to
    `f`.
-   `contramap` allows us to map over `f` to
    change the type of its **input**.
-   We say that the argument `a` in `f a` is
    contravariant.

Example:

    !haskell
    newtype Pred a = Pred { runPred :: a -> Bool }

    instance Contravariant Pred where
      contramap f (Pred p) = Pred (p . f)

    isEven :: Pred Int
    isEven = Pred ((== 0) . (`rem` 2))

    isLengthEven :: Pred String
    isLengthEven = contramap length isEven

------------------------------------------------------------------------

Profunctor
==========

    !haskell
    class Profunctor (p :: Type -> Type -> Type) where
      dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'

- An instance of a profunctor is a type `p a b` where the first argument is
contravariant and the second argument is covariant.
- Intuitively it is a transformer that consumes values of type
`a` and produces values of type `b`.

Canonical example:

    !haskell
    instance Profunctor (->) where
      dimap i o f = o . f . i

More interesting example (foldl package):

    !haskell
    data Fold a b = forall s. Fold (s -> a -> s) s (s -> b)

    instance Profunctor Fold where
      dimap f g (Fold step begin extract) =
        Fold (\s a -> step s (f a)) begin (g . extract)

------------------------------------------------------------------------

Functors that went pro
======================

    !haskell
    newtype P1 a b = P1 (a -> (b, b))
    newtype P2 a b = P2 (a -> (Bool, b))
    newtype P3 a b = P3 (a -> ())

Which of the above are profunctors?

------------------------------------------------------------------------

Functors that went pro
======================

    !haskell
    newtype P1 a b = P1 (a -> (b, b))
    newtype P2 a b = P2 (a -> (Bool, b))
    newtype P3 a b = P3 (a -> ())

Which of the above are profunctors?

All of them:

    !haskell
    instance Profunctor P1 where
      dimap i o (P1 f) = P1 $ (\(b1, b2) -> (o b1, o b2)) . f . i

    instance Profunctor P2 where
      dimap i o (P2 f) = P2 $ (\(p, r) -> (p, o r)) . f . i

    instance Profunctor P3 where
      dimap i _ (P3 f) = P3 $ f . i

This can be generalized to any functor `f`:

    !haskell
    newtype UpStar f a b = UpStar { unUpStar :: a -> f b }

    instance Functor f => Profunctor (UpStar f) where
      dimap i o (UpStar f) = UpStar $ fmap o . f . i
