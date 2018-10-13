{-# OPTIONS_GHC -Wall -fno-warn-partial-type-signatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Data.Kind

import Data.Functor.Const
import Data.Functor.Identity
import Data.Monoid
import qualified Data.Text as T

import Prelude -- hiding (Functor(..))

{-
class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b

instance Functor ((,) b) where
  fmap f (b, a) = (b, f a)

newtype Fun a r = Fun (a -> r)

instance Functor (Fun a) where
  fmap f (Fun fun) = Fun (f . fun)
-}

----

class Contravariant (f :: Type -> Type) where
  contramap :: (a -> b) -> f b -> f a

newtype Pred a = Pred { runPred :: a -> Bool }

instance Contravariant Pred where
  contramap f (Pred p) = Pred (p . f)

isEven :: Pred Int
isEven = Pred ((== 0) . (`rem` 2))

lengthIsEven :: Pred String
lengthIsEven = contramap length isEven

----

data Fold a b = forall s. Fold (s -> a -> s) s (s -> b)

instance Profunctor Fold where
  dimap f g (Fold step begin extract) = Fold (\s a -> step s (f a)) begin (g . extract)

newtype P1 a b = P1 (a -> (b, b))
newtype P2 a b = P2 (a -> (Bool, b))
newtype P3 a b = P3 (a -> ())

instance Profunctor P1 where
  dimap i o (P1 f) = P1 $ (\(b1, b2) -> (o b1, o b2)) . f . i

instance Profunctor P2 where
  dimap i o (P2 f) = P2 $ (\(p, r) -> (p, o r)) . f . i
  
instance Profunctor P3 where
  dimap i _o (P3 f) = P3 $ f . i

----

class Profunctor (p :: Type -> Type -> Type) where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'

instance Profunctor (->) where
  dimap i o f = o . f . i

newtype Star f a b = Star { unStar :: a -> f b }

instance Functor f => Profunctor (Star f) where
  dimap i o (Star f) = Star $ fmap o . f . i

----

swapP :: (a, b) -> (b, a)
swapP (a, b) = (b, a)

cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
cross f g (x, y) = (f x, g y)

-- Intuitively a profunctor is cartesian if it can pass around additional
-- context in the form of a pair.

class Profunctor p => Cartesian p where
  first  :: p a b -> p (a, c) (b, c)
  first = dimap swapP swapP . second

  second :: p a b -> p (c, a) (c, b)
  second = dimap swapP swapP . first

instance Cartesian (->) where
  first :: (a -> b) -> (a, c) -> (b, c)
  first f = f `cross` id

instance Functor f => Cartesian (Star f) where
  first :: Star f a b -> Star f (a, c) (b, c)
  first (Star f) = Star $ (\(fx, y) -> (, y) <$> fx) . (f `cross` id)

----

-- Intuitively a profunctor is cocartesian if it can act on one part of the sum
-- type while leaving the other part alone.

swapE :: Either a b -> Either b a
swapE (Left a)  = Right a
swapE (Right a) = Left a

class Profunctor p => Cocartesian p where
  left  :: p a b -> p (Either a c) (Either b c)
  left = dimap swapE swapE . right

  right :: p a b -> p (Either c a) (Either c b)
  right = dimap swapE swapE . left

instance Cocartesian (->) where
  left :: (a -> b) -> Either a c -> Either b c
  left f = either (Left . f) Right

instance Applicative f => Cocartesian (Star f) where
  left :: Star f a b -> Star f (Either a c) (Either b c)
  left (Star f) = Star $ either (fmap Left . f) (pure . Right)

----

class (Cartesian p, Cocartesian p) => Wander p where
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t)
         -> p a b
         -> p s t

instance Wander (->) where
  wander :: ((a -> Identity b) -> s -> Identity t)
         -> (a -> b)
         -> (s -> t)
  wander t f = runIdentity . t (Identity . f)

instance Applicative f => Wander (Star f) where
  wander :: ((a -> f b) -> s -> f t)
         -> Star f a b
         -> Star f s t
  wander t (Star f) = Star (t f)

----

data Adapter a b s t = Adapter (s -> a) (b -> t)

instance Profunctor (Adapter a b) where
  dimap :: (s' -> s) -> (t -> t') -> Adapter a b s t -> Adapter a b s' t'
  dimap f g (Adapter from to) = Adapter (from . f) (g . to)

adapterP :: Adapter a b s t -> AdapterP a b s t
adapterP (Adapter from to) pab = dimap from to pab

adapter :: forall a b s t. AdapterP a b s t -> Adapter a b s t
adapter l = l (Adapter id id :: Adapter a b a b)

text :: AdapterP T.Text T.Text String String
text = adapterP $ Adapter T.pack T.unpack

_Sum :: AdapterP (Sum a) (Sum b) a b
_Sum = adapterP $ Adapter Sum getSum

_Product :: AdapterP (Product a) (Product b) a b
_Product = adapterP $ Adapter Product getProduct

_First :: AdapterP (First a) (First b) (Maybe a) (Maybe b)
_First = adapterP $ Adapter First getFirst

----

data Lens a b s t = Lens (s -> a) (s -> b -> t)

instance Profunctor (Lens a b) where
  dimap :: (s' -> s) -> (t -> t') -> Lens a b s t -> Lens a b s' t'
  dimap f g (Lens view update) = Lens (view . f) (\s -> g . update (f s))

instance Cartesian (Lens a b) where
  first :: Lens a b s t -> Lens a b (s, c) (t, c)
  first (Lens view update) = Lens (view . fst) (\(s, c) b -> (update s b, c))

lensP :: forall a b s t. Lens a b s t -> LensP a b s t
lensP (Lens view update) pab =
  dimap (\s -> (view s, s)) (\(b, s) -> update s b) (first pab :: _ (a, s) (b, s))

lens :: LensP a b s t -> Lens a b s t
lens l = l (Lens id (\_ b -> b) :: Lens a b a b)

viewl :: forall a b s t. Optic (Star (Const a)) a b s t -> s -> a
viewl l s = getConst $ unStar (l (Star Const :: Star (Const a) a b)) s

--

_fst :: LensP a b (a, c) (b, c)
_fst = lensP $ Lens fst (\(_, c) b -> (b, c))

_snd :: LensP a b (c, a) (c, b)
_snd = lensP $ Lens snd (\(c, _) b -> (c, b))

----

data Prism a b s t = Prism (s -> Either t a) (b -> t)

instance Profunctor (Prism a b) where
  dimap :: (s' -> s) -> (t -> t') -> Prism a b s t -> Prism a b s' t'
  dimap f g (Prism match build) = Prism (either (Left . g) Right . match . f) (g . build)

instance Cocartesian (Prism a b) where
  left :: Prism a b s t -> Prism a b (Either s c) (Either t c)
  left (Prism match build) =
    Prism (either (either (Left . Left) Right . match) (Left . Right)) (Left . build)

prismP :: forall a b s t. Prism a b s t -> PrismP a b s t
prismP (Prism match build) pab =
  dimap match (either id build) (right pab :: _ (Either t a) (Either t b))

prism :: forall a b s t. PrismP a b s t -> Prism a b s t
prism l = l (Prism Right id :: Prism a b a b)

preview :: Optic (Star (Const (First a))) a b s t -> s -> Maybe a
preview l s = getFirst . getConst $ unStar (l (Star (Const . First . Just))) s

_Left :: PrismP a b (Either a c) (Either b c)
_Left = prismP $ Prism match Left
  where
    match (Left a)  = Right a
    match (Right t) = Left (Right t)

_Right :: PrismP a b (Either c a) (Either c b)
_Right = prismP $ Prism match Right
  where
    match (Left t)  = Left (Left t)
    match (Right a) = Right a


----

traversalP :: Traversal a b s t -> TraversalP a b s t
traversalP = wander

traversal :: TraversalP a b s t -> Traversal a b s t
traversal = traverseOf

traversed :: Traversable t => TraversalP a b (t a) (t b)
traversed = wander traverse

traverseOf :: Applicative f => Optic (Star f) a b s t -> (a -> f b) -> s -> f t
traverseOf t = unStar . t . Star

both :: TraversalP a b (a, a) (b, b)
both = wander $ \t (a1, a2) -> (,) <$> t a1 <*> t a2

----

type Optic p a b s t = p a b -> p s t

type AdapterP a b s t = forall p. Profunctor p => Optic p a b s t

type LensP a b s t = forall p. Cartesian p => Optic p a b s t

type PrismP a b s t = forall p. Cocartesian p => Optic p a b s t

type TraversalP a b s t = forall p. Wander p => Optic p a b s t

----

type Traversal a b s t = forall f. Applicative f => (a -> f b) -> s -> f t

traversedC :: Traversable t => Traversal a b (t a) (t b)
traversedC = traverse

bothC :: Traversal a b (a, a) (b, b)
bothC t (a1, a2) = (,) <$> t a1 <*> t a2

sample1 :: [(Either Int Char, String)]
sample1 = [(Left 1, "asd"), (Right 'a', "xxx"), (Left 7, "qwe")]

sample2 :: [Either (Int, Int) [Char]]
sample2 = [Left (4, 5), Right "hey", Left (1, 9)]

-- _fst
-- _fst . _Left
-- viewl
-- preview

-- sample1
-- sample2
