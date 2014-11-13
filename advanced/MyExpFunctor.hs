{-# LANGUAGE RankNTypes, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module MyExpFunctor(
  Comparison(..),
  Const(..),
  Equivalence(..),
  ForAll(..),
  MyCata(..),
  MyContravariantFunctor(..),
  MyExpFunctor(..),
  Nu(..),
  O(..),
  Predicate(..),
  Rec(..),
  Rollable(..),
  Unrollable(..),
  ($$),
  anaMH,
  cataFS,
  cataMH,
  reroll,
  toMH,
  xmapDefault) where

import MyFunctor


-- http://comonad.com/reader/2008/rotten-bananas

-- | universally quantified $ so we can get away with fewer parentheses
($$) :: ((forall a. f a) -> b) -> (forall a. f a) -> b
f $$ x = f x
infixr 0 $$

newtype ForAll f = ForAll { runForAll :: forall a. f a }


-- 'Contravariant' functors, sometimes referred to colloquially as @Cofunctor@,
-- even though the dual of a 'Functor' is just a 'Functor'. As with 'Functor'
-- the definition of 'Contravariant' for a given ADT is unambiguous.

-- | Any instance should be subject to the following laws:
--
-- > contramap id = id
-- > contramap f . contramap g = contramap (g . f)
--
-- Note, that the second law follows from the free theorem of the type of
-- 'contramap' and the first law, so you need only check that the former
-- condition holds.

class MyContravariantFunctor f where
  contraMap :: (b -> a) -> f a -> f b


newtype Predicate a = Predicate { getPredicate :: a -> Bool }

-- | A 'Predicate' is a 'Contravariant' 'Functor', because 'contramap' can
-- apply its function argument to the input of the predicate.
instance MyContravariantFunctor Predicate where
  contraMap g (Predicate p) = Predicate (p . g)


newtype Const a b = Const a

instance MyContravariantFunctor (Const a) where
  contraMap _ (Const a) = Const a


-- | Defines a total ordering on a type as per 'compare'
newtype Comparison a = Comparison { getComparison :: a -> a -> Ordering }

-- | A 'Comparison' is a 'Contravariant' 'Functor', because 'contramap' can
-- apply its function argument to each input to the
-- comparison function.
instance MyContravariantFunctor Comparison where
  contraMap f g = Comparison $ \a b -> getComparison g (f a) (f b)


-- | Define an equivalence relation
newtype Equivalence a = Equivalence { getEquivalence :: a -> a -> Bool }

-- | Equivalence relations are 'Contravariant', because you can
-- apply the contramapped function to each input to the equivalence
-- relation.
instance Contravariant Equivalence where
  contramap f g = Equivalence $ \a b -> getEquivalence g (f a) (f b)


class MyExpFunctor f where
  xMap :: (a -> b) -> (b -> a) -> f a -> f b

{-
An invariant functor has two laws:
  identity – xmap(ma)(identity, identity) == ma
  composite – xmap(xmap(ma)(f1, g1))(f2, g2) == xmap(ma)(f2 compose f1, g1 compose g2)
-}

-- | Every Functor is trivially an ExpFunctor
xmapDefault :: MyFunctor f => (a -> b) -> (b -> a) -> f a -> f b
xmapDefault = const . funMap

newtype O f g e = Comp { deComp :: f (g e) }

instance (MyFunctor f, MyFunctor g) => MyFunctor (f `O` g) where
  funMap f = Comp . funMap (funMap f) . deComp

instance (MyExpFunctor f, MyExpFunctor g) => MyExpFunctor (f `O` g) where
  xMap f g = Comp . xMap (xMap f g) (xMap g f) . deComp


class Rollable f t | t -> f where
  roll :: f t -> t

class Unrollable f t | t -> f where
  unroll :: t -> f t


newtype Nu f  = Nu { old :: f (Nu f) }

class MyCata f t | t -> f where
  cata :: (f a -> a) -> t -> a

-- Meijer/Hutton
cataMH :: MyExpFunctor f => (f a -> a) -> (a -> f a) -> Nu f -> a
cataMH f g = f . xMap (cataMH f g) (anaMH f g) . old
anaMH :: MyExpFunctor f => (f a -> a) -> (a -> f a) -> a -> Nu f
anaMH f g = Nu . xMap (anaMH f g) (cataMH f g) . g

instance MyExpFunctor f => Rollable f (Nu f) where
  roll = Nu

instance Unrollable f (Nu f) where
  unroll = old

instance MyFunctor f => MyCata f (Nu f) where
  cata f = f . funMap (cata f) . old


-- | Fegaras/Sheard
data Rec f a = Roll (f (Rec f a)) | Place a
cataFS :: MyExpFunctor f => (f a -> a) -> Rec f a -> a
cataFS f (Roll x)  = f (xMap (cataFS f) Place x)
cataFS f (Place x) = x

instance Rollable f (Rec f a) where
  roll = Roll

instance Unrollable f (Rec f a) where
  unroll (Roll x)  = x
  unroll (Place x) = undefined


-- | Change of representation
reroll :: (MyCata f t, Rollable f t') => t -> t'
reroll = cata roll

-- to Meijer/Hutton
toMH :: (MyCata f t, MyFunctor f) => t -> Nu f
toMH = cata Nu
