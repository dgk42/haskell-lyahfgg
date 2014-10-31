{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

-- http://stackoverflow.com/questions/24000465/step-by-step-deep-explain-the-power-of-coyoneda-preferably-in-scala-throu

import MyFunctor
import MyMonad


newtype MyYoneda f a = MyYoneda { runYoneda :: forall b . (a -> b) -> f b }

instance MyFunctor (MyYoneda f) where
  funMap f y = MyYoneda (\ab -> runYoneda y (ab . f))

toYoneda :: MyFunctor f => f a -> MyYoneda f a
toYoneda fa = MyYoneda (\f -> funMap f fa)

fromYoneda :: MyYoneda f a -> f a
fromYoneda y = runYoneda y id


data MyCoYoneda f a = forall b . MyCoYoneda (b -> a) (f b)

instance MyFunctor (MyCoYoneda f) where
  funMap f (MyCoYoneda mp fb) = MyCoYoneda (f . mp) fb

toCoYoneda :: f a -> MyCoYoneda f a
toCoYoneda fa = MyCoYoneda id fa

fromCoYoneda :: MyFunctor f => MyCoYoneda f a -> f a
fromCoYoneda (MyCoYoneda mp fb) = funMap mp fb

mapCoYoneda :: (forall a . f a -> g a) -> MyCoYoneda f a -> MyCoYoneda g a
mapCoYoneda phi (MyCoYoneda mp fb) = MyCoYoneda mp (phi fb)


-- YEndo ~ Yoneda Endo
data YEndo a = YEndo { yEndo :: forall b . (a -> b) -> (b -> b) }

instance MyFunctor YEndo where
  funMap f y = YEndo (\ab -> yEndo y (ab . f))

instance MyMonad YEndo where
  monadReturn a = YEndo (\ab _ -> ab a)
  y `monadBind` f  = YEndo (\ab b -> yEndo y (\a -> yEndo (f a) ab b) b)

{-
Consider the following two functions:

phi :: f a -> CoYoneda f a
phi x = CoYoneda id x

psi :: Functor f => CoYoneda f a -> f a
psi (CoYoneda g y) = fmap g y

Then obviously psi . phi == id:

(psi . phi) x == fmap id x == x

The above free theorem implies also that phi . psi == id:

(phi . psi) (CoYoneda g y) == CoYoneda id (fmap g y) == CoYoneda g y
-}
