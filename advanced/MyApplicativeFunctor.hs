module MyApplicativeFunctor (
  MyApplicativeFunctor(..),
  MyZipList(..))where

import MyFunctor
import MyMonoid


class (MyFunctor f) => MyApplicativeFunctor f where
  -- | Lift a value.
  pure :: a -> f a

  -- | Sequential application.
  -- (<*>)
  apply :: f (a -> b) -> f a -> f b

instance MyApplicativeFunctor Maybe where
  pure = Just
  Just f `apply` Just x = Just (f x)
  _ `apply` _ = Nothing

instance MyApplicativeFunctor (Either e) where
  pure = Right
  Right f `apply` Right x = Right (f x)
  Left f `apply` _ = Left f
  _ `apply` Left x = Left x

instance MyApplicativeFunctor ((->) t) where
  pure = const
  f `apply` g = \t -> f t (g t)

instance (MyMonoid a) => MyApplicativeFunctor ((,) a) where
  pure x = (monoidEmpty, x)
  (u, f) `apply` (v, x) = (u `monoidAppend` v, f x)


newtype MyZipList a = MyZipList [a]

instance MyFunctor MyZipList where
  funMap f (MyZipList xs) = MyZipList (map f xs)

instance MyApplicativeFunctor MyZipList where
  (MyZipList fs) `apply` (MyZipList xs) = MyZipList (zipWith ($) fs xs)
  pure x = MyZipList (repeat x)
