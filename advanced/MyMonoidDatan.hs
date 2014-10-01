module MyMonoidDatan (
  MonoidCount(..),
  monoidSum,
  monoidProd,
  monoidMin,
  monoidMax,
  monoidCount) where

import MyMonoid


-- http://twdkz.wordpress.com/2013/05/31/data-analysis-with-monoids
newtype MonoidCount = MonoidCount Integer deriving (Show)

instance MyMonoid MonoidCount where
  monoidEmpty = MonoidCount 0
  MonoidCount n1 `monoidAppend` MonoidCount n2 = MonoidCount (n1 + n2)


monoidSum :: (Num a) => a -> MonoidSum a
monoidSum = MonoidSum

monoidProd :: (Num a) => a -> MonoidProd a
monoidProd = MonoidProd

monoidMin :: (Ord a) => a -> Smallest a
monoidMin = Smallest

monoidMax :: (Ord a) => a -> Largest a
monoidMax = Largest

monoidCount :: a -> MonoidCount
monoidCount _ = MonoidCount 1


newtype MonoidMean a = MonoidMean (MonoidSum a, MonoidCount) deriving (Show)

instance (Num a) => MyMonoid (MonoidMean a) where
  monoidEmpty = MonoidMean monoidEmpty
  MonoidMean m1 `monoidAppend` MonoidMean m2 = MonoidMean (monoidAppend m1 m2)

monoidMean v = MonoidMean (MonoidSum v, MonoidCount 1)


class (MyMonoid a) => Aggregation a where
  aggResult :: a -> b
