module MonoidDatan (
  Count(..),
  monoidSum,
  monoidProd,
  monoidMin,
  monoidMax,
  count,
  Mean(..),
  mean) where

import MyMonoid


-- http://twdkz.wordpress.com/2013/05/31/data-analysis-with-monoids
newtype Count = Count Integer deriving (Show, Read, Eq, Ord)

instance MyMonoid Count where
  monoidEmpty = Count 0
  Count n1 `monoidAppend` Count n2 = Count (n1 + n2)


monoidSum :: (Num a) => a -> MonoidSum a
monoidSum = MonoidSum

monoidProd :: (Num a) => a -> MonoidProd a
monoidProd = MonoidProd

monoidMin :: (Ord a) => a -> Smallest a
monoidMin = Smallest

monoidMax :: (Ord a) => a -> Largest a
monoidMax = Largest

count :: a -> Count
count _ = Count 1


newtype Mean a = Mean (MonoidSum a, Count) deriving (Show, Read)

instance (Eq a, Num a) => Eq (Mean a) where
  Mean (MonoidSum a1, Count b1) == Mean (MonoidSum a2, Count b2) = (a1 * (fromIntegral b2)) == (a2 * (fromIntegral b1))

instance (Ord a, Num a) => Ord (Mean a) where
  Mean (MonoidSum a1, Count b1) `compare` Mean (MonoidSum a2, Count b2) = (a1 * (fromIntegral b2)) `compare` (a2 * (fromIntegral b1))

instance (Num a) => MyMonoid (Mean a) where
  monoidEmpty = Mean monoidEmpty
  Mean m1 `monoidAppend` Mean m2 = Mean (monoidAppend m1 m2)

mean v = Mean (MonoidSum v, Count 1)
