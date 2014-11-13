module MyFoldable (
  MyFoldable(..),
  foldAll,
  foldAnd,
  foldAny,
  foldConcat,
  foldConcatMap,
  foldElem,
  foldFindFirst,
  foldFindLast,
  foldLargest,
  foldLargestBy,
  foldNotElem,
  foldOr,
  foldProd,
  foldSmallest,
  foldSmallestBy,
  foldSum,
  foldToList) where

import MyMonoid


-- Class of data structures that can be folded to a summary value.

-- | Data structures that can be folded.
--
-- Minimal complete definition: 'foldMap' or 'foldr'.
--
-- For example, given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Foldable Tree where
-- >    foldMap f Empty        = mempty
-- >    foldMap f (Leaf x)     = f x
-- >    foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r
--
-- This is suitable even for abstract types, as the monoid is assumed
-- to satisfy the monoid laws.  Alternatively, one could define @foldr@:
--
-- > instance Foldable Tree where
-- >    foldr f z Empty        = z
-- >    foldr f z (Leaf x)     = f x z
-- >    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l
--

class MyFoldable t where
  -- | Combine the elements of a structure using a monoid.
  fold :: MyMonoid m => t m -> m
  fold = foldMap id

  -- | Map each element of the structure to a monoid,
  -- and combine the results.
  foldMap :: MyMonoid m => (a -> m) -> t a -> m
  foldMap f = foldRight (monoidAppend . f) monoidEmpty

  -- | Right-associative fold of a structure.
  --
  -- @'foldr' f z = 'Prelude.foldr' f z . 'toList'@
  foldRight :: (a -> b -> b) -> b -> t a -> b
  foldRight f z t = appMonoidEndo (foldMap (MonoidEndo . f) t) z

  -- | Left-associative fold of a structure.
  --
  -- @'foldl' f z = 'Prelude.foldl' f z . 'toList'@
  foldLeft :: (b -> a -> b) -> b -> t a -> b
  foldLeft f z t = appMonoidEndo (getMonoidDual (foldMap (MonoidDual . MonoidEndo . flip f) t)) z

instance MyFoldable [] where
  foldRight = foldr
  foldLeft = foldl

instance MyFoldable Maybe where
  foldRight _ z Nothing  = z
  foldRight f z (Just x) = f x z

  foldLeft _ z Nothing  = z
  foldLeft f z (Just x) = f z x

instance MyFoldable (Either a) where
  foldMap _ (Left _)  = monoidEmpty
  foldMap f (Right y) = f y

  foldRight _ z (Left _)  = z
  foldRight f z (Right y) = f y z

instance MyFoldable ((,) a) where
  foldMap f (_, y) = f y
  foldRight f z (_, y) = f y z


foldFindFirst :: MyFoldable t => (a -> Bool) -> t a -> Maybe a
foldFindFirst p = getMonoidFirst . foldMap (\x -> if p x then MonoidFirst (Just x)
                                                         else MonoidFirst Nothing)

foldFindLast :: MyFoldable t => (a -> Bool) -> t a -> Maybe a
foldFindLast p = getMonoidLast . foldMap (\x -> if p x then MonoidLast (Just x)
                                                       else MonoidLast Nothing)


-- These use foldr rather than foldMap to avoid repeated concatenation.

-- | List of elements of a structure.
foldToList :: MyFoldable t => t a -> [a]
foldToList t = foldRight (:) [] t

-- | The concatenation of all the elements of a container of lists.
foldConcat :: MyFoldable t => t [a] -> [a]
foldConcat = fold

-- | Map a function over all the elements of a container and concatenate
-- the resulting lists.
foldConcatMap :: MyFoldable t => (a -> [b]) -> t a -> [b]
foldConcatMap = foldMap

-- | 'and' returns the conjunction of a container of Bools.  For the
-- result to be 'True', the container must be finite; 'False', however,
-- results from a 'False' value finitely far from the left end.
foldAnd :: MyFoldable t => t Bool -> Bool
foldAnd = getMonoidAll . foldMap MonoidAll

-- | 'or' returns the disjunction of a container of Bools.  For the
-- result to be 'False', the container must be finite; 'True', however,
-- results from a 'True' value finitely far from the left end.
foldOr :: MyFoldable t => t Bool -> Bool
foldOr = getMonoidAny . foldMap MonoidAny

-- | Determines whether any element of the structure satisfies the predicate.
foldAny :: MyFoldable t => (a -> Bool) -> t a -> Bool
foldAny p = getMonoidAny . foldMap (MonoidAny . p)

-- | Determines whether all elements of the structure satisfy the predicate.
foldAll :: MyFoldable t => (a -> Bool) -> t a -> Bool
foldAll p = getMonoidAll . foldMap (MonoidAll . p)

-- | The 'sum' function computes the sum of the numbers of a structure.
foldSum :: (MyFoldable t, Num a) => t a -> a
foldSum = getMonoidSum . foldMap MonoidSum

-- | The 'product' function computes the product of the numbers of a structure.
foldProd :: (MyFoldable t, Num a) => t a -> a
foldProd = getMonoidProd . foldMap MonoidProd

-- | The largest element of a non-empty structure.
foldLargest :: (MyFoldable t, Ord a) => t a -> a
foldLargest x = let Largest y = foldMap Largest x
                in y

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.
foldLargestBy :: MyFoldable t => (a -> a -> Ordering) -> t a -> a
foldLargestBy cmp x = let Largest y = foldRight max' MinusInfinity x where
                            max' a (Largest b) = case cmp a b of GT -> Largest a
                                                                 _ -> Largest b
                            max' a MinusInfinity = Largest a
                      in y

-- | The least element of a non-empty structure.
foldSmallest :: (MyFoldable t, Ord a) => t a -> a
foldSmallest x = let Smallest y = foldMap Smallest x
                 in y

-- | The least element of a non-empty structure with respect to the
-- given comparison function.
foldSmallestBy :: MyFoldable t => (a -> a -> Ordering) -> t a -> a
foldSmallestBy cmp x = let Smallest y = foldRight min' PlusInfinity x where
                             min' a (Smallest b) = case cmp a b of GT -> Smallest b
                                                                   _ -> Smallest a
                             min' a PlusInfinity = Smallest a
                       in y

-- | Does the element occur in the structure?
foldElem :: (MyFoldable t, Eq a) => a -> t a -> Bool
foldElem = foldAny . (==)

-- | 'notElem' is the negation of 'elem'.
foldNotElem :: (MyFoldable t, Eq a) => a -> t a -> Bool
foldNotElem x = not . foldElem x
