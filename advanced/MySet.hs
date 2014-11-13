module MySet (
  MySet,
  addToSet,
  emptySet,
  filterSet,
  fromSet,
  removeFromSet,
  setContains,
  setDifference,
  setExists,
  setForAll,
  setIntersection,
  setUnion,
  singletonSet,
  toSet) where


-- http://adumont.fr/blog/fun-with-sets

type MySet a = a -> Bool

emptySet :: MySet a
emptySet = \_ -> False

-- Given an already existing set and a new entry, return a new set.
addToSet :: Eq a => MySet a -> a -> MySet a
addToSet s e = \i -> (e == i) || setContains s i

-- Is this parameter in the set?
setContains :: MySet a -> a -> Bool
setContains s e = s e

-- Simple set creation given an entry.
singletonSet :: Eq a => a -> MySet a
singletonSet a = \e -> (a == e)

-- The standard union set definition – all which is a or b (or both).
setUnion :: MySet a -> MySet a -> MySet a
setUnion a b = \e -> a e || b e

-- The standard intersection definition – all which is in a and b.
setIntersection :: MySet a -> MySet a -> MySet a
setIntersection a b = \e -> a e && b e

-- The standard difference definition set – all which is in a but not in b.
setDifference :: MySet a -> MySet a -> MySet a
setDifference a b = \e -> a e && not (b e)

-- Given a predicate and a set,
-- return a set that satisfies both the predicate and the existing set.
-- This reminds me of something... Indeed, if we see the predicate as a set,
-- the filter is then simply the intersect function.
filterSet :: (a -> Bool) -> MySet a -> MySet a
filterSet = setIntersection

-- Given an entry and a set, remove the entry from the set.
removeFromSet :: Eq a => a -> MySet a -> MySet a
removeFromSet e s = \i -> (setDifference s (singletonSet e)) i

-- Utility function to create set from a list.
toSet :: Eq a => [a] -> MySet a
toSet []     = emptySet
toSet (x:xs) = addToSet (toSet xs) x

fromSet :: MySet a -> [a] -> [a]
fromSet s xs = filter (setContains s) xs

-- Is there any element in the set that satisfies the predicate?
setExists :: MySet a -> (a -> Bool) -> [a] -> Bool
setExists s p xs = or $ map (setIntersection s p) xs

-- Checks if all elements in the set satisfy the predicate.
setForAll :: MySet a -> (a -> Bool) -> [a] -> Bool
setForAll s p xs = and $ map (setIntersection s p) xs
