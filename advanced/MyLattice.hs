module MyLattice (
  MyLattice(..)) where


-- http://matt.might.net/articles/best-programming-languages

{-
A bounded lattice is a mathematical structure that has
a least element (bot), a greatest element (top),
a partially ordered less than relation (<:),
a join operation (join) and a meet operation (meet).
-}
class MyLattice a where
  top :: a
  bot :: a
  (<:) :: a -> a -> Bool
  join :: a -> a -> a
  meet :: a -> a -> a

{-
instance (Ord k, MyLattice a) => MyLattice (Map k a) where
  bot = Map.empty
  top = error $ "Cannot be represented."
  f <: g = Map.isSubmapOfBy (<:) f g
  f `join` g = Map.unionWith join f g
  f `meet` g = Map.intersectionWith meet f g
-}

instance (Eq a, MyLattice a, MyLattice b) => MyLattice (a, b) where
  bot = (bot, bot)
  top = (top, top)
  (a1, b1) <: (a2, b2) = (a1 <: a2) ||
                         (a1 == a2 && b1 <: b2)
  (a1, b1) `join` (a2, b2) = (a1 `join` a2, b1 `join` b2)
  (a1, b1) `meet` (a2, b2) = (a1 `meet` a2, b1 `meet` b2)
