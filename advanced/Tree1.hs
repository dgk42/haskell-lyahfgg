module Tree1 (
  Tree1(Leaf, Branch)
  ) where

import MyFunctor


data Tree1 a = Leaf a
             | Branch (Tree1 a) (Tree1 a)
             deriving (Show)

{-
treeMap :: (a -> b) -> Tree1 a -> Tree1 b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)
-}

instance MyFunctor Tree1 where
  funMap f (Leaf x) = Leaf (f x)
  funMap f (Branch left right) = Branch (funMap f left) (funMap f right)
