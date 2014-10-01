module Tree (
  Tree(Empty, Single, Node)
  ) where

import MyFunctor
import MyMonoid
import MyFoldable


data Tree a = Empty
            | Single a
            | Node a (Tree a) (Tree a)
            deriving (Show, Read, Eq)

instance MyFunctor Tree where
  funMap f Empty = Empty
  funMap f (Single x) = Single (f x)
  funMap f (Node x l r) = Node (f x) (funMap f l) (funMap f r)

instance MyFoldable Tree where
  foldMap f Empty = monoidEmpty
  foldMap f (Single x) = f x
  foldMap f (Node x l r) = foldMap f l `monoidAppend` f x `monoidAppend` foldMap f r
