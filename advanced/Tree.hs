module Tree (
  Tree(Empty, Node)) where

import MyMonoid
import MyFoldable
import MyFunctor
import MyApplicativeFunctor


-- https://blog.cppcabrera.com/posts/functor-traverse-fold-tree.html

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)


instance MyFoldable Tree where
  foldMap f Empty        = monoidEmpty
  foldMap f (Node x l r) = foldMap f l `monoidAppend`
                           f x         `monoidAppend`
                           foldMap f r

instance MyFunctor Tree where
  funMap f Empty        = Empty
  funMap f (Node x l r) = Node (f x)
                               (funMap f l)
                               (funMap f r)

instance MyApplicativeFunctor Tree where
  pure x = Node x (pure x) (pure x)
  Empty `apply` _                       = Empty
  _ `apply` Empty                       = Empty
  (Node x ll lr) `apply` (Node y rl rr) = Node (x y)
                                               (ll `apply` rl)
                                               (lr `apply` rr)
