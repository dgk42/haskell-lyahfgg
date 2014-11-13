module Tree1 (
  Tree1(Leaf, Branch)) where

import MyMonoid
import MyFoldable
import MyFunctor
import MyApplicativeFunctor
import MyMonad


data Tree1 a = Leaf a
             | Branch (Tree1 a) (Tree1 a) deriving (Show, Read, Eq)


instance MyFoldable Tree1 where
  foldMap f (Leaf x)     = f x
  foldMap f (Branch l r) = foldMap f l `monoidAppend`
                           foldMap f r

{-
treeMap :: (a -> b) -> Tree1 a -> Tree1 b
treeMap f (Leaf x)            = Leaf (f x)
treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)
-}

instance MyFunctor Tree1 where
  funMap f (Leaf x)            = Leaf (f x)
  funMap f (Branch left right) = Branch (funMap f left)
                                        (funMap f right)

instance MyMonad Tree1 where
  monadReturn = Leaf
  Leaf a `monadBind` f     = f a
  Branch l r `monadBind` f = Branch (l `monadBind` f)
                                    (r `monadBind` f)

instance MyApplicativeFunctor Tree1 where
  pure = monadReturn
  tf `apply` tx = tf          `monadBind` (\f ->
  	          tx          `monadBind` (\x ->
  	          monadReturn (f x)))
