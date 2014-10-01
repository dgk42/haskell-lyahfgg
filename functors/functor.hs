class MyFunctor f where
  genericMap :: (a -> b) -> f a -> f b

instance MyFunctor Maybe where
  genericMap f Nothing = Nothing
  genericMap f (Just x) = Just (f x)

instance MyFunctor [] where
  -- fmap = map
  genericMap f [] = []
  genericMap f (x : xs) = f x : genericMap f xs

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving (Show)

{-
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)
-}

instance MyFunctor Tree where
  genericMap f (Leaf x) = Leaf (f x)
  genericMap f (Branch left right) = Branch (genericMap f left) (genericMap f right)

example1 = genericMap (2*) (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4)))
