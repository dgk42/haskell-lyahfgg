module MyFunctor (
  MyFunctor(..)) where

class MyFunctor f where
  funMap :: (a -> b) -> f a -> f b

instance MyFunctor Maybe where
  funMap f Nothing = Nothing
  funMap f (Just x) = Just (f x)

instance MyFunctor [] where
  -- fmap = map
  funMap f [] = []
  funMap f (x : xs) = f x : funMap f xs
