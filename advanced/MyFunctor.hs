module MyFunctor (
  MyFunctor(..)) where


class MyFunctor f where
  funMap :: (a -> b) -> f a -> f b

instance MyFunctor [] where
  -- fmap = map
  funMap f [] = []
  funMap f (x : xs) = f x : funMap f xs

instance MyFunctor Maybe where
  funMap f Nothing = Nothing
  funMap f (Just x) = Just (f x)

instance MyFunctor (Either e) where
  funMap f (Left l)  = Left l
  funMap f (Right r) = Right (f r)

instance MyFunctor ((->) t) where
  funMap f g = f . g

instance MyFunctor ((,) a) where
  funMap f (x, y) = (x, f y)
