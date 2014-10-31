module MyFunctor (
  MyFunctor(..),
  funVoid) where


{- | The 'Functor' class is used for types that can be mapped over.
Instances of 'Functor' should satisfy the following laws:

> fmap id  ==  id
> fmap (f . g)  ==  fmap f . fmap g

The instances of 'Functor' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
satisfy these laws.
-}

class MyFunctor f where
  -- <$>
  funMap :: (a -> b) -> f a -> f b


instance MyFunctor [] where
  -- fmap = map
  funMap = map


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


-- | @'void' value@ discards or ignores the result of evaluation, such as the return value of an 'IO' action.
funVoid :: MyFunctor f => f a -> f ()
funVoid = funMap (const ())
