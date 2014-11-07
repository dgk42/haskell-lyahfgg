{-# LANGUAGE RankNTypes #-}

-- http://tonymorris.github.io/blog/posts/debut-with-a-catamorphism

-- Data.Maybe
newtype Option a = Option { cata :: forall x . (a -> x) -> x -> x }

-- Just
some :: a -> Option a
some a = Option (\s _ -> s a)

-- Nothing
none :: Option a
none = Option (const id)

-- fmap
map' :: (a -> b) -> Option a -> Option b
map' f m = cata m (some . f) none

-- (>>=)
flatMap :: (a -> Option b) -> Option a -> Option b
flatMap f m = cata m f none

-- fromMaybe
getOrElse :: Option a -> a -> a
getOrElse = flip cata id

filter :: Option a -> (a -> Bool) -> Option a
filter m p = cata m (\x -> if p x then some x else none) none

-- mapM_
foreach :: Option a -> (a -> IO ()) -> IO ()
foreach m f = cata m f (return ())

-- isJust
isDefined :: Option a -> Bool
isDefined m = cata m (const True) False

-- isNothing
isEmpty :: Option a -> Bool
isEmpty m = (not . isDefined) m

-- WARNING: not defined for none
-- fromJust
get :: Option a -> a
get m = cata m id (error "none.get")

-- mplus
orElse :: Option a -> Option a -> Option a
orElse m n = cata m (const m) n

toLeft :: Option a -> x -> Either a x
toLeft m x = cata m Left (Right x)

toRight :: Option a -> x -> Either x a
toRight m x = cata m Right (Left x)

-- maybeToList
toList :: Option a -> [a]
toList m = cata m (:[]) []
