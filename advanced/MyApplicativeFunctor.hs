module MyApplicativeFunctor (
  MyApplicativeFunctor(..),
  MyZipList(..)) where

import MyMonoid
import MyFunctor


-- | A functor with application, providing operations to
--
-- * embed pure expressions ('pure'), and
--
-- * sequence computations and combine their results ('<*>').
--
-- A minimal complete definition must include implementations of these
-- functions satisfying the following laws:
--
-- [/identity/]
--
--      @'pure' 'id' '<*>' v = v@
--
-- [/composition/]
--
--      @'pure' (.) '<*>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)@
--
-- [/homomorphism/]
--
--      @'pure' f '<*>' 'pure' x = 'pure' (f x)@
--
-- [/interchange/]
--
--      @u '<*>' 'pure' y = 'pure' ('$' y) '<*>' u@
--
-- The other methods have the following default definitions, which may
-- be overridden with equivalent specialized implementations:
--
--   * @u '*>' v = 'pure' ('const' 'id') '<*>' u '<*>' v@
--
--   * @u '<*' v = 'pure' 'const' '<*>' u '<*>' v@
--
-- As a consequence of these laws, the 'Functor' instance for @f@ will satisfy
--
--   * @'fmap' f x = 'pure' f '<*>' x@
--
-- If @f@ is also a 'Monad', it should satisfy
--
--   * @'pure' = 'return'@
--
--   * @('<*>') = 'ap'@
--
-- (which implies that 'pure' and '<*>' satisfy the applicative functor laws).

class (MyFunctor f) => MyApplicativeFunctor f where
  -- | Lift a value.
  pure :: a -> f a

  -- | Sequential application.
  -- <*>
  apply :: f (a -> b) -> f a -> f b


instance MyApplicativeFunctor [] where
  pure x = [x]
  fs `apply` xs = [f x | f <- fs, x <- xs]


instance MyApplicativeFunctor Maybe where
  pure = Just
  Just f `apply` Just x = Just (f x)
  _ `apply` _           = Nothing


instance MyApplicativeFunctor (Either e) where
  pure = Right
  Right f `apply` Right x = Right (f x)
  Left f `apply` _        = Left f
  _ `apply` Left x        = Left x


instance MyApplicativeFunctor ((->) t) where
  pure = const
  f `apply` g = \t -> f t (g t)


instance MyMonoid a => MyApplicativeFunctor ((,) a) where
  pure x = (monoidEmpty, x)
  (u, f) `apply` (v, x) = (u `monoidAppend` v, f x)


-- | Lists, but with an 'Applicative' functor based on zipping, so that
--
-- @f '<$>' 'ZipList' xs1 '<*>' ... '<*>' 'ZipList' xsn = 'ZipList' (zipWithn f xs1 ... xsn)@
--
newtype MyZipList a = MyZipList [a]

instance MyFunctor MyZipList where
  funMap f (MyZipList xs) = MyZipList (map f xs)

instance MyApplicativeFunctor MyZipList where
  pure x = MyZipList (repeat x)
  (MyZipList fs) `apply` (MyZipList xs) = MyZipList (zipWith ($) fs xs)
