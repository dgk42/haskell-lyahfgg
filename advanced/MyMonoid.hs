module MyMonoid (
  Largest(Largest, MinusInfinity),
  MonoidAll(..),
  MonoidAny(..),
  MonoidDual(..),
  MonoidEndo(..),
  MonoidFirst(..),
  MonoidLast(..),
  MonoidProd(..),
  MonoidSum(..),
  MyMonoid(..),
  Smallest(Smallest, PlusInfinity)) where


-- ---------------------------------------------------------------------------
-- | The class of monoids (types with an associative binary operation that
-- has an identity).  Instances should satisfy the following laws:
--
--  * @mappend mempty x = x@
--
--  * @mappend x mempty = x@
--
--  * @mappend x (mappend y z) = mappend (mappend x y) z@
--
--  * @mconcat = 'foldr' mappend mempty@
--
-- The method names refer to the monoid of lists under concatenation,
-- but there are many other instances.
--
-- Minimal complete definition: 'mempty' and 'mappend'.
--
-- Some types can be viewed as a monoid in more than one way,
-- e.g. both addition and multiplication on numbers.
-- In such cases we often define @newtype@s and make those instances
-- of 'Monoid', e.g. 'Sum' and 'Product'.

class MyMonoid m where
  -- ^ Identity of 'mappend'
  monoidEmpty :: m
  -- ^ An associative operation
  monoidAppend :: m -> m -> m

  -- bonus method!
  -- ^ Fold a list using the monoid.
  -- For most types, the default definition for 'mconcat' will be
  -- used, but the function is included in the class definition so
  -- that an optimized version can be provided for specific types.
  monoidConcat :: [m] -> m
  monoidConcat = foldr monoidAppend monoidEmpty

  {- alternatively (in terms of monoidConcat):
  monoidEmpty = monoidConcat []
  monoidAppend a b = monoidConcat [a, b]
  -}


instance MyMonoid b => MyMonoid (a -> b) where
  monoidEmpty _ = monoidEmpty
  monoidAppend f g x = f x `monoidAppend` g x


instance MyMonoid [a] where
  monoidEmpty = []
  monoidAppend = (++)


-- | The Ordering Monoid.
instance MyMonoid Ordering where
  monoidEmpty = EQ
  LT `monoidAppend` _ = LT
  EQ `monoidAppend` y = y
  GT `monoidAppend` _ = GT


-- | The Product Monoid.
instance (MyMonoid a, MyMonoid b) => MyMonoid (a, b) where
  monoidEmpty = (monoidEmpty, monoidEmpty)
  (u, v) `monoidAppend` (w, x) = (u `monoidAppend` w, v `monoidAppend` x)


-- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
-- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
-- turned into a monoid simply by adjoining an element @e@ not in @S@
-- and defining @e*e = e@ and @e*s = s = s*e@ for all @s âˆˆ S@.\" Since
-- there is no \"Semigroup\" typeclass providing just 'mappend', we
-- use 'Monoid' instead.
instance MyMonoid a => MyMonoid (Maybe a) where
  monoidEmpty = Nothing
  Nothing `monoidAppend` x     = x
  x `monoidAppend` Nothing     = x
  Just x `monoidAppend` Just y = Just (monoidAppend x y)


-- | Maybe monoid returning the leftmost non-Nothing value.
newtype MonoidFirst a = MonoidFirst { getMonoidFirst :: Maybe a } deriving (Show, Read)

instance MyMonoid (MonoidFirst a) where
  monoidEmpty = MonoidFirst Nothing
  r@(MonoidFirst (Just _)) `monoidAppend` _ = r
  MonoidFirst Nothing `monoidAppend` r      = r


-- | Maybe monoid returning the rightmost non-Nothing value.
newtype MonoidLast a = MonoidLast { getMonoidLast :: Maybe a } deriving (Show, Read)

instance MyMonoid (MonoidLast a) where
  monoidEmpty = MonoidLast Nothing
  _ `monoidAppend` r@(MonoidLast (Just _)) = r
  r `monoidAppend` MonoidLast Nothing      = r


-- | The Dual Monoid (obtained by swapping the arguments of <>).
newtype MonoidDual a = MonoidDual { getMonoidDual :: a } deriving (Show, Read)

instance MyMonoid a => MyMonoid (MonoidDual a) where
  monoidEmpty = MonoidDual monoidEmpty
  MonoidDual x `monoidAppend` MonoidDual y = MonoidDual (y `monoidAppend` x)


-- | The monoid of endomorphisms under composition.
newtype MonoidEndo a = MonoidEndo { appMonoidEndo :: a -> a }

instance MyMonoid (MonoidEndo a) where
  monoidEmpty = MonoidEndo id
  MonoidEndo f `monoidAppend` MonoidEndo g = MonoidEndo (f . g)


-- | Boolean monoid under conjunction.
newtype MonoidAll = MonoidAll { getMonoidAll :: Bool } deriving (Show, Read)

instance MyMonoid MonoidAll where
  monoidEmpty = MonoidAll True
  MonoidAll x `monoidAppend` MonoidAll y = MonoidAll (x && y)


-- | Boolean monoid under disjunction.
newtype MonoidAny = MonoidAny { getMonoidAny :: Bool } deriving (Show, Read)

instance MyMonoid MonoidAny where
  monoidEmpty = MonoidAny False
  MonoidAny x `monoidAppend` MonoidAny y = MonoidAny (x || y)


-- | Monoid under addition.
newtype MonoidSum a = MonoidSum { getMonoidSum :: a } deriving (Show, Read)

instance Num a => MyMonoid (MonoidSum a) where
  monoidEmpty = MonoidSum 0
  MonoidSum x `monoidAppend` MonoidSum y = MonoidSum (x + y)


-- | Monoid under multiplication.
newtype MonoidProd a = MonoidProd { getMonoidProd :: a } deriving (Show, Read)

instance Num a => MyMonoid (MonoidProd a) where
  monoidEmpty = MonoidProd 1
  monoidAppend (MonoidProd x) (MonoidProd y) = MonoidProd (x * y)


-- | Monoid under min.
data Smallest a = Smallest a
                | PlusInfinity deriving (Show, Read, Eq, Ord)

instance Ord a => MyMonoid (Smallest a) where
  monoidEmpty = PlusInfinity
  monoidAppend = min


-- | Monoid under max.
data Largest a = MinusInfinity
               | Largest a deriving (Show, Read, Eq, Ord)

instance Ord a => MyMonoid (Largest a) where
  monoidEmpty = MinusInfinity
  monoidAppend = max
