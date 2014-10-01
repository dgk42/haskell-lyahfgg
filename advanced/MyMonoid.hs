module MyMonoid (
  MyMonoid(..),
  MonoidDual(..),
  MonoidEndo(..),
  MonoidAll(..),
  MonoidAny(..),
  MonoidSum(..),
  MonoidProd(..),
  Smallest(Smallest, PlusInfinity),
  Largest(Largest, MinusInfinity),
  MonoidFirst(..),
  MonoidLast(..)) where


class MyMonoid m where
  monoidEmpty :: m
  monoidAppend :: m -> m -> m
  -- bonus method!
  monoidConcat :: [m] -> m
  monoidConcat = foldr monoidAppend monoidEmpty
  {- alternatively (in terms of monoidConcat):
  monoidEmpty = monoidConcat []
  monoidAppend a b = monoidConcat [a, b]
  -}


instance (MyMonoid b) => MyMonoid (a -> b) where
  monoidEmpty _ = monoidEmpty
  monoidAppend f g x = f x `monoidAppend` g x


instance MyMonoid [a] where
  monoidEmpty = []
  monoidAppend = (++)


-- | The Product Monoid.
instance (MyMonoid a, MyMonoid b) => MyMonoid (a, b) where
  monoidEmpty = (monoidEmpty, monoidEmpty)
  (u, v) `monoidAppend` (w, x) = (u `monoidAppend` w, v `monoidAppend` x)


-- | The Dual Monoid (obtained by swapping the arguments of <>).
newtype MonoidDual a = MonoidDual { getMonoidDual :: a } deriving (Show)

instance (MyMonoid a) => MyMonoid (MonoidDual a) where
  monoidEmpty = MonoidDual monoidEmpty
  MonoidDual x `monoidAppend` MonoidDual y = MonoidDual (y `monoidAppend` x)


-- | The monoid of endomorphisms under composition.
newtype MonoidEndo a = MonoidEndo { appMonoidEndo :: a -> a }

instance MyMonoid (MonoidEndo a) where
  monoidEmpty = MonoidEndo id
  MonoidEndo f `monoidAppend` MonoidEndo g = MonoidEndo (f . g)


-- | Boolean monoid under conjunction.
newtype MonoidAll = MonoidAll { getMonoidAll :: Bool } deriving (Show)

instance MyMonoid MonoidAll where
  monoidEmpty = MonoidAll True
  MonoidAll x `monoidAppend` MonoidAll y = MonoidAll (x && y)


-- | Boolean monoid under disjunction.
newtype MonoidAny = MonoidAny { getMonoidAny :: Bool } deriving (Show)

instance MyMonoid MonoidAny where
  monoidEmpty = MonoidAny False
  MonoidAny x `monoidAppend` MonoidAny y = MonoidAny (x || y)


-- | Monoid under addition.
newtype MonoidSum a = MonoidSum { getMonoidSum :: a } deriving (Show)

instance (Num a) => MyMonoid (MonoidSum a) where
  monoidEmpty = MonoidSum 0
  MonoidSum x `monoidAppend` MonoidSum y = MonoidSum (x + y)


-- | Monoid under multiplication.
newtype MonoidProd a = MonoidProd { getMonoidProd :: a } deriving (Show)

instance (Num a) => MyMonoid (MonoidProd a) where
  monoidEmpty = MonoidProd 1
  monoidAppend (MonoidProd x) (MonoidProd y) = MonoidProd (x * y)


data Smallest a = Smallest a
                | PlusInfinity
                deriving (Show, Read, Eq, Ord)

instance (Ord a) => MyMonoid (Smallest a) where
  monoidEmpty = PlusInfinity
  monoidAppend = min


data Largest a = MinusInfinity
               | Largest a
               deriving (Show, Read, Eq, Ord)

instance (Ord a) => MyMonoid (Largest a) where
  monoidEmpty = MinusInfinity
  monoidAppend = max


-- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
-- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
-- turned into a monoid simply by adjoining an element @e@ not in @S@
-- and defining @e*e = e@ and @e*s = s = s*e@ for all @s âˆˆ S@.\" Since
-- there is no \"Semigroup\" typeclass providing just 'mappend', we
-- use 'Monoid' instead.
instance (MyMonoid a) => MyMonoid (Maybe a) where
  monoidEmpty = Nothing
  monoidAppend Nothing x = x
  monoidAppend x Nothing = x
  monoidAppend (Just x) (Just y) = Just (monoidAppend x y)


-- | Maybe monoid returning the leftmost non-Nothing value.
newtype MonoidFirst a = MonoidFirst { getMonoidFirst :: Maybe a } deriving (Eq, Ord, Read, Show)

instance MyMonoid (MonoidFirst a) where
  monoidEmpty = MonoidFirst Nothing
  r@(MonoidFirst (Just _)) `monoidAppend` _ = r
  MonoidFirst Nothing `monoidAppend` r = r


-- | Maybe monoid returning the rightmost non-Nothing value.
newtype MonoidLast a = MonoidLast { getMonoidLast :: Maybe a } deriving (Eq, Ord, Read, Show)

instance MyMonoid (MonoidLast a) where
  monoidEmpty = MonoidLast Nothing
  _ `monoidAppend` r@(MonoidLast (Just _)) = r
  r `monoidAppend` MonoidLast Nothing = r
