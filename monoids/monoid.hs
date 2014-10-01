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

example1 = MonoidSum 5 `monoidAppend` MonoidSum 6 `monoidAppend` MonoidSum 10
example2 = monoidConcat [MonoidSum 5, MonoidSum 6, MonoidSum 10]
example3 = getMonoidSum $ monoidConcat $ map MonoidSum [5, 6, 10]
example4 = monoidConcat ["5", "6", "10"]


-- | Monoid under multiplication.
newtype MonoidProd a = MonoidProd { getMonoidProd :: a } deriving (Show)

instance (Num a) => MyMonoid (MonoidProd a) where
  monoidEmpty = MonoidProd 1
  monoidAppend (MonoidProd x) (MonoidProd y) = MonoidProd (x * y)


instance (MyMonoid a) => MyMonoid (Maybe a) where
  monoidEmpty = Nothing
  monoidAppend Nothing x = x
  monoidAppend x Nothing = x
  monoidAppend (Just x) (Just y) = Just (monoidAppend x y)

example5 = Just (MonoidSum 5) `monoidAppend` Nothing `monoidAppend` Just (MonoidSum 6) `monoidAppend` Just (MonoidSum 10) `monoidAppend` Nothing


data Smallest a = Smallest a
                | PlusInfinity
                deriving (Show, Read, Eq, Ord)

instance (Ord a) => MyMonoid (Smallest a) where
  monoidEmpty = PlusInfinity
  monoidAppend = min

example6 = Smallest 5 `monoidAppend` PlusInfinity `monoidAppend` Smallest 6 `monoidAppend` Smallest 10 `monoidAppend` PlusInfinity


data Largest a = MinusInfinity
               | Largest a
               deriving (Show, Read, Eq, Ord)

instance (Ord a) => MyMonoid (Largest a) where
  monoidEmpty = MinusInfinity
  monoidAppend = max

example7 = Largest 5 `monoidAppend` MinusInfinity `monoidAppend` Largest 6 `monoidAppend` Largest 10 `monoidAppend` MinusInfinity


class MyFoldable t where
  -- | Combine the elements of a structure using a monoid.
  fold :: (MyMonoid m) => t m -> m
  fold = foldMap id

  -- | Map each element of the structure to a monoid,
  -- and combine the results.
  foldMap :: (MyMonoid m) => (a -> m) -> t a -> m
  foldMap f = foldr' (monoidAppend . f) monoidEmpty

  -- | Right-associative fold of a structure.
  --
  -- @'foldr' f z = 'Prelude.foldr' f z . 'toList'@
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldr' f z t = appMonoidEndo (foldMap (MonoidEndo . f) t) z


data Tree1 a = Empty
             | Leaf a
             | Node (Tree1 a) a (Tree1 a)
             deriving (Show, Read, Eq)

instance MyFoldable Tree1 where
  foldMap f Empty = monoidEmpty
  foldMap f (Leaf x) = f x
  foldMap f (Node l k r) = foldMap f l `monoidAppend` f k `monoidAppend` foldMap f r

tree1 = Node (Leaf 1) 7 (Leaf 2)
example8 = foldMap (MonoidAny . (== 1)) tree1
example9 = foldMap (MonoidAll . (> 5)) tree1
example10 = foldMap Largest tree1
example11 = foldMap (\x -> (Smallest x, Largest x)) tree1
example12 = foldMap (\x -> ((Smallest x, Largest x), MonoidSum x)) tree1
example13 = getMonoidSum $ foldMap (MonoidSum . (\x -> if x > 15 then 1 else 0)) tree1
tree2 = Node
          (Node
            (Node Empty 10 (Leaf 42))
            13
            (Node Empty 16 Empty))
          15
          (Node
            (Node Empty 18 Empty)
            19
            (Node Empty 10 Empty))
example8' = foldMap (MonoidAny . (== 1)) tree2
example9' = foldMap (MonoidAll . (> 5)) tree2
example10' = foldMap Largest tree2
example11' = foldMap (\x -> (Smallest x, Largest x)) tree2
example12' = foldMap (\x -> ((Smallest x, Largest x), MonoidSum x)) tree2
example13' = getMonoidSum $ foldMap (MonoidSum . (\x -> if x > 15 then 1 else 0)) tree2



{-
data Tree v a = Leaf v a
              | Branch v (Tree v a) (Tree v a)
              deriving (Show)

treeToList :: Tree v a -> [a]
treeToList (Leaf _ a)     = [a]
treeToList (Branch _ x y) = treeToList x ++ treeToList y

treeTag :: Tree v a -> v
treeTag (Leaf v _)     = v
treeTag (Branch v _ _) = v

treeHead :: Tree v a -> a
treeHead (Leaf _ a)     = a
treeHead (Branch _ x _) = treeHead x

type Size = Int
type Priority = Int
-}
