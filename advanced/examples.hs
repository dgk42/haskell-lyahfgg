import MyFunctor
import MyMonoid
import MyApplicativeFunctor
import MyFoldable
import MyMonad
import MySet
import Tree1
import Tree


example0 = funMap (2*) (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4)))


example10 = MonoidSum 5 `monoidAppend` MonoidSum 6 `monoidAppend` MonoidSum 10
example11 = monoidConcat [MonoidSum 5, MonoidSum 6, MonoidSum 10]
example12 = getMonoidSum $ monoidConcat $ map MonoidSum [5, 6, 10]
example13 = monoidConcat ["5", "6", "10"]

example14 = Smallest 5 `monoidAppend` PlusInfinity `monoidAppend` Smallest 6 `monoidAppend` Smallest 10 `monoidAppend` PlusInfinity

example15 = Largest 5 `monoidAppend` MinusInfinity `monoidAppend` Largest 6 `monoidAppend` Largest 10 `monoidAppend` MinusInfinity

example16 = Just (MonoidSum 5) `monoidAppend` Nothing `monoidAppend` Just (MonoidSum 6) `monoidAppend` Just (MonoidSum 10) `monoidAppend` Nothing


countIfGreaterThan15 x = if x > 15 then 1 else 0

tree1 = Node 7
             (Single 1)
             (Single 2)
example20 = foldMap (MonoidAny . (== 1)) tree1
example21 = foldMap (MonoidAll . (> 5)) tree1
example22 = foldMap Largest tree1
example23 = foldMap (\x -> (Smallest x, Largest x)) tree1
example24 = foldMap (\x -> ((Smallest x, Largest x), MonoidSum x)) tree1
example25 = getMonoidSum $ foldMap (MonoidSum . countIfGreaterThan15) tree1
example26 = getMonoidSum $ foldMap (MonoidSum . (const 1)) tree1
example27 = foldFindFirst (== 42) tree1

tree2 = Node 15
             (Node 13
                   (Node 10
                         Empty
                         (Single 42))
                   (Node 16
                         Empty
                         Empty))
             (Node 19
                   (Node 18
                         Empty
                         Empty)
                   (Node 10
                         Empty
                         Empty))
example20' = foldMap (MonoidAny . (== 1)) tree2
example21' = foldMap (MonoidAll . (> 5)) tree2
example22' = foldMap Largest tree2
example23' = foldMap (\x -> (Smallest x, Largest x)) tree2
example24' = foldMap (\x -> ((Smallest x, Largest x), MonoidSum x)) tree2
example25' = getMonoidSum $ foldMap (MonoidSum . countIfGreaterThan15) tree2
example26' = getMonoidSum $ foldMap (MonoidSum . (const 1)) tree2
example27' = foldFindFirst (== 42) tree2

tree3 :: Tree (String, Int)
tree3 = Node ("a", 5)
             (Node ("b", 1)
                   (Single ("d", 9))
                   (Single ("e", 7)))
             (Node ("c", 3)
                   (Single ("f", 10))
                   (Node ("g", 2)
                         (Node ("h", 4)
                               (Node ("i", 8)
                                     (Single ("k", 69105))
                                     Empty)
                               (Node ("j", 42)
                                     Empty
                                     (Node ("l", 69)
                                           Empty
                                           (Single ("m", 0)))))
                         Empty))

newtype TreeContent = TreeContent (String, Int) deriving (Show)

instance Eq TreeContent where
  TreeContent (s1, i1) == TreeContent (s2, i2) = i1 == i2

instance Ord TreeContent where
  TreeContent (s1, i1) `compare` TreeContent (s2, i2) = i1 `compare` i2

example30 = foldFindFirst (\x -> snd x == 69105) tree3
example31 = foldMap (Largest . TreeContent) tree3
example32 = foldLargest tree3
example33 = foldLargestBy (\x y -> fst y `compare` fst x) tree3
example34 = foldSmallestBy (\x y -> fst y `compare` fst x) tree3


fx :: Int -> Int -> Int -> Int
fx x y z = 4 * x + 2 * y + z

example40 = fx `funMap` (Just 5) `apply` (Just 5) `apply` (Just 5)


type Person = String

father :: Person -> Maybe Person
father "Dio" = Just "George"
father "Nick" = Just "George"
father "George" = Just "Nionios"
father "Elpis" = Just "Nicolas"
father _ = Nothing

mother :: Person -> Maybe Person
mother "Dio" = Just "Elpis"
mother "Nick" = Just "Elpis"
mother "George" = Just "Maria"
mother "Elpis" = Just "Eftychia"
mother _ = Nothing

paternalGrandfather :: Person -> Maybe Person
paternalGrandfather p = father p `monadBind` father

maternalGrandfather :: Person -> Maybe Person
maternalGrandfather p = mother p `monadBind` father

bothGrandfathers :: Person -> Maybe (Person, Person)
bothGrandfathers p =
  father p `monadBind`
    (\dad -> father dad `monadBind`
      (\gfdad -> mother p `monadBind`
        (\mom -> father mom `monadBind`
          (\gfmom -> monadReturn (gfdad, gfmom)))))

bothGrandmothers :: Person -> Maybe (Person, Person)
bothGrandmothers p =
  father p `monadBind`
    (\dad -> mother dad `monadBind`
      (\gmdad -> mother p `monadBind`
        (\mom -> mother mom `monadBind`
          (\gmmom -> monadReturn (gmdad, gmmom)))))


example50 = monadReturn (+) `monadAp` [10, 11, 12] `monadAp` [100, 200]
example51 = monadReturn (+) `monadAp` Just 42 `monadAp` Just 100
example52 = monadReturn (+) `monadAp` Nothing `monadAp` Just 100
example53 = monadReturn (+) `monadAp` Just 42 `monadAp` Nothing

example60 = emptySet []
example61 = ((addToSet emptySet 1) 1)
example62 = ((addToSet emptySet 1) 2)
example63 = setContains emptySet 1
example64 = setContains (addToSet emptySet 1) 1
example65 = setContains (addToSet emptySet 1) 2
example66 = setContains (singletonSet 1) 1
example67 = setContains (singletonSet 1) 3
example68 = (setUnion (singletonSet 1) (singletonSet 2)) 1
example69 = (setUnion (singletonSet 1) (singletonSet 2)) 2
example70 = (setUnion (singletonSet 1) (singletonSet 2)) 3
example71 = (setIntersection (setUnion (singletonSet 1) (singletonSet 2)) (singletonSet 1)) 2
example72 = (setIntersection (setUnion (singletonSet 1) (singletonSet 2)) (singletonSet 1)) 1
example73 = (setDifference (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3))) 1
example74 = (setDifference (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3))) 2
example75 = (setDifference (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3))) 3
example76 = (filterSet (== 2) (setUnion (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3)))) 3
example77 = (filterSet (== 3) (setUnion (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3)))) 3
example78 = (filterSet (>= 1) (setUnion (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3)))) 3
example79 = (filterSet (>= 1) (setUnion (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3)))) 1
example80 = (filterSet (>= 1) (setUnion (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3)))) 2
example81 = (filterSet (>= 1) (setUnion (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3)))) 10
example82 = (removeFromSet 1 (setUnion (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3)))) 1
example83 = (removeFromSet 1 (setUnion (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3)))) 2
example84 = (removeFromSet 1 (setUnion (setUnion (singletonSet 1) (singletonSet 2)) (setUnion (singletonSet 1) (singletonSet 3)))) 3
example85 = map (toSet [1, 2, 3]) [0..4]
example86 = setExists (toSet [1..3]) (== 1) [-100..100]
example87 = setExists (toSet [1..3]) (== 0) [-100..100]
example88 = setForAll (toSet [1..3]) (> 0) [-100..100]
example89 = setForAll (toSet [1..3]) (> 0) [-100..100]
example90 = setForAll (toSet [1..3]) (> 0) [1, 2]
