applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

{-
ghci> applyTwice (+3) 10
16
ghci> applyTwice (++ " HAHA") "HEY"
"HEY HAHA HAHA"
ghci> applyTwice ("HAHA " ++) "HEY"
"HAHA HAHA HEY"
ghci> applyTwice (multThree 2 2) 9
144
ghci> applyTwice (3:) [1]
[3,3,1]
-}

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

{-
ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
[6,8,7,9]
ghci> zipWith' max [6,3,2,1] [7,3,1,5]
[7,3,2,5]
ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
["foo fighters","bar hoppers","baz aldrin"]
ghci> zipWith' (*) (replicate 5 2) [1..]
[2,4,6,8,10]
ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
[[3,4,6],[9,20,30],[10,12,12]]
-}

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
      | p x       = x : filter' p xs
      | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (> x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

largestDivisibleBy3829 :: (Integral a) => a
largestDivisibleBy3829 = head (filter p [100000, 99999..])
                         where p x = x `mod` 3829 == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x : xs)
         | p x       = x : takeWhile' p xs
         | otherwise = []

sumOfAllOddSquaresSmallerThan10000 :: Integer
sumOfAllOddSquaresSmallerThan10000 = sum (takeWhile' (< 10000) (filter odd (map (^2) [1..])))
-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)

chainsWithLengthGreaterThan15 :: Int
chainsWithLengthGreaterThan15 = length (filter (\xs -> length xs > 15) (map chain [1..100]))

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f acc [] = acc
foldl' f acc (x : xs) = foldl' f (f acc x) xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x (y : ys) = foldl' (\acc x -> if x == y then True else acc) False ys

foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' f acc [] = acc
foldr' f acc (x : xs) = f x (foldr' f acc xs)

map'' :: (a -> b) -> [a] -> [b]
map'' f (x : xs) = foldr' (\x acc -> f x : acc) [] xs

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f [x] = x
foldl1' f (x1 : x2 : xs) = foldl1' f (f x1 x2 : xs)

sum' :: (Num a) => [a] -> a
sum' = foldl1' (+)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [x] = x
foldr1' f (x : xs) = f x (foldr1' f xs)

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1' (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl' (flip' (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1' (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr' (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1' (\x _ -> x)

last' :: [a] -> a
last' = foldl1' (\_ x -> x)

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f acc [] = [acc]
scanl' f acc (x : xs) = acc : scanl' f (f acc x) xs

scanr' :: (b -> a -> a) -> a -> [b] -> [a]
scanr' f acc [] = [acc]
scanr' f acc (x : xs) = f x q : qs
                        where qs @ (q : _) = scanr' f acc xs

scanl1' :: (a -> a -> a) -> [a] -> [a]
scanl1' f (x : xs) = scanl' f x xs
scanl1' _ [] = error "scanl1': empty list"

scanr1' :: (a -> a -> a) -> [a] -> [a]
scanr1' f [x] = [x]
scanr1' f (x : xs) = f x q : qs
                     where qs @ (q : _) = scanr1' f xs
scanr1' _ [] = error "scanr1': empty list"

sqrtSums :: Int
sqrtSums = length (takeWhile' (<1000) (scanl1' (+) (map' sqrt [1..]))) + 1
