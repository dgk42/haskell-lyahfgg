lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER 7!"
lucky _ = "Tough luck sucker!"

factorial :: (Integral a) => a -> a
factorial n = product [1..n]

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Head of empty list"
head' (x : _) = x

tail' :: [a] -> [a]
tail' [] = error "Tail of empty list"
tail' (_ : xs) = xs

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
      | bmi <= skinny = "You're underweight, you emo, you!"
      | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
      | bmi <= fat    = "You're fat! Lose some weight, fatty!"
      | otherwise     = "You're a whale, congratulations!"
      where bmi    = weight / height ^ 2
            skinny = 18.5
            normal = 25.0
            fat    = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
   | a > b     = a
   | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ ". "
         where (f : _) = firstName
               (l : _) = lastName

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
         where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty."
                                               [x] -> "a singleton list."
                                               _   -> "a longer list."
