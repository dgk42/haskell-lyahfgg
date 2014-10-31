import MyExpFunctor

veryOdd :: Predicate Integer
veryOdd = contraMap (`div` 2) (Predicate odd)

main :: IO ()
main = print $ map (getPredicate veryOdd) [0..11]
