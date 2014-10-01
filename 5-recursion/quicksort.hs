-- QuickSort:
--   A sorted empty list is an empty list.
--   Main algorithm:
--     A sorted list is a list that has
--     all the values smaller than (or equal to) the head of the list in front (and those values are sorted),
--     then comes the head of the list in the middle and
--     then come all the values that are bigger than the head (they 're also sorted).
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted
