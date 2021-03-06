import Test.QuickCheck
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = left ++ [x] ++ right
  where left  = quicksort [n | n <- xs, n <= x]
        right = quicksort [n | n <- xs, n > x]

test_quicksort = quicksort [1,2,3,8,6,4,2]
test_quicksort'= quicksort "aasdfijwe"

prop_qs :: [Int] -> Bool
prop_qs x = quicksort x == quicksort ( quicksort x)

main :: IO ()
main = quickCheck prop_qs
