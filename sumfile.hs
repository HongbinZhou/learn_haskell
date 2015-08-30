
-- | Write a list of numbers [1..n] to file
genNumFile :: Int -> IO ()
genNumFile n = do
  writeFile "numbers.txt" (numList2Str [1..n])

-- | [1..3] will be "1 2 3"
numList2Str :: [Int] -> String
numList2Str  = foldr (\x acc -> show x ++ " " ++ acc) ""

-- | build: ghc --make sumfile.hs
-- | run: cat numbers.txt | ./sumfile
main :: IO ()
main = do
  contents <- getContents
  print (sumFile contents)
    where sumFile = sum . map read . words
