{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Graphics.Vty.Widgets.All
import Graphics.Vty.Input.Events

import Data.Matrix as M
import System.Random
import qualified Data.Vector as V
import Data.List as L


maxSize = 4 :: Int

initRow :: Int -> Matrix Int
initRow x
  | x<1 = h 1
  | x > maxSize = h maxSize
  | otherwise = h x
  where h x = let z = zero 1 maxSize in
               setElem 2 (1, x) z

initTable :: Matrix Int
initTable = (initRow 1) <-> zero (maxSize-1) maxSize

test_matrix :: Matrix Int
test_matrix = M.fromLists [[2,2,0,2],
                           [4,0,2,0],
                           [4,0,0,2],
                           [2,2,0,0]]

findAllHole :: [Int] -> [(Int,Int)]
findAllHole l = filter (\(a, _) -> a==0) $ zip l [0..]

findAllHoleIdx :: [Int] -> [Int]
findAllHoleIdx l = foldl (\acc (a, b) -> acc ++ [b]) [] $ findAllHole l

pickup :: (RandomGen g) => g -> [Int] -> Int
pickup g l = let (r, _) = randomR (0, (length l)-1) g
                 (_, x:_) = splitAt r l
             in x

fillOneHole :: (RandomGen g) => g -> [Int] -> [Int]
fillOneHole g l = let r = pickup g $ findAllHoleIdx l
                      (x, y:ys) = splitAt r l
                  in x ++ [2] ++ ys

group2by2 :: [Int] -> [[Int]]
group2by2 [] = [[]]
group2by2 [x] = [[x]]
group2by2 (x:y:xs) = [x,y]:group2by2 xs

group' :: [Int] -> [[Int]]
group' [] = [[]]
group' [x] = [[x]]
group' s@(x:y:xs)
  | x==y = group2by2 s
  | otherwise = [x] : group' (y:xs)

squeeze :: [Int] -> [Int]
squeeze = map sum . group' . (L.filter (/=0))

squeeze' :: [Int] -> [Int]
squeeze' = map sum . group . (L.filter (/=0))

squeezeLeft :: [Int] -> [Int]
squeezeLeft = addTailZero . squeeze

squeezeRight :: [Int] -> [Int]
squeezeRight = addLeadingZero . reverse . squeeze . reverse

addTailZero :: [Int] -> [Int]
addTailZero = take maxSize . (++ repeat 0)

addLeadingZero :: [Int] -> [Int]
addLeadingZero = reverse . addTailZero . reverse

growRow1' :: [[Int]] -> [[Int]]
growRow1' (x:xs) = grow' x : xs

growRow1 :: [[Int]] -> [[Int]]
growRow1 (x:xs) = grow x : xs

grow' :: [Int] -> [Int]
grow' = reverse . grow . reverse

grow :: [Int] -> [Int]
grow [] = []
grow (0:xs) = 2:xs
grow (x:xs) = x:(grow xs)

squeezeMatrix :: (RandomGen g) => g -> ([Int] -> [Int]) -> Matrix Int -> Matrix Int
squeezeMatrix g f = fromList maxSize maxSize . fillOneHole g . concat . map f . toLists

tblLeft :: (RandomGen g) => g -> Matrix Int -> Matrix Int
tblLeft g = squeezeMatrix g squeezeLeft

tblRight :: (RandomGen g) => g -> Matrix Int -> Matrix Int
tblRight g = squeezeMatrix g squeezeRight

tblDown :: (RandomGen g) => g -> Matrix Int -> Matrix Int
tblDown g = M.transpose . tblRight g . M.transpose

tblUp :: (RandomGen g) => g -> Matrix Int -> Matrix Int
tblUp g = M.transpose . tblLeft g . M.transpose

test_tbl:: (StdGen -> Matrix Int -> Matrix Int) -> IO (Matrix Int)
test_tbl f = do
  g <- newStdGen
  return $ f g test_matrix

test_tbl_all :: IO [Matrix Int]
test_tbl_all = sequence $ map (test_tbl) [tblLeft, tblRight, tblUp, tblDown]

showMatrix :: Matrix (Widget FormattedText) -> Widget Table -> IO ()
showMatrix m tbl = do
  let [row1, row2, row3, row4] = M.toLists m
  addRow tbl row1
  addRow tbl row2
  addRow tbl row3
  addRow tbl row4

textMatrix :: Matrix Int -> IO (Matrix (Widget FormattedText))
textMatrix = sequence . fmap (plainText . T.pack . show)

setTextMatrix :: Matrix (Widget FormattedText) -> Matrix Int -> IO ()
setTextMatrix a b = do
  sequence $ elementwise setText a ((T.pack . show) <$> b)
  return ()

main :: IO ()
main = do
  tbl <- newTable [column ColAuto,
                   column ColAuto,
                   column ColAuto,
                   column ColAuto] BorderFull
  mat <- textMatrix test_matrix
  showMatrix mat tbl

  ui <- centered tbl

  fg <- newFocusGroup
  addToFocusGroup fg tbl

  coll <- newCollection
  addToCollection coll ui fg

  gen <- newStdGen
  fg `onKeyPressed` \_ k _ ->
    case k of
     KEsc -> shutdownUi >> return True
     KDown -> do
       setTextMatrix mat (tblDown gen test_matrix)
       return True
     KUp -> do
       setTextMatrix mat (tblUp gen test_matrix)
       return True

     KRight -> do
       setTextMatrix mat (tblRight gen test_matrix)
       return True

     KLeft -> do
       setTextMatrix mat (tblLeft gen test_matrix)
       return True

     _ -> return False

  runUi coll defaultContext
