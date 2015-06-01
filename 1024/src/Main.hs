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

tblLeft :: Matrix Int -> Matrix Int
tblLeft = fromLists . growRow1' . map squeezeLeft . toLists

tblRight :: Matrix Int -> Matrix Int
tblRight = fromLists . growRow1 . map squeezeRight . toLists

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

tblDown :: Matrix Int -> Matrix Int
tblDown = M.transpose . tblRight . M.transpose

tblUp :: Matrix Int -> Matrix Int
tblUp = M.transpose . tblLeft . M.transpose

main :: IO ()
main = do
  tbl <- newTable [column ColAuto, column ColAuto] BorderFull
  n1 <- plainText "2"
  n2 <- plainText "4"
  n3 <- plainText " "
  n4 <- plainText " "
  addRow tbl $ n1 .|. n2
  addRow tbl $ n3 .|. n4

  ui <- centered tbl

  fg <- newFocusGroup
  addToFocusGroup fg tbl

  coll <- newCollection
  addToCollection coll ui fg

  fg `onKeyPressed` \_ k _ ->
    case k of
     KEsc -> shutdownUi >> return True
     KDown -> do
       setText n1 "2"
       setText n2 " "
       setText n3 "2"
       setText n4 "4"
       return True
     _ -> return False

  runUi coll defaultContext
