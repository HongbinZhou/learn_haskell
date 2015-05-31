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

-- squeezeRight :: [Int] -> [Int]
-- squeezeRight = addLeadingZero . dropTailZero . map sum . group . (L.filter (/=0))

-- dropTailZero :: [Int] -> [Int]
-- dropTailZero = reverse . dropWhile  (==0).  reverse

squeeze :: [Int] -> [Int]
squeeze = map sum . group . (L.filter (/=0))

squeezeLeft :: [Int] -> [Int]
squeezeLeft = addTailZero . squeeze

squeezeRight :: [Int] -> [Int]
squeezeRight = addLeadingZero . squeeze


addTailZero :: [Int] -> [Int]
addTailZero x = take maxSize $ x ++ (repeat 0)

addLeadingZero :: [Int] -> [Int]
addLeadingZero x = (take (maxSize - (length x) ) $ repeat 0) ++ x

tblLeft :: Matrix Int -> Matrix Int
tblLeft = fromLists . map squeezeLeft . toLists

tblRight :: Matrix Int -> Matrix Int
tblRight = fromLists . map squeezeRight . toLists

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
