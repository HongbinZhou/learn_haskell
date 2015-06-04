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

initMatrix :: (RandomGen g) => g -> Matrix Int
initMatrix g = let (Left x) = tblLeft g (zero maxSize maxSize) in x

test_matrix :: Matrix Int
test_matrix = M.fromLists [[2,2,0,2],
                           [4,0,2,0],
                           [4,0,0,2],
                           [2,2,0,0]]

findAllHoleIdx :: [Int] -> Maybe [Int]
findAllHoleIdx l =
  case foldl (\acc (a, i) -> if a==0 then acc ++ [i] else acc ) [] (zip l [0..]) of
   [] -> Nothing
   x -> Just x

pickup :: (RandomGen g) => g -> [Int] -> Int
pickup g l = let (r, _) = randomR (0, (length l)-1) g
                 (_, x:_) = splitAt r l
             in x


fillOneHole :: (RandomGen g) => g -> [Int] -> Maybe [Int]
fillOneHole g l =
  case findAllHoleIdx l of
   Nothing -> Nothing
   Just idx -> let r = pickup g idx
                   (x, y:ys) = splitAt r l
               in Just $ x ++ [2] ++ ys

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

squeezeLeft :: [Int] -> [Int]
squeezeLeft = addTailZero . squeeze

squeezeRight :: [Int] -> [Int]
squeezeRight = addLeadingZero . reverse . squeeze . reverse

addTailZero :: [Int] -> [Int]
addTailZero = take maxSize . (++ repeat 0)

addLeadingZero :: [Int] -> [Int]
addLeadingZero = reverse . addTailZero . reverse

squeezeMatrix :: (RandomGen g) => g -> ([Int] -> [Int]) -> Matrix Int -> Either (Matrix Int) String
squeezeMatrix g f m =
  case fillOneHole g $ concat . map f . toLists $ m of
   Nothing -> Right "Matrix Full!"
   Just x -> Left $ fromList maxSize maxSize x

tblLeft :: (RandomGen g) => g -> Matrix Int -> Either (Matrix Int) String
tblLeft g = squeezeMatrix g squeezeLeft

tblRight :: (RandomGen g) => g -> Matrix Int -> Either (Matrix Int) String
tblRight g = squeezeMatrix g squeezeRight

tblUp :: (RandomGen g) => g -> Matrix Int -> Either (Matrix Int) String
tblUp g m =
  case tblLeft g . M.transpose $ m of
   Left m -> Left $ M.transpose m
   x -> x

tblDown :: (RandomGen g) => g -> Matrix Int -> Either (Matrix Int) String
tblDown g m =
  case tblRight g . M.transpose $ m of
   Left m -> Left $ M.transpose m
   x -> x

matrixToTable :: Widget Table -> Matrix (Widget FormattedText) -> IO ()
matrixToTable tbl m  = do
  sequence $ addRow tbl <$> M.toLists m
  return ()

textMatrix :: Matrix Int -> IO (Matrix (Widget FormattedText))
textMatrix = sequence . fmap (plainText . T.pack . show)

setTextMatrix :: Matrix (Widget FormattedText) -> Matrix Int -> IO ()
setTextMatrix a b = do
  sequence $ elementwise setText a ((T.pack . show) <$> b)
  return ()

loopGame tbl mat gen m  = do

  tbl `onKeyPressed` \_ k _ ->
    case k of
     KEsc -> shutdownUi >> return True
     KDown -> do
       case tblDown gen m of
        Left mm -> do
          setTextMatrix mat mm
          loopGame tbl mat gen mm
        Right s -> do
            putStrLn "Game over!"
       return True

     KUp -> do

       case tblUp gen m of
        Left mm -> do
          setTextMatrix mat mm
          loopGame tbl mat gen mm
        Right s -> do
            putStrLn "Game over!"
       return True
     KRight -> do

       case tblRight gen m of
        Left mm -> do
          setTextMatrix mat mm
          loopGame tbl mat gen mm
        Right s -> do
            putStrLn "Game over!"
       return True
     KLeft -> do

       case tblLeft gen m of
        Left mm -> do
          setTextMatrix mat mm
          loopGame tbl mat gen mm
        Right s -> do
            putStrLn "Game over!"

       return True
     _ -> return False


main :: IO ()
main = do
  tbl <- newTable [column ColAuto,
                   column ColAuto,
                   column ColAuto,
                   column ColAuto] BorderFull
  gen <- newStdGen

  let m = initMatrix gen
  mat <- textMatrix m
  matrixToTable tbl mat

  ui <- centered tbl

  fg <- newFocusGroup
  addToFocusGroup fg tbl

  coll <- newCollection
  addToCollection coll ui fg

  loopGame tbl mat gen m

  runUi coll defaultContext
