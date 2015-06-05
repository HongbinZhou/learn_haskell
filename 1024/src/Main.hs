{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Graphics.Vty.Widgets.All
import Graphics.Vty.Input.Events

import Data.Matrix as M
import System.Random
import qualified Data.Vector as V
import Data.List as L

import Control.Monad.Writer


logMatrix :: (MonadWriter [String] m) => M.Matrix Int -> m (M.Matrix Int)
logMatrix x = writer (x, ["Got matrix: " ++ show x])

logNumber :: (MonadWriter [String] m) => Int -> m Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: (MonadWriter [String] m) => m Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)

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

type Row = [Int]

squeeze :: Row -> Row
squeeze [] = []
squeeze (0:x) = squeeze x
squeeze [x] = [x]
squeeze (x:y:xs)
  | x==y = (x+y): squeeze xs
  | otherwise = x : squeeze (y:xs)

squeezeLeft :: Row -> Row
squeezeLeft = take maxSize . (++ repeat 0) . squeeze

squeezeRight :: Row -> Row
squeezeRight = reverse . squeezeLeft . reverse

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

tblMove :: RandomGen g =>
     g ->
     Widget a ->
     (g -> Matrix Int -> Either (Matrix Int) String) ->
     Matrix Int ->
     Matrix (Widget FormattedText) ->
     IO ()
tblMove gen tbl f m mat = do
       case f gen m of
        Left mm -> do
          setTextMatrix mat mm
          loopGame gen tbl mat mm
        Right s -> do
            putStrLn "Game over!"

loopGame :: RandomGen g =>
     g
     -> Widget a
     -> Matrix (Widget FormattedText)
     -> Matrix Int
     -> IO ()
loopGame gen tbl mat m  = do

  tbl `onKeyPressed` \_ k _ ->
    case k of
     KEsc -> shutdownUi >> return True
     KDown -> do
       tblMove gen tbl tblDown m mat
       return True
     KUp -> do
       tblMove gen tbl tblUp m mat
       return True
     KRight -> do
       tblMove gen tbl tblRight m mat
       return True
     KLeft -> do
       tblMove gen tbl tblLeft m mat
       return True
     _ -> return False


main :: IO ()
main = do
  let bdrStyle = take maxSize $ repeat (column ColAuto `pad` (padAll 2)
                                        `align` AlignCenter)

  tbl <- newTable bdrStyle BorderFull
  gen <- newStdGen

  let m = initMatrix gen
  mat <- textMatrix m
  matrixToTable tbl mat

  ui <- centered tbl

  fg <- newFocusGroup
  addToFocusGroup fg tbl

  coll <- newCollection
  addToCollection coll ui fg

  loopGame gen tbl mat m

  runUi coll defaultContext
