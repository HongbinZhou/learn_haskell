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
initMatrix g =  fillOneHole g (zero maxSize maxSize)

test_matrix :: Matrix Int
test_matrix = M.fromLists [[2,2,0,2],
                           [4,0,2,0],
                           [4,0,0,2],
                           [2,2,0,0]]

pickup :: (RandomGen g) => g -> [Int] -> Int
pickup g l = let (r, _) = randomR (0, (length l)-1) g
                 (_, x:_) = splitAt r l
             in x

fillOneHole ::
  (RandomGen g) =>
  g ->
  Matrix Int ->
  Matrix Int
fillOneHole g =
  M.fromList maxSize maxSize . randFill . M.toList
  where randFill :: [Int] -> [Int]
        randFill l = case findAllHoleIdx l of
                      [] -> l
                      idx -> let r = pickup g idx
                                 (x,y:ys) = splitAt r l
                             in x ++ [2] ++ ys

        findAllHoleIdx :: [Int] -> [Int]
        findAllHoleIdx l =
          foldl (\acc (a, i) -> if a==0 then acc ++ [i] else acc ) [] (zip l [0..])


type Row = [Int]

-- squeeze Row to left
-- eg: [2,2,4,0] -> [4,4]
--     [2,2,2,4] -> [4,2,4]
squeeze :: Row -> Row
squeeze [] = []
squeeze (0:x) = squeeze x
squeeze [x] = [x]
squeeze (x:y:xs)
  | x==y = (x+y): squeeze xs
  | otherwise = x : squeeze (y:xs)

data Direction =
  DUp
  | DDown
  | DLeft
  | DRight
  deriving(Eq, Enum)

squeezeMatrix ::
  Direction
  -> Matrix Int
  -> Matrix Int
squeezeMatrix DLeft
  = M.fromLists . map squeezeLeft . M.toLists
  where squeezeLeft :: Row -> Row
        squeezeLeft = take maxSize . (++ repeat 0) . squeeze
squeezeMatrix DRight =
  mirrorLR . squeezeMatrix DLeft . mirrorLR
squeezeMatrix DDown =
   M.transpose . squeezeMatrix DRight . M.transpose
squeezeMatrix DUp =
   M.transpose . squeezeMatrix DLeft . M.transpose

moveMatrix ::
  (RandomGen g) =>
  g
  -> Direction
  -> Matrix Int
  -> Matrix Int
moveMatrix g d m
  | squeezeMatrix d m == m = m
  | otherwise = fillOneHole g . squeezeMatrix d $ m

isGameOver :: Matrix Int -> Bool
isGameOver m =
  and $ map (\x -> m == squeezeMatrix x m) [DUp .. DRight]

mirrorLR :: Matrix Int -> Matrix Int
mirrorLR = M.fromLists . map reverse. M.toLists

mirrorUD :: Matrix Int -> Matrix Int
mirrorUD = M.transpose . mirrorLR . M.transpose

matrixToTable :: Widget Table -> Matrix (Widget FormattedText) -> IO ()
matrixToTable tbl m  = do
  sequence_ $ addRow tbl <$> M.toLists m

textMatrix :: Matrix Int -> IO (Matrix (Widget FormattedText))
textMatrix = sequence . fmap (plainText . T.pack . show)

setTextMatrix :: Matrix (Widget FormattedText) -> Matrix Int -> IO ()
setTextMatrix a b = do
  sequence_ $ elementwise setText a ((T.pack . show) <$> b)

moveTbl :: RandomGen g =>
     g ->
     Widget a ->
     Direction ->
     Matrix Int ->
     Matrix (Widget FormattedText) ->
     IO ()
moveTbl g tbl d m mat =
  let mm = moveMatrix g d m
  in
   if isGameOver mm
   then putStrLn "Game over!"
   else do
     setTextMatrix mat mm
     loopGame g tbl mat mm

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
       moveTbl gen tbl DDown m mat
       return True
     KUp -> do
       moveTbl gen tbl DUp m mat
       return True
     KRight -> do
       moveTbl gen tbl DRight m mat
       return True
     KLeft -> do
       moveTbl gen tbl DLeft m mat
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
