{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Graphics.Vty.Widgets.All
import Graphics.Vty.Input.Events

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
  
