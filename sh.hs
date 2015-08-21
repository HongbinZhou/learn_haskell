#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
-- import qualified Data.Text as T

str = "Hello"
main = do
  echo str
  echo $ str <> "zhou"
  dir <- pwd
  time <- datefile dir
  print time



