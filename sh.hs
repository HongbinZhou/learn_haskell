#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
-- import qualified Data.Text as T

datePwd :: IO UTCTime
datePwd = do
  dir <- pwd
  datefile dir

str :: Text
str = "Hello"

main :: IO ()
main = do
  echo str
  echo $ str <> "zhou"
  dir <- pwd
  time <- datefile dir
  print time

main' = do
  time <- datePwd
  print time

test1 :: IO ()
test1 = do
  _ <- proc "ls" ["-a", "-l"] empty
  die "hi"

test2 :: IO ()
test2 = do
  _ <- shell "ls -al" empty
  die "hi"

test3 :: IO ()
test3 = stdout stdin

getEnv  x = do
  export  "hi" "zhouasdf"
  e <- env
  print $  lookup x e
