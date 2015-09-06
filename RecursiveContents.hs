module RecusiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names' <- getDirectoryContents topdir
  let names = filter (`notElem` [".", ".."]) names'
  paths <- forM names $ \n -> do
             let p = topdir </> n
             isDir <- doesDirectoryExist p
             if isDir
             then getRecursiveContents p
             else return [p]
  return (concat paths)
