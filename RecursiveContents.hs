module RecusiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO
import System.Posix (getFileStatus, fileSize, FileOffset)

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

-- | need to open file first
simpleFileSize :: FilePath -> IO Integer
simpleFileSize path =
    withFile path ReadMode hFileSize

-- | no need to open file
getFileSize :: FilePath -> IO FileOffset
getFileSize path = do
    fs <- getFileStatus path
    return $ fileSize fs
