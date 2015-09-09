module RecusiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO (hFileSize, hClose, openFile, withFile, IOMode(..))
import System.Posix (getFileStatus, fileSize, FileOffset)
import Control.Exception.Base (SomeException(..), handle, bracket)

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

-- | only get size for normal files,
--   others are exception, will return Nothing
saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize file =
    handle (\(SomeException _) -> return Nothing) $ do
      h <- openFile file ReadMode
      size <- hFileSize h
      hClose h
      return (Just size)

-- |
--  http://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Exception.html#v:bracket
--  bracket :: IO a            ,computation to run first ("acquire resource")
--             -> (a -> IO b)  ,computation to run last ("release resource")
--             -> (a -> IO c)  ,computation to run in-between
--             -> IO c
getFileSize' :: FilePath -> IO (Maybe Integer)
getFileSize' file =
    handle (\(SomeException _) -> return Nothing) $
      bracket (openFile file ReadMode) hClose $ \h -> do
          size <- hFileSize h
          return (Just size)

-- | no need to open file
getFileSize :: FilePath -> IO FileOffset
getFileSize path = do
    fs <- getFileStatus path
    return $ fileSize fs
