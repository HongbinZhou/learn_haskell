module RecusiveContents (getRecursiveContents) where

import Control.Monad (forM, filterM)
import System.Directory (doesDirectoryExist, getDirectoryContents, Permissions(..), getModificationTime, getPermissions)
import Data.Time (UTCTime(..))
import System.FilePath ((</>), takeExtension)
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

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)

-- | find file with name `file`
findFileWithName :: FilePath -> FilePath -> IO [FilePath]
findFileWithName file = simpleFind ((==) file)

-- | find file with extention `ext`
findFileWithExt :: String -> FilePath -> IO [FilePath]
findFileWithExt ext =
    simpleFind (\f -> takeExtension f == ext )

-- |
type Predicate = FilePath
               -> Permissions   -- file permissions
               -> Maybe Integer -- file size
               -> UTCTime       -- file modifid time
               -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path =
    getRecursiveContents path >>= filterM check
        where check :: FilePath -> IO Bool
              check file = do
                perm <- getPermissions file
                size <- saferFileSize file
                mtime <- getModificationTime file
                return (p file perm size mtime)

type InfoP a = FilePath
             -> Permissions
             -> Maybe Integer
             -> UTCTime
             -> a

pathP :: InfoP FilePath
pathP p _ _ _ = p

permP :: InfoP Permissions
permP _ perm _ _ = perm

sizeP :: InfoP Integer
sizeP _ _ (Just s) _ = s
sizeP _ _ _ _ = -1

timeP :: InfoP UTCTime
timeP _ _ _ t = t

equalP' :: (Eq a) => InfoP a -> a -> Predicate
equalP' info a = \f p s t -> (info f p s t) == a

liftP :: (a -> b -> c)
      -> (InfoP a)
      -> b
      -> (InfoP c)
liftP ff info  b =
    \f p s t -> (info f p s t) `ff` b

equalP :: (Eq a) => InfoP a -> a -> Predicate
equalP = liftP (==)

greaterP :: (Ord a) => InfoP a -> a -> Predicate
greaterP = liftP (>)

lessP :: (Ord a) => InfoP a -> a -> Predicate
lessP = liftP (<)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 k ma mb = \f p s t -> (ma f p s t) `k` (mb f p s t)

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> (InfoP a)
liftPath ff = \f _ _ _ -> ff f

test1 = betterFind (liftPath takeExtension `equalP` ".hs") "."
test11 = betterFind (pathP `equalP` "./re.hs") "."

test2 = betterFind (sizeP `greaterP` 100000) "."

test3 = do
  perm <- getPermissions "./re.hs"
  betterFind (permP `greaterP` perm) "."

test4 = do
  t <- getModificationTime "./re.hs"
  betterFind (timeP `greaterP` t) "."

test5 = betterFind
        ((pathP `equalP` "./re.hs")
        `andP` (sizeP `greaterP` 100))
        "."
