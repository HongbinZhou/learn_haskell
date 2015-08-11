import qualified Data.Text as T
import qualified Data.Text.IO as T


load :: FilePath -> IO ()
load  path = do
  c <- T.readFile path
  _ <- T.putStr $ process c
  T.writeFile (path ++ ".rev") $ process c

process = T.unlines . map T.reverse . T.lines
