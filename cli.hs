import System.Environment (getArgs)

-- A smile command line script.
-- ref: http://book.realworldhaskell.org/read/functional-programming.html#fp.fold.exercises
-- How to use:
--     ghc --make cli.hs
--     cli input.txt output.txt


interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inFile outFile = do
  input <- readFile inFile
  writeFile outFile (f input)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> interactWith id input output
    _ -> error "exactly two arguments needed!"
