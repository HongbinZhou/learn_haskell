
import System.Random

main = do
  putStrLn "Guess the number!"

  gen <- newStdGen
  let (x:_) = randomRs (0,100) gen :: [Integer]
  putStrLn $ "The random number is: " ++ (show x)
  oneGuess x

oneGuess :: Integer -> IO()
oneGuess y = do
  putStrLn "Please input your guess (input 'q' to quit):"
  x <- getLine
  case x of
   "q" -> return ()
   _   -> case reads x :: [(Integer, String)] of
           [(x', _)] -> case compare x' y of
                          LT -> do
                            putStrLn "Smaller"
                            oneGuess y
                          GT -> do
                            putStrLn "Bigger"
                            oneGuess y
                          EQ ->
                            putStrLn "You win"
           _ -> do
             putStrLn "Illegal input"
             oneGuess y

