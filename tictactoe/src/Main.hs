import Data.Foldable
import Data.Maybe
import System.Random


data Player = X | O
            deriving (Eq, Bounded, Enum, Show)

-- Learn random
-- ref: http://en.wikibooks.org/wiki/Haskell/Libraries/Random
instance Random Player where
  random g = case randomR (fromEnum (minBound :: Player),
                           fromEnum (maxBound :: Player))
                  g
             of (r, g') -> (toEnum r, g')

  randoms g = case random g of
               (a, g') -> a:(randoms g')

  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                     (r, g') -> (toEnum r, g')

genPlayers :: StdGen -> [Player]
genPlayers g =
  let (fst, _) = random g :: (Player, StdGen)
  in case fst of
      X -> concat $ repeat [X, O]
      O -> concat $ repeat [O, X]

test_genPlays = do
  gen <- newStdGen
  putStrLn $ show $ take 10 $ genPlayers gen

test_randoms = do
  gen <- newStdGen
  let x = take 10 $ randoms gen :: [Player]
  return $ show x

test_random = do
  gen <- newStdGen
  let (x, y) = random gen :: (Player, StdGen)
  return $ show x

test_randomR = do
  gen <- newStdGen
  let (x, y) = randomR (X, O) gen
  return $ show x


data Position = Position {
  getPosition :: (Int,Int)}
              deriving (Eq)

data PosPlayer = PosPlayer {
  getPos :: Position,
  getPlayer :: Maybe Player}
              deriving (Eq)

data Status =
  Empty Step
  | InPlay Step
  | Finished Step
  deriving (Show)

data Board = Board {
  getPosPlayers :: [PosPlayer],
  getStatus   :: Status}

emptyBoard :: Board
emptyBoard = Board posPlayer (Empty $ Step [])
  where posPlayer = map (\x -> PosPlayer x Nothing) matrix
        matrix = [Position (x,y)| x <- [1..3], y <- [1..3]]

data Step = Step [PosPlayer]
            deriving (Show)

testBoard :: Board
testBoard =
  Board [PosPlayer (Position (1,1)) (Just X),
         PosPlayer (Position (1,2)) (Just O),
         PosPlayer (Position (1,3)) (Just O),
         PosPlayer (Position (2,1)) Nothing,
         PosPlayer (Position (2,2)) (Just X),
         PosPlayer (Position (2,3)) Nothing,
         PosPlayer (Position (3,1)) Nothing,
         PosPlayer (Position (3,2)) Nothing,
         PosPlayer (Position (3,3)) Nothing]
  -- (InPlay $ Step [])
  (InPlay $ Step [PosPlayer (Position (1, 3)) (Just O),
                 PosPlayer (Position (2, 2)) (Just X),
                 PosPlayer (Position (1, 2)) (Just O),
                 PosPlayer (Position (1, 1)) (Just X)])

toPosition:: (Int, Int) -> Position
toPosition (a, b) = Position (a, b)

point2Line :: [(Int, Int)] -> Line
point2Line x =
  let [px,py,pz] = Position <$> x
  in Line px py pz

row :: Int -> Line
row n = point2Line [(n, x) | x <- [1..3]]

col :: Int -> Line
col n = point2Line [(x, n) | x <- [1..3]]

leftX :: Line
leftX = point2Line [(x, x) | x <- [1..3]]

rightX :: Line
rightX = point2Line [(x, y) | x <- [1..3], y <- [1..3], x+y==4]

allLines :: [Line]
allLines = [f x | f <- [row, col], x <- [1,2,3]] ++
           [leftX, rightX]

data Line = Line Position Position Position
            deriving (Show)

whoMakeLine :: Board -> Line -> Maybe Player
whoMakeLine b (Line px py pz) = do
     x <- playerAt b px
     y <- playerAt b py
     z <- playerAt b pz
     if x==y && y==z
       then return x
       else Nothing


whoWon :: Board -> Maybe Player
whoWon b =
  case catMaybes $ map (whoMakeLine b) allLines of
   [] -> Nothing
   x:_ -> Just x

playerAt :: Board -> Position -> Maybe Player
playerAt (Board _ (Empty _)) _ = Nothing
playerAt (Board b _) p =
  case find (\(PosPlayer pos _) -> pos==p) b of
   Just (PosPlayer pos' player) -> player
   _ -> Nothing

isOccupied :: Board -> Position -> Bool
isOccupied b p =
  case playerAt b p of
   Just _ -> True
   _ -> False

getSteps :: Board -> [PosPlayer]
getSteps b =
  case getStatus b of
   Empty (Step x) -> x
   InPlay (Step x) -> x
   Finished (Step x) -> x

move :: Board -> Position -> Player -> Either Board String
move b p who =
  case getStatus b of
    Finished _ -> Right $ "Error: can't move more for finished board!"
    Empty    _ -> Left $ Board posPlayer status
    InPlay   _ ->
      if isOccupied b p
      then Right $ "Warning: Position" ++ show p ++ " was occupied, play again!"
      else Left $ Board posPlayer status
  where posPlayer = setPosPlayer (getPosPlayers b) p (Just who)
        newStep = Step $ (PosPlayer p (Just who)): (getSteps b)
        status =
          -- First do move on given board, then calc status
          let newBoard = Board posPlayer (InPlay newStep)
          in
           case whoWon newBoard of
            Just _ -> Finished newStep
            _ -> if isBoardFull newBoard
                 then Finished newStep
                 else InPlay newStep

takeBack :: Board -> Either Board String
takeBack b =
  case getStatus b of
   Empty _ -> Right "Error: Can't do takeBack for Empty board!"
   InPlay (Step x) -> genNewBoard x
   Finished (Step x) -> genNewBoard x
  where genNewBoard (x:xs) = Left $ Board posPlayer step
          where posPlayer = setPosPlayer (getPosPlayers b) (getPos x) Nothing
                step = InPlay (Step xs)

setPosPlayer :: [PosPlayer] -> Position -> Maybe Player -> [PosPlayer]
setPosPlayer p pos mp = foldl (\acc pp ->
                            if  getPos pp == pos
                            then acc ++ [PosPlayer pos mp]
                            else acc ++ [pp]) [] p

instance Show Position where
  show (Position (x, y)) =
    "(" ++ show x ++ ", " ++ show y ++ ")"

instance Show PosPlayer where
  show (PosPlayer pos player) =
    "(" ++ show pos ++ ", " ++ show player ++ ")"

instance Show Board where
  show (Board xs s) =
    (unlines $ map showRow (group3By3 xs)) ++ show s
    where showRow = foldl (\acc x -> acc ++ show x ++ " " ) ""
          group3By3 :: [a] -> [[a]]
          group3By3 [] = []
          group3By3 x = [take 3 x] ++ group3By3 (drop 3 x)

allPosition :: [Position]
allPosition = [Position (m, n) | m <- [1..3], n <- [1..3]]

isInBoard :: Position -> Bool
isInBoard p = elem p allPosition

isBoardFull :: Board -> Bool
isBoardFull b = and $ isOccupied b <$> allPosition

posParser :: String -> Maybe (Int, Int)
posParser cs0 =
  case [ (x, y, cs2) | (x, cs1) <- reads cs0, (y, cs2) <- reads cs1 ] of
   [(x, y, "")] -> if isInBoard (Position (x, y))
                   then Just (x, y)
                   else Nothing
   _ -> Nothing

info :: String -> String
info = (++) ">>> INFO: "

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

isFinished :: Board -> Bool
isFinished b =
  case getStatus b of
   Finished _  -> True
   Empty _ -> False
   InPlay _ ->  False

loopGame :: Board -> Player -> IO()
loopGame b who = do
  putStrLn $ info "Current board is:"
  putStrLn $ show b
  if isFinished b
    then do
    case whoWon b of
     Just p -> putStrLn $ info "Game finished. Player "++ show who ++ " win!"
     Nothing -> if isBoardFull b
                then putStrLn $ info "Game finished. We tie!"
                else putStrLn $ info "Something wrong!"
    else do
      putStrLn $ info "Player " ++ show who ++ ", input your position (eg: 1 2): "
      pos <- getLine
      case posParser pos of
       Just (x, y) ->
         let pos = Position (x, y)
         in do
           putStrLn $ "Your input position is:" ++ show pos
           case move b pos who of
            Left b' -> loopGame b' (nextPlayer who)
            Right s -> do
              putStrLn s
              loopGame b who
       Nothing -> do
         putStrLn "Error: Invalid input!"
         loopGame b who


main :: IO()
main = do
  putStrLn "Let's play tic tac toe game!"
  gen <- newStdGen
  let (fst, _) = random gen :: (Player, StdGen)
  loopGame emptyBoard fst

