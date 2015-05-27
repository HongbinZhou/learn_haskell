import Data.Foldable
import Data.Maybe

-- import Control.Monad.State

data Player = X | O
            deriving (Eq, Show)

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
         PosPlayer (Position (1,2)) (Just X),
         PosPlayer (Position (1,3)) (Just X),
         PosPlayer (Position (2,1)) Nothing,
         PosPlayer (Position (2,2)) (Just X),
         PosPlayer (Position (2,3)) Nothing,
         PosPlayer (Position (3,1)) Nothing,
         PosPlayer (Position (3,2)) Nothing,
         PosPlayer (Position (3,3)) (Just X)]
         (Finished $ Step [])

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

whoWon :: Board -> Either (Maybe Player) String
whoWon b = case getStatus b of
            Finished _ -> Left player
            _ -> Right "Error: whoWon can only run on finished board!"
            where player :: Maybe Player
                  player = case catMaybes $ map (whoMakeLine b) allLines of
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

move :: Board -> Position -> Player -> Either Board String
move b p who =
  case getStatus b of
   Finished _ -> Right $ "Error: can't move more for finished board!"
   Empty (Step pp)-> Left $ Board posPlayer (status pp)
   InPlay (Step pp) -> 
     if isOccupied b p
     then Right $ "Position" ++ show p ++ "was occupied. replay please! "
     else Left $ Board posPlayer (status pp)
  where posPlayer = setPosPlayer (getPosPlayers b) p (Just who)
        status pp = InPlay $ Step $ (PosPlayer p (Just who)):pp

test_move = move emptyBoard (Position (1,2)) X
test_takeBack = case test_move  of
        Left b -> takeBack b

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
