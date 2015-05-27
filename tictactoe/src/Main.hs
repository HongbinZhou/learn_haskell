import Data.Foldable

data Player = X | O
            deriving (Eq, Show)

data Position = Position {
  getPosition :: (Int,Int)}
              deriving (Eq)

data PosPlayer = PosPlayer {
  getPlayerPos :: Position,
  getPlayer :: Maybe Player}
              deriving (Eq)

data Status =
  Empty
  | InPlay
  | Finished
  deriving (Eq, Show)

data Board = Board {
  getPosPlayer :: [PosPlayer],
  getStatus   :: Status}
           deriving (Eq)


emptyBoard :: Board
emptyBoard = Board posPlayer Empty
  where posPlayer = map (\x -> PosPlayer x Nothing) matrix  
        matrix = [Position (x,y)| x <- [1..3], y <- [1..3]]

move :: Board -> Position -> Player -> Either Board String
move b p who = 
  case getStatus b of
   Finished -> Right "move takes an Empty or InPlay board"
   _ -> setPosition b p who

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
         PosPlayer (Position (3,2)) (Just X)]
         Finished

toPosition:: (Int, Int) -> Position
toPosition (a, b) = Position (a, b)

row :: Int -> [Position]
row n = [Position (n, x) | x <- [1..3]]

col :: Int -> [Position]
col n = [Position (x, n) | x <- [1..3]]

leftX :: [Position]
leftX = [Position (x, x) | x <- [1..3]]

rightX :: [Position]
rightX = [Position (x, y) | x<- [1..3], y<-[1..3], x+y==4]

whoWon :: Board -> Position -> [Maybe Player]
whoWon b (Position (m, n)) = 
  case (m, n) of
   (1, 1) -> map (whoMakeLine b) [row 1, leftX, col 1]
   (1, 2) -> map (whoMakeLine b) [row 2]
   (1, 3) -> map (whoMakeLine b) [rightX, col 3]
   (2, 1) -> map (whoMakeLine b) [row 2]
   (3, 1) -> map (whoMakeLine b) [row 3]

whoWon' :: Board -> [Maybe Player]
whoWon' b = foldl (\acc x -> acc ++ (whoWon b x)) [] 
            ((row 1) ++ (toPosition <$>[(2,1), (3, 1)]))

whoMakeLine :: Board -> [Position] -> Maybe Player
whoMakeLine b (px:py:pz:[]) = do
     x <- playerAt b px
     y <- playerAt b py
     z <- playerAt b pz
     if x==y && y==z 
       then return x 
       else Nothing

playerAt :: Board -> Position -> Maybe Player
playerAt (Board _ Empty) _ = Nothing
playerAt (Board b _) p = 
  case find (\(PosPlayer pos _) -> pos==p) b of
   Just (PosPlayer pos' player) -> player
   _ -> Nothing

isOccupied :: Board -> Position -> Bool
isOccupied b p =
  case playerAt b p of
   Just _ -> True
   _ -> False

setPosition :: Board -> Position -> Player -> Either Board String
setPosition b@(Board bb _) p@(Position (x, y)) who =
  if isOccupied b p
  then Right $ "Position (" ++ show x ++ "," ++ show y ++ ") was occupied. replay please! "
  else Left $ Board posPlayer InPlay
       where posPlayer = foldl (\acc pp ->
                                  if getPlayerPos pp == p
                                  then acc ++ [PosPlayer p (Just who)]
                                  else acc ++ [pp]) [] bb

instance Show Position where
  show (Position (x, y)) = 
    show x ++ ", " ++ show y

instance Show PosPlayer where
  show (PosPlayer pos player) = 
    "(" ++ show pos ++ ", " ++ show player ++ ")"

instance Show Board where
  show (Board xs _) = 
    unlines $ map show (group3By3 xs)
    where group3By3 :: [a] -> [[a]]
          group3By3 [] = []
          group3By3 x = [take 3 x] ++ group3By3 (drop 3 x)
