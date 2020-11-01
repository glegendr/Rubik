module Moves
    ( Move(..)
    , Option(..)
    , toMove
    , putMoves
    , changeDirection
    , moveToInt
    , intToMove
    ) where

data Option = ODirection | OStrokes | ONothing deriving (Eq)
instance Show Option where
    show ODirection = "Direction"
    show OStrokes = "Strokes"
    show ONothing = "Nothing"
    
data Move =  MFront !Option | MRight !Option | MUp !Option | MBack !Option | MLeft !Option | MDown !Option deriving (Eq)
instance Show Move where
    show (MFront o) = "Front "  ++ show o 
    show (MRight o) = "Right " ++ show o
    show (MUp o) = "Up " ++ show o
    show (MBack o) = "Back " ++ show o
    show (MLeft o) = "Left " ++ show o
    show (MDown o) = "Down " ++ show o

moveToString :: [Move] -> String
moveToString [] = []
moveToString ((MFront opt):xs) = "F" ++ optionToString opt ++ " " ++ moveToString xs
moveToString ((MRight opt):xs) = "R" ++ optionToString opt ++ " " ++ moveToString xs
moveToString ((MUp opt):xs) = "U" ++ optionToString opt ++ " " ++ moveToString xs
moveToString ((MBack opt):xs) = "B" ++ optionToString opt ++ " " ++ moveToString xs
moveToString ((MLeft opt):xs) = "L" ++ optionToString opt ++ " " ++ moveToString xs
moveToString ((MDown opt):xs) = "D" ++ optionToString opt ++ " " ++ moveToString xs

optionToString :: Option -> String
optionToString ODirection = "\'"
optionToString OStrokes = "2"
optionToString ONothing = []

putMoves :: [Move] -> IO ()
putMoves = putStrLn . moveToString

toMove :: String -> Move
toMove str
    | mv == 'F' = MFront $ getOpt opt
    | mv == 'R' = MRight $ getOpt opt
    | mv == 'U' = MUp $ getOpt opt
    | mv == 'B' = MBack $ getOpt opt
    | mv == 'L' = MLeft $ getOpt opt
    | otherwise = MDown $ getOpt opt 
    where
        mv = head str
        opt = tail str


getOpt :: String -> Option
getOpt opt
    | opt == "\'" = ODirection
    | opt == "2" = OStrokes
    | otherwise = ONothing


changeDirection :: Move -> Move
changeDirection (MFront o) = MFront (opposeDirection o)
changeDirection (MRight o) = MRight (opposeDirection o)
changeDirection (MUp o) = MUp (opposeDirection o)
changeDirection (MBack o) = MBack (opposeDirection o)
changeDirection (MLeft o) = MLeft (opposeDirection o)
changeDirection (MDown o) = MDown (opposeDirection o)

opposeDirection :: Option -> Option
opposeDirection ODirection = ONothing
opposeDirection ONothing = ODirection
opposeDirection OStrokes = OStrokes

moveToInt :: Move -> Int
moveToInt (MUp o) = 0 + optionToInt o
moveToInt (MDown o) = 3 + optionToInt o
moveToInt (MFront o) = 6 + optionToInt o
moveToInt (MBack o) = 9 + optionToInt o
moveToInt (MLeft o) = 12 + optionToInt o
moveToInt (MRight o) = 15 + optionToInt o

optionToInt :: Option -> Int
optionToInt ONothing = 0
optionToInt OStrokes = 1
optionToInt ODirection = 2

intToMove :: Int -> Move
intToMove m
    | move == 0 = MUp (intToOption option)
    | move == 1 = MDown (intToOption option)
    | move == 2 = MFront (intToOption option)
    | move == 3 = MBack (intToOption option)
    | move == 4 = MLeft (intToOption option)
    | move == 5 = MRight (intToOption option)
    where
        move = div m 3
        option = mod m 3

intToOption :: Int -> Option
intToOption 0 = ONothing
intToOption 1 = OStrokes
intToOption 2 = ODirection