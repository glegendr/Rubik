module Moves
    ( Move(..)
    , Option(..)
    , toMove
    , putMoves
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