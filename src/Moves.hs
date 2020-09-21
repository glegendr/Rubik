module Moves
    ( Move(..)
    , toMove
    ) where

data Option = ODirection | OStrokes | ONothing
instance Show Option where
    show ODirection = "Direction"
    show OStrokes = "Strokes"
    show ONothing = "Nothing"
    
data Move =  MFront !Option | MRight !Option | MUp !Option | MBack !Option | MLeft !Option | MDown !Option
instance Show Move where
    show (MFront o) = "Front "  ++ show o 
    show (MRight o) = "Right " ++ show o
    show (MUp o) = "Up " ++ show o
    show (MBack o) = "Back " ++ show o
    show (MLeft o) = "Left " ++ show o
    show (MDown o) = "Down " ++ show o


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