module Lexer
( Lex(..)
, lexMe
, getShuffle
, applyFlags
, getDisplayValue
) where

import Cube
import Moves
import Data.List
import System.Console.ANSI
import System.Environment
import Data.Maybe (fromJust)
import Data.Char
import System.Exit
import Algo
import Control.DeepSeq
import Data.Time
import Data.ByteString as Bs (hGetLine, ByteString)
import Data.Hashable
import System.IO

data Lex = LexError !String | LexFlag !String !String | LexMoves ![String] deriving (Eq)
allFlags = allFlagsNoArg ++ allFlagsArg
allFlagsNoArg = ["-h", "--help", "-i", "--interactive", "-l", "--length", "-d", "--display", "-a", "--animated"]
allFlagsArg = ["-s", "--shuffle"]
allFlagsReturn = ["-l", "--length", "-d", "--display", "-a", "--animated"]
allFlagsApply = take (2 * 2) allFlagsNoArg ++ allFlagsArg
allFlagsFunctions = [helper, putInteractive, createShuffle]

lexMe :: [String] -> String -> [Lex]
lexMe args name
    | lex == [] = [LexError ("No Argument given\nUsage: ./" ++ name ++ " \"shuffle\" flags")]
    | isError == True = [last lex]
    | count > 1 = [LexError "Multiple Shuffle given"]
    | otherwise = lex
    where
        lex = createLex args
        isError = case last lex of
            LexError _ -> True
            otherwise -> False
        count = foldl (\ acc x -> case x of
            LexMoves _ -> acc + 1
            otherwise -> acc
            ) 0 lex

createLex :: [String] -> [Lex]
createLex [] = []
createLex (x:x1:xs)
    | x `elem` allFlagsArg = LexFlag x x1 : createLex xs
createLex (x:xs)
    | x == [] = [LexError "Empty Argument"]
    | x `elem` allFlagsNoArg = LexFlag x [] : createLex xs
    | x `elem` allFlagsArg = [LexError ("No Argument given to the flag \"" ++ filter (/= '-') x ++ "\"")]
    | head x == '-' = [LexError "Unknown Flag"]
    | foldl (\ isOk move -> if move `elem` allMoves then isOk else False) True (words x) = LexMoves (words x) : createLex xs
    | otherwise = [(LexError ("Unknown Move \"" ++ head (filter ([] /=) $ map (\ z -> if z `elem` allMoves then [] else z) (words x)) ++ "\""))]

getShuffle :: [Lex] -> [String]
getShuffle [] = []
getShuffle ((LexMoves moves):_) = moves
getShuffle (_:xs) = getShuffle xs 

applyFlags :: [Lex] -> IO ()
applyFlags lex = do
    let flags = nub $ map (\ x@(LexFlag name arg) -> if length name > 2 then LexFlag (take 2 $ tail name) arg else x) $ getFlags lex
    let functionLst = map getFlagFunction flags
    sequence functionLst
    return ()

getFlags :: [Lex] -> [Lex]
getFlags [] = []
getFlags (f@(LexFlag name arg):xs)
    | name `elem` allFlagsApply = f : getFlags xs
getFlags (_:xs) = getFlags xs

getFlagFunction :: Lex -> IO ()
getFlagFunction (LexFlag name arg) = (allFlagsFunctions !! div (fromJust $ elemIndex name allFlagsApply) 2) arg

helper :: String -> IO ()
helper _ = do
    name <- getProgName
    putStrLn $ "Usage: ./" ++ name ++ " \"shuffle\" flags"
    putStrLn $ "Moves: " ++ foldl1 (\acc x -> acc ++ " " ++ x) allMoves
    putStrLn "Flags:"
    putStrLn "-h --help          Display this message"
    putStrLn "-i --interactive   Launch interactive mode -- got his own helper"
    putStrLn "-l --length        Display the length of the result"
    putStrLn "-d --display       Display the shuffeled cube"
    putStrLn "-a --animated      Launch an animation at the end of the algo showing the solving of the cube"
    putStrLn "-s --shuffle <x>   Create a shuffle of x moves"


putInteractive :: String -> IO ()
putInteractive _ = do
    putInteractive' newCube []

putInteractive' :: Cube -> [Move] -> IO ()
putInteractive' cube moves = do
    putStrLn $ "Moves done: " ++ movesToString moves
    putCubeColor cube
    l <- getLine
    case filter (not . isSpace) $ map toLower l of
        "quit" -> exitWith ExitSuccess
        "clear" -> clear cube moves
        "reset" -> reset cube moves
        "length" -> lengthInteractive cube moves
        "algo" -> algoInteractive cube moves
        "help" -> help cube moves
        otherwise -> return ()
    name <- getProgName
    let lexed = lexMe [l] name
    case head lexed of
        LexError str -> do
            clearFromCursorToScreenEnd
            putStrLn $ "Error: " ++ str
            cursorUpLine 2
            clearLine
            cursorUpLine 18
            putInteractive' cube moves
        otherwise -> return ()
    let shuffle = getShuffle lexed
    let myMove = head $ map toMove shuffle
    let movedCube = (moveToAction myMove) cube
    cursorUpLine 1
    clearFromCursorToScreenEnd
    cursorUpLine 17
    putInteractive' movedCube (moves ++ [myMove])

clear :: Cube -> [Move] -> IO ()
clear cube moves = do
    clearScreen
    setCursorPosition 0 0
    putInteractive' cube moves

reset :: Cube -> [Move] -> IO ()
reset cube moves =  do
    clearScreen
    setCursorPosition 0 0
    putInteractive' newCube []

lengthInteractive :: Cube -> [Move] -> IO ()
lengthInteractive cube moves =  do
    clearFromCursorToScreenEnd
    putStrLn $ "Strokes: " ++ show (length moves)
    cursorUpLine 2
    clearLine
    cursorUpLine 18
    putInteractive' cube moves

algoInteractive :: Cube -> [Move] -> IO ()
algoInteractive cube moves =  do
    clearFromCursorToScreenEnd
    let shuffeledCube =  foldl (\ cube move -> applyMove move cube) newCubeNumber $ map moveToInt moves
    time <- getCurrentTime
    let ret = algo shuffeledCube
    time2 <- (movesToString ret) `deepseq` getCurrentTime
    putStrLn ("Strokes: " ++ show (length ret))
    putStrLn ("Time: " ++ (show $ diffUTCTime time2 time))
    putMoves ret
    cursorUpLine 4
    clearLine
    cursorUpLine 18
    putInteractive' cube moves

help :: Cube -> [Move] -> IO ()
help cube moves = do
    clearFromCursorToScreenEnd
    putStrLn "Help: Write a move or a command and press enter"
    putStrLn $ "Moves: " ++ foldl1 (\acc x -> acc ++ " " ++ x) allMoves
    putStrLn "Commands:"
    putStrLn "- help: Display this message"
    putStrLn "- quit: Quit the program"
    putStrLn "- clear: Clear the window"
    putStrLn "- reset: Reset the cube and all your moves"
    putStrLn "- length: Count the number of moves that you have already done"
    putStrLn "- algo: Launch the algo with the current cube"
    cursorUpLine 10
    clearLine
    cursorUpLine 18
    putInteractive' cube moves

createShuffle :: String -> IO ()
createShuffle str = do
    let filtered = filter (isNumber) str
    let readed = read str
    if not $ all (isNumber) str || filtered == []
    then (do
        putStrLn "Error: Wrong argument given"
        exitWith (ExitFailure 2))
    else if readed > 9999
    then (do
        putStrLn "Error: Argument to big"
        exitWith (ExitFailure 2))
    else return ()
    handle <- openFile "/dev/random" ReadMode  
    lines <- getMultipleLines readed [] handle
    hClose handle
    let salts = map (abs . hash) lines
    putMoves $ map intToMove $ createList salts (-1) (-1)

createList :: [Int] -> Int -> Int -> [Int]
createList [] _ _ = []
createList (x:xs) oldMove oldOldMove
    | x < 18 = x : createList xs (div x 3) oldMove
    | newMoveRaw == oldMove || (oldOldMove == newMoveRaw && (div newMoveRaw 2) == (div oldMove 2)) = createList (div x 2:xs) oldMove oldOldMove
    | otherwise = newMove : createList xs newMoveRaw oldMove
    where
        newMove = mod x 18
        newMoveRaw = div newMove 3

getMultipleLines :: Int -> [ByteString] -> Handle -> IO [ByteString]
getMultipleLines 0 ret _ = return ret
getMultipleLines x ret handle = do
    line <- Bs.hGetLine handle
    getMultipleLines (x - 1) (line:ret) handle
        

getDisplayValue :: [Lex] -> [Int]
getDisplayValue [] = []
getDisplayValue ((LexFlag name arg):xs)
    | name `elem` allFlagsReturn = div (fromJust $ elemIndex name allFlagsReturn) 2 : getDisplayValue xs
getDisplayValue (_:xs) = getDisplayValue xs