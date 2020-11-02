module Main where

import Moves
import Lexer
import System.Environment
import System.Exit
import System.Console.ANSI
import Control.Concurrent
import Cube
import Algo
import Control.DeepSeq


main :: IO ()
main = do
    args <- getArgs
    name <- getProgName
    let lexed = lexMe args name
    case head lexed of
        LexError str -> alert str
        otherwise -> return ()
    applyFlags lexed
    let shuffle = getShuffle lexed
    let shuffeledCube =  foldl (\ cube move -> applyMove move cube) newCubeNumber $ map (moveToInt . toMove) shuffle
    let shuffeledCube' = foldl (\ cube f -> f cube) newCube $ map (moveToAction . toMove) shuffle
    let ret = algo shuffeledCube
    let displayValues = getDisplayValue lexed
    if 2 `elem` displayValues
    then initAnimatedCube shuffeledCube' ret
    else if 1 `elem` displayValues
    then putCubeColor $ makeMoves ret shuffeledCube'
    else return ()
    if 0 `elem` displayValues && (not $ 2 `elem` displayValues)
    then putStrLn ("Strokes: " ++ show (length ret))
    else return ()
    case ret of
        [] -> return ()
        otherwise -> if 2 `elem` displayValues then return () else putMoves ret

alert :: String -> IO a
alert str = do
    putStrLn $ "Error: " ++ str 
    exitWith (ExitFailure 2)

initAnimatedCube :: Cube -> [Move] -> IO ()
initAnimatedCube x y = do
    let rdy = (movesToString y) `deepseq` "--------<(  READY  )>--------"
    putStrLn rdy
    threadDelay 500000
    cursorUpLine 1
    clearLine
    putStrLn "--------<(    3    )>--------"
    threadDelay 500000
    cursorUpLine 1
    clearLine
    putStrLn "--------<(    2    )>--------"
    threadDelay 500000
    cursorUpLine 1
    clearLine
    putStrLn "--------<(    1    )>--------"
    threadDelay 500000
    cursorUpLine 1
    clearLine
    putStrLn "--------<(   GOO   )>--------"
    putAnimatedCube x y []

putAnimatedCube :: Cube -> [Move] -> [Move] -> IO ()
putAnimatedCube cube [] lst = do
    putCubeColor cube
    putStrLn $ "Current Move: Finish"
    putStrLn $ "Move Done: " ++ movesToString lst
    putStrLn $ "Progress: " ++ makeBar (length lst) 0
    putStrLn $ "Strokes: " ++ show (length lst)
putAnimatedCube cube (x:xs) lst = do
    putCubeColor cube
    clearFromCursorToScreenEnd
    putStrLn $ "Current Move: " ++ movesToString [x]
    putStrLn $ "Move Done: " ++ movesToString lst
    putStrLn $ "Progress: " ++ makeBar (length lst + 1) (length xs)
    putStrLn $ "Strokes: " ++ show (length lst + 1)
    cursorUpLine 19
    threadDelay 250000
    putAnimatedCube (moveToAction x cube) xs (lst ++ [x])

makeBar :: Int -> Int -> String
makeBar done rest =
    "[" ++ replicate barLength '=' ++ replicate (50 - barLength) ' ' ++ "]"
    where
        max = done + rest
        barLength = round $ ((realToFrac done) / (realToFrac max)) * 50