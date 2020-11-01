module Main where

import Moves
import Lexer
import System.Environment
import System.Exit
import System.Console.ANSI
import Control.Concurrent
import Cube
import Algo


main :: IO ()
main = do
    args <- getArgs
    let lexed = lexMe args
    case head lexed of
        LexError str -> alert str
        otherwise -> return ()
    let shuffle = getShuffle lexed
    let shuffeledCube =  foldl (\ cube move -> applyMove move cube) newCubeNumber $ map (moveToInt . toMove) shuffle
    let shuffeledCube' = foldl (\ cube f -> f cube) newCube $ map (moveToAction . toMove) shuffle
    let ret = algo shuffeledCube
    putMoves ret
    -- putStrLn $ show $ makeMovesNumber ret shuffeledCube
    -- putCubeColor $ makeMoves ret shuffeledCube'

alert :: String -> IO a
alert str = do
    putStrLn $ "Error: " ++ str 
    exitWith (ExitFailure 2)

putAnimatedCube :: Cube -> [(Cube -> Cube)] -> IO ()
putAnimatedCube cube [] = putCubeColor cube
putAnimatedCube cube (x:xs) = do
    putCubeColor cube
    cursorUpLine 15
    threadDelay 250000
    putAnimatedCube (x cube) xs

putInteractive :: Cube -> IO ()
putInteractive cube = do
    l <- getLine
    let lexed = lexMe [l]
    case head lexed of
        LexError str -> do
            putStrLn $ "Error: " ++ str
            putInteractive cube
        otherwise -> return ()
    let shuffle = getShuffle lexed
    let myMove = head $ map toMove shuffle
    let movedCube = (moveToAction myMove) cube
    cursorUpLine 16
    putCubeColor movedCube
    putInteractive movedCube
