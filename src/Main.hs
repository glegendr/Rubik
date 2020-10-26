module Main where

import Moves
import Lexer
import System.Environment
import System.Exit
import System.Console.ANSI
import Control.Concurrent
import Cube
import Verification
import Algo


main :: IO ()
main = do
    -- args <- getArgs
    -- let lexed = lexMe args
    -- case head lexed of
    --     LexError str -> alert str
    --     otherwise -> return ()
    -- let shuffle = getShuffle lexed
    -- let shuffeledCube = foldl (\ cube f -> f cube) newCube $ map moveToAction $ map toMove shuffle
    -- putCubeColor shuffeledCube
    -- let ret = algo shuffeledCube
    -- putStrLn $ show $ length ret
    -- putMoves ret
    ---------------
    -- putAnimatedCube False shuffeledCube (map moveToAction ret)
    putStrLn $ show $ makeMoves (take 400000 (repeat (MRight ONothing))) newCube
    -- 400000 U - (3.502 + 3.596 + 3.587 + 3.584 + 3.622) / 5 = 3,5782 
    -- 400000 F - (9.117 + 9.017 + 8.968 + 8.929 + 9.023) / 5 = 9,0108 
    -- 400000 R - (11.191 + 10.002 + 10.030 + 9.075 + 9.986) / 5 = 10,0568


    -- putInteractive newCube
    {-- putAnimatedCube False newCube [moveR, moveU, moveR', moveU'] --}
    {--rtxOnCube newCube--}
    {--putCubeColor $ moveR newCube
    putCubeColor $ moveU $ moveR newCube
    putCubeColor $ moveR' $ moveU $ moveR newCube
    putCubeColor $ moveU' $ moveR' $ moveU $ moveR newCube
    putStrLn $ "U': " ++ (show $ moveU' $ moveR $ moveD2 $ moveL $ moveF' newCube)
    putStrLn $ "R2: " ++ (show $ moveR2 $ moveU' $ moveR $ moveD2 $ moveL $ moveF' newCube)
    putStrLn $ "D : " ++ (show $ moveD $ moveR2 $ moveU' $ moveR $ moveD2 $ moveL $ moveF' newCube)
    putCubeColor $ moveB $ moveD $ moveR2 $ moveU' $ moveR $ moveD2 $ moveL $ moveF' newCube
    putCubeColor newCube--}

{--F'LD2RU'R2D--}

alert :: String -> IO a
alert str = do
    putStrLn $ "Error: " ++ str 
    exitWith (ExitFailure 2)

putAnimatedCube :: Bool -> Cube -> [(Cube -> Cube)] -> IO ()
putAnimatedCube False cube [] = putCubeColor cube
putAnimatedCube True cube [] = rtxOnCube cube
putAnimatedCube False cube (x:xs) = do
    putCubeColor cube
    cursorUpLine 15
    threadDelay 250000
    putAnimatedCube False (x cube) xs
putAnimatedCube True cube (x:xs) = do
    rtxOnCube cube
    cursorUpLine 41
    threadDelay 500000
    putAnimatedCube True (x cube) xs

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
    cursorUpLine 17
    putCubeColor movedCube
    putStrLn $ show $ countAll movedCube
    putInteractive movedCube


countAll :: Cube -> Bool
countAll cube = all (== 9) $ countAll' $ foldl1 (++) cube

countAll' :: [Char] -> [Int]
countAll' lst = (length $ filter (== 'W') lst):(length $ filter (== 'G') lst):(length $ filter (== 'R') lst):(length $ filter (== 'Y') lst):(length $ filter (== 'B') lst):(length $ filter (== 'O') lst):[]