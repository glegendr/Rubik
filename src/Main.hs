module Main where

import Moves
import Lexer
import System.Environment
import System.Exit
import System.Console.ANSI
import Control.Concurrent
import Cube


main :: IO ()
main = do
    args <- getArgs
    let lexed = lexMe args
    case head lexed of
        LexError str -> alert str
        otherwise -> return ()
    let shuffle = getShuffle lexed
    putStrLn $ show $ map toMove shuffle
    putAnimatedCube newCube [moveR, moveU, moveR', moveU']
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

putAnimatedCube :: Cube -> [(Cube -> Cube)] -> IO ()
putAnimatedCube cube [] = putCubeColor cube
putAnimatedCube cube (x:xs) = do
    putCubeColor cube
    cursorUpLine 15
    threadDelay 500000
    putAnimatedCube (x cube) xs