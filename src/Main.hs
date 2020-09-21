module Main where

import Moves
import Lexer
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    let lexed = lexMe args
    case head lexed of
        LexError str -> alert str
        otherwise -> return ()
    let shuffle = getShuffle lexed
    putStrLn $ show $ map toMove shuffle


alert :: String -> IO a
alert str = do
    putStrLn $ "Error: " ++ str 
    exitWith (ExitFailure 2)