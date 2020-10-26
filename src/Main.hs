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
import Data.Bits


main :: IO ()
main = do
    args <- getArgs
    let lexed = lexMe args
    case head lexed of
        LexError str -> alert str
        otherwise -> return ()
    let shuffle = getShuffle lexed
    let shuffeledCube = foldl (\ cube f -> f cube) newCube $ map moveToAction $ map toMove shuffle
    putCubeColor shuffeledCube
    let ret = algo shuffeledCube
    putStrLn $ show $ length ret
    putMoves ret
    putCubeColor $ makeMoves ret newCube
    -- putStrLn $ show newCube'
    -- putStrLn $ show $ foldl (\acc f -> f acc) newCube' (take 600000 $ repeat (applyMove 0)) 
    -- putStrLn $ show $ makeMoves (take 2000000000 (repeat (MUp ONothing))) newCube
    ----------------
    -- putAnimatedCube False shuffeledCube (map moveToAction ret)
    --putInteractive newCube
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
    cursorUpLine 16
    putCubeColor movedCube
    putInteractive movedCube


-- Move		int	-> inverse int
-- U		0	-> 2
-- U2		1	-> 1
-- U'		2	-> 0
-- D		3	-> 5
-- D2		4	-> 4
-- D'		5	-> 3
-- F		6	-> 8
-- F2		7	-> 7
-- F'		8	-> 6
-- B		9	-> 11
-- B2		10	-> 10
-- B'		11	-> 9
-- L		12	-> 14
-- L2		13	-> 13
-- L'		14	-> 12
-- R		15	-> 17
-- R2		16	-> 16
-- R'		17	-> 15
--

affectedCubies :: [[Int]]
affectedCubies = [[0, 1, 2, 3, 0, 1, 2, 3],[4, 7, 6, 5, 4, 5, 6, 7],[0, 9, 4, 8,0, 3, 5, 4],[2, 10, 6, 11, 2, 1, 7, 6],[3, 11, 7, 9, 3, 2, 6, 5],[1, 8, 5, 10, 1,0, 4, 7]]

-- private static readonly int[,] AFFECTED_CUBIES =
-- {
-- 	{  0,  1,  2,  3,  0,  1,  2,  3 },   // U
-- 	{  4,  7,  6,  5,  4,  5,  6,  7 },   // D
-- 	{  0,  9,  4,  8,  0,  3,  5,  4 },   // F
-- 	{  2, 10,  6, 11,  2,  1,  7,  6 },   // B
-- 	{  3, 11,  7,  9,  3,  2,  6,  5 },   // L
-- 	{  1,  8,  5, 10,  1,  0,  4,  7 },   // R
-- };


-- private static readonly int[] GOAL_STATE = {
-- 	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
-- 	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
-- };

newCube' :: [Int]
newCube' = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

applyMove :: Int -> [Int] -> [Int]
applyMove move cube =
    applyMove' turn face cube
    where
        turn = (mod move 3) + 1
        face = div move 3

applyMove' 0 _ cube = cube
applyMove' turn face cube = applyMove' (turn - 1) face (applyMove'' 0 face cube cube turn)

applyMove'' 8 _ _ cube _ = cube
applyMove'' index face oldCube cube turn =
    let isCorner = boolToInt (index > 3)
        target = affectedCubies !! face !! index + isCorner * 12
        killer = affectedCubies !! face !! (if (index .&. 3) == 3 then index - 3 else index + 1) + isCorner * 12
        orientationDelta = if (index < 4) then boolToInt (face > 1 && face < 4) else if (face < 2) then 0 else 2 - (index .&. 1)
        tmpCube = buildMe (target + 20) (oldCube !! (killer + 20) + orientationDelta) $ buildMe target (oldCube !! killer) cube
        retCube = if turn == 0 then buildMe (target + 20) (mod (tmpCube !! (target + 20)) 2 + isCorner) tmpCube else tmpCube
    in applyMove'' (index + 1) face oldCube retCube turn

buildMe :: Int -> Int -> [Int] -> [Int]
buildMe index replace cube = take (index - 1) cube ++ [replace] ++ drop index cube 

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0 

-- private int[] ApplyMove(int move, int[] state)
-- {
-- 	int turns = move % 3 + 1;
-- 	int face = move / 3;
-- 	state = (int[]) state.Clone();
-- 	while (turns-- > 0)
-- 	{
-- 		int[] oldState = (int[]) state.Clone();
-- 		for (int i = 0; i < 8; i++)
-- 		{
-- 			int isCorner = Convert.ToInt32(i > 3);
-- 			int target = AFFECTED_CUBIES[face, i] + isCorner * 12;
-- 			int killer = AFFECTED_CUBIES[face, (i & 3) == 3 ? i - 3 : i + 1] + isCorner * 12; ;
-- 			int orientationDelta = (i < 4) ? Convert.ToInt32(face > 1 && face < 4) :
-- 				(face < 2) ? 0 : 2 - (i & 1);
-- 			state[target] = oldState[killer];
-- 			state[target + 20] = oldState[killer + 20] + orientationDelta;
-- 			if (turns == 0)
-- 				state[target + 20] %= 2 + isCorner;
-- 		}
-- 	}
-- 	return state;
-- }
