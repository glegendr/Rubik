module Cube
( Cube
, newCube
, cubeToString
, putCube
, putCubeColor
, moveF
, moveF2
, moveF'
, moveR
, moveR2
, moveR'
, moveL
, moveL2
, moveL'
, moveU
, moveU2
, moveU'
, moveD
, moveD2
, moveD'
, moveB
, moveB2
, moveB'
) where

import Moves
import Data.List
import Data.Text (pack)
import Data.Function ((&))
import Rainbow
{--
   
   |OOO|    |         | O  O  W | 
   |O2O|    |         | O  O  W |
   |OOO|    |         | O  O  W |
---|---|--- | ------- | ------- | -------
GGG|WWW|012 | G  G  G | W  W  R | 2  5  8    
G1G|WWW|345 | G  G  G | W  W  R | 1  B  7  
GGG|WWW|678 | G  G  G | W  W  R | 0  3  6  
---|---|--- | ------- | ------- | -------
   |RRR|    |         | R  R  Y |
   |R4R|    |         | R  R  Y |
   |RRR|    |         | R  R  Y |
   |---|    |         | ------- |
   |YYY|    |         | Y  Y  O |
   |Y5Y|    |         | Y  Y  O |
   |YYY|    |         | Y  Y  O |
--}
type Cube = [String]

allMoves = ["F", "F\'", "F2", "R", "R\'", "R2", "U", "U\'", "U2", "B", "B\'", "B2", "L", "L\'", "L2", "D", "D\'", "D2"]

frontFace = 0
leftFace = 1
upFace = 2
rightFace = 3
downFace = 4
backFace = 5

newCube :: Cube
newCube = ["WWWWWWWWW", "GGGGGGGGG", "OOOOOOOOO", "BBBBBBBBB", "RRRRRRRRR", "YYYYYYYYY"]

cubeToString :: Cube -> String
cubeToString (ff:lf:uf:rf:df:bf:[]) =
    upPatron ++ centralBar ++ middlePatron ++ centralBar ++ downPatron1 ++ downBar ++ downPatron2 
    where
        centralBar = "------- | ------- | -------\n "
        downBar = "        | ------- |\n"
        upPatron = foldl1 (++) $ zipWith (++) ["         | ", " |\n         | ", " |\n         | "," |\n"] $ (map (intercalate "  ") $ cutBy3 uf) ++ [" "]
        downPatron1 = foldl1 (++) $ zipWith (++) ["        | ", " |\n         | ", " |\n         | "," |\n"] $ (map (intercalate "  ") $ cutBy3 df) ++ [" "]
        downPatron2 = reverse $ drop 1 $ reverse $ foldl1 (++) $ zipWith (++) ["         | ", " |\n         | ", " |\n         | "," |\n"] $ (map (intercalate "  ") $ cutBy3 bf) ++ [" "]
        face0 = map (intercalate "  ") $ cutBy3 ff
        face1 = map (intercalate "  ") $ cutBy3 lf
        face3 = map (intercalate "  ") $ cutBy3 rf
        middlePatron = foldl1 (++) $ zipWith5 (\ a b c d e -> a ++ d ++ b ++ d ++ c ++ e) face1 face0 face3 [" | ", " | ", " | "] ["\n ", "\n ", "\n "]
        cutBy3 :: String -> [[String]]
        cutBy3 [] = []
        cutBy3 (x:y:z:xs) = [[x], [y], [z]] : cutBy3 xs
cubeToString _ = []

putCube :: Cube -> IO ()
putCube = putStr . cubeToString

putCubeColor :: Cube -> IO ()
putCubeColor = putCubeColor' . cubeToString

putCubeColor' :: String -> IO ()
putCubeColor' [] = return ()
putCubeColor' (x:xs)
    | x == 'W' = do
        putChunk $ (chunk $ pack ['■']) & fore white & bold
        putCubeColor' xs
    | x == 'G' = do
        putChunk $ (chunk $ pack ['■']) & fore green & bold
        putCubeColor' xs
    | x == 'O' = do
        putChunk $ (chunk $ pack ['■']) & fore magenta & bold
        putCubeColor' xs
    | x == 'B' = do
        putChunk $ (chunk $ pack ['■']) & fore blue & bold
        putCubeColor' xs
    | x == 'R' = do
        putChunk $ (chunk $ pack ['■']) & fore red & bold
        putCubeColor' xs
    | x == 'Y' = do
        putChunk $ (chunk $ pack ['■']) & fore yellow & bold
        putCubeColor' xs
    | otherwise = do
        putChar x 
        putCubeColor' xs


moveF :: Cube -> Cube
moveF myCube = face0:face1:face2:face3:face4:(myCube !! backFace):[]
    where
        face0 = [myCube !! frontFace !! 6, myCube !! frontFace !! 3, myCube !! frontFace !! 0, myCube !! frontFace !! 7, myCube !! frontFace !! 4, myCube !! frontFace !! 1, myCube !! frontFace !! 8, myCube !! frontFace !! 5, myCube !! frontFace !! 2]
        face1 = [myCube !! leftFace !! 0, myCube !! leftFace !! 1, myCube !! downFace !! 0, myCube !! leftFace !! 3, myCube !! leftFace !! 4, myCube !! downFace !! 1, myCube !! leftFace !! 6, myCube !! leftFace !! 7, myCube !! downFace !! 2]
        face2 = take 6 (myCube !! upFace) ++ [myCube !! leftFace !! 2, myCube !! leftFace !! 5, myCube !! leftFace !! 8]
        face3 = [myCube !! upFace !! 6, myCube !! rightFace !! 1, myCube !! rightFace !! 2, myCube !! upFace !! 7, myCube !! rightFace !! 4, myCube !! rightFace !! 5, myCube !! upFace !! 8, myCube !! rightFace !! 7, myCube !! rightFace !! 8]
        face4 = [myCube !! rightFace !! 0, myCube !! rightFace !! 3, myCube !! rightFace !! 6] ++ drop 3 (myCube !! downFace)

moveF2 :: Cube -> Cube
moveF2 = moveF . moveF

moveF' :: Cube -> Cube
moveF' myCube = face0:face1:face2:face3:face4:(myCube !! backFace):[]
    where
        face0 = [myCube !! frontFace !! 2, myCube !! frontFace !! 5, myCube !! frontFace !! 8, myCube !! frontFace !! 1, myCube !! frontFace !! 4, myCube !! frontFace !! 7, myCube !! frontFace !! 0, myCube !! frontFace !! 3, myCube !! frontFace !! 6]
        face1 = [myCube !! leftFace !! 0, myCube !! leftFace !! 1, myCube !! upFace !! 8, myCube !! leftFace !! 3, myCube !! leftFace !! 4, myCube !! upFace !! 7, myCube !! leftFace !! 6, myCube !! leftFace !! 7, myCube !! upFace !! 6]
        face2 = take 6 (myCube !! upFace) ++ [myCube !! rightFace !! 0, myCube !! rightFace !! 3, myCube !! rightFace !! 6]
        face3 = [myCube !! downFace !! 2, myCube !! rightFace !! 1, myCube !! rightFace !! 2, myCube !! downFace !! 1, myCube !! rightFace !! 4, myCube !! rightFace !! 5, myCube !! downFace !! 0, myCube !! rightFace !! 7, myCube !! rightFace !! 8]
        face4 = [myCube !! leftFace !! 2, myCube !! leftFace !! 5, myCube !! leftFace !! 8] ++ drop 3 (myCube !! downFace)

moveR :: Cube -> Cube
moveR myCube = face0:(myCube !! leftFace):face2:face3:face4:face5:[]
        where
        face0 = [myCube !! frontFace !! 0, myCube !! frontFace !! 1, myCube !! downFace !! 2, myCube !! frontFace !! 3, myCube !! frontFace !! 4, myCube !! downFace !! 5, myCube !! frontFace !! 6, myCube !! frontFace !! 7, myCube !! downFace !! 8]
        face2 = [myCube !! upFace !! 0, myCube !! upFace !! 1, myCube !! frontFace !! 2, myCube !! upFace !! 3, myCube !! upFace !! 4, myCube !! frontFace !! 5, myCube !! upFace !! 6, myCube !! upFace !! 7, myCube !! frontFace !! 8]
        face3 = [myCube !! rightFace !! 6, myCube !! rightFace !! 3, myCube !! rightFace !! 0, myCube !! rightFace !! 7, myCube !! rightFace !! 4, myCube !! rightFace !! 1, myCube !! rightFace !! 8, myCube !! rightFace !! 5, myCube !! rightFace !! 2]
        face4 = [myCube !! downFace !! 0, myCube !! downFace !! 1, myCube !! backFace !! 2, myCube !! downFace !! 3, myCube !! downFace !! 4, myCube !! backFace !! 5, myCube !! downFace !! 6, myCube !! downFace !! 7, myCube !! backFace !! 8]
        face5 = [myCube !! backFace !! 0, myCube !! backFace !! 1, myCube !! upFace !! 2, myCube !! backFace !! 3, myCube !! backFace !! 4, myCube !! upFace !! 5, myCube !! backFace !! 6, myCube !! backFace !! 7, myCube !! upFace !! 8]

moveR2 :: Cube -> Cube
moveR2 = moveR . moveR

moveR' :: Cube -> Cube
moveR' myCube = face0:(myCube !! leftFace):face2:face3:face4:face5:[]
        where
        face0 = [myCube !! frontFace !! 0, myCube !! frontFace !! 1, myCube !! upFace !! 2, myCube !! frontFace !! 3, myCube !! frontFace !! 4, myCube !! upFace !! 5, myCube !! frontFace !! 6, myCube !! frontFace !! 7, myCube !! upFace !! 8]
        face2 = [myCube !! upFace !! 0, myCube !! upFace !! 1, myCube !! backFace !! 2, myCube !! upFace !! 3, myCube !! upFace !! 4, myCube !! backFace !! 5, myCube !! upFace !! 6, myCube !! upFace !! 7, myCube !! backFace !! 8]
        face3 = [myCube !! rightFace !! 2, myCube !! rightFace !! 5, myCube !! rightFace !! 8, myCube !! rightFace !! 1, myCube !! rightFace !! 4, myCube !! rightFace !! 7, myCube !! rightFace !! 0, myCube !! rightFace !! 3, myCube !! rightFace !! 6]
        face4 = [myCube !! downFace !! 0, myCube !! downFace !! 1, myCube !! frontFace !! 2, myCube !! downFace !! 3, myCube !! downFace !! 4, myCube !! frontFace !! 5, myCube !! downFace !! 6, myCube !! downFace !! 7, myCube !! frontFace !! 8]
        face5 = [myCube !! backFace !! 0, myCube !! backFace !! 1, myCube !! downFace !! 2, myCube !! backFace !! 3, myCube !! backFace !! 4, myCube !! downFace !! 5, myCube !! backFace !! 6, myCube !! backFace !! 7, myCube !! downFace !! 8]

moveL :: Cube -> Cube
moveL myCube =  face0:face1:face2:(myCube !! rightFace):face4:face5:[]
    where
        face0 = [myCube !! upFace !! 0, myCube !! frontFace !! 1, myCube !! frontFace !! 2, myCube !! upFace !! 3, myCube !! frontFace !! 4, myCube !! frontFace !! 5, myCube !! upFace !! 6, myCube !! frontFace !! 7, myCube !! frontFace !! 8]
        face1 = [myCube !! leftFace !! 6, myCube !! leftFace !! 3, myCube !! leftFace !! 0, myCube !! leftFace !! 7, myCube !! leftFace !! 4, myCube !! leftFace !! 1, myCube !! leftFace !! 8, myCube !! leftFace !! 5, myCube !! leftFace !! 2]
        face2 = [myCube !! backFace !! 8, myCube !! upFace !! 1, myCube !! upFace !! 2, myCube !! backFace !! 5, myCube !! upFace !! 4, myCube !! upFace !! 5, myCube !! backFace !! 2, myCube !! upFace !! 7, myCube !! upFace !! 8]
        face4 = [myCube !! frontFace !! 0, myCube !! downFace !! 1, myCube !! downFace !! 2, myCube !! frontFace !! 3, myCube !! downFace !! 4, myCube !! downFace !! 5, myCube !! frontFace !! 6, myCube !! downFace !! 7, myCube !! downFace !! 8]
        face5 = [myCube !! downFace !! 0, myCube !! backFace !! 1, myCube !! backFace !! 2, myCube !! downFace !! 3, myCube !! backFace !! 4, myCube !! backFace !! 5, myCube !! downFace !! 6, myCube !! backFace !! 7, myCube !! backFace !! 8]

moveL2 :: Cube -> Cube
moveL2 = moveL . moveL

moveL' :: Cube -> Cube
moveL' myCube = face0:face1:face2:(myCube !! rightFace):face4:face5:[]
    where
        face0 = [myCube !! downFace !! 0, myCube !! frontFace !! 1, myCube !! frontFace !! 2, myCube !! downFace !! 3, myCube !! frontFace !! 4, myCube !! frontFace !! 5, myCube !! downFace !! 6, myCube !! frontFace !! 7, myCube !! frontFace !! 8]
        face1 = [myCube !! leftFace !! 2, myCube !! leftFace !! 5, myCube !! leftFace !! 8, myCube !! leftFace !! 1, myCube !! leftFace !! 4, myCube !! leftFace !! 7, myCube !! leftFace !! 0, myCube !! leftFace !! 3, myCube !! leftFace !! 6]
        face2 = [myCube !! frontFace !! 0, myCube !! upFace !! 1, myCube !! upFace !! 2, myCube !! frontFace !! 3, myCube !! upFace !! 4, myCube !! upFace !! 5, myCube !! frontFace !! 6, myCube !! upFace !! 7, myCube !! upFace !! 8]
        face4 = [myCube !! backFace !! 8, myCube !! downFace !! 1, myCube !! downFace !! 2, myCube !! backFace !! 5, myCube !! downFace !! 4, myCube !! downFace !! 5, myCube !! backFace !! 2, myCube !! downFace !! 7, myCube !! downFace !! 8]
        face5 = [myCube !! upFace !! 0, myCube !! backFace !! 1, myCube !! backFace !! 2, myCube !! upFace !! 3, myCube !! backFace !! 4, myCube !! backFace !! 5, myCube !! upFace !! 6, myCube !! backFace !! 7, myCube !! backFace !! 8]

moveU :: Cube -> Cube
moveU myCube = face0:face1:face2:face3:(myCube !! downFace):face5:[]
    where
        face0 = [myCube !! rightFace !! 0, myCube !! rightFace !! 1, myCube !! rightFace !! 2, myCube !! frontFace !! 3, myCube !! frontFace !! 4, myCube !! frontFace !! 5, myCube !! frontFace !! 6, myCube !! frontFace !! 7, myCube !! frontFace !! 8]
        face1 = [myCube !! frontFace !! 0, myCube !! frontFace !! 1, myCube !! frontFace !! 2, myCube !! leftFace !! 3, myCube !! leftFace !! 4, myCube !! leftFace !! 5, myCube !! leftFace !! 6, myCube !! leftFace !! 7, myCube !! leftFace !! 8]
        face2 = [myCube !! upFace !! 6,myCube !! upFace !! 3,myCube !! upFace !! 0,myCube !! upFace !! 7,myCube !! upFace !! 4,myCube !! upFace !! 1,myCube !! upFace !! 8,myCube !! upFace !! 5,myCube !! upFace !! 2]
        face3 = [myCube !! backFace !! 8, myCube !! backFace !! 7, myCube !! backFace !! 6, myCube !! rightFace !! 3, myCube !! rightFace !! 4, myCube !! rightFace !! 5, myCube !! rightFace !! 6, myCube !! rightFace !! 7, myCube !! rightFace !! 8]
        face5 = [myCube !! backFace !! 0, myCube !! backFace !! 1, myCube !! backFace !! 2, myCube !! backFace !! 3, myCube !! backFace !! 4, myCube !! backFace !! 5, myCube !! leftFace !! 2, myCube !! leftFace !! 1, myCube !! leftFace !! 0]

moveU2 :: Cube -> Cube
moveU2 = moveU . moveU

moveU' :: Cube -> Cube
moveU' myCube = face0:face1:face2:face3:(myCube !! downFace):face5:[]
    where
        face0 = [myCube !! leftFace !! 0, myCube !! leftFace !! 1, myCube !! leftFace !! 2, myCube !! frontFace !! 3, myCube !! frontFace !! 4, myCube !! frontFace !! 5, myCube !! frontFace !! 6, myCube !! frontFace !! 7, myCube !! frontFace !! 8]
        face1 = [myCube !! backFace !! 8, myCube !! backFace !! 7, myCube !! backFace !! 6, myCube !! leftFace !! 3, myCube !! leftFace !! 4, myCube !! leftFace !! 5, myCube !! leftFace !! 6, myCube !! leftFace !! 7, myCube !! leftFace !! 8]
        face2 = [myCube !! upFace !! 2, myCube !! upFace !! 5, myCube !! upFace !! 8, myCube !! upFace !! 1, myCube !! upFace !! 4, myCube !! upFace !! 7, myCube !! upFace !! 0, myCube !! upFace !! 3, myCube !! upFace !! 6]
        face3 = [myCube !! frontFace !! 0, myCube !! frontFace !! 1, myCube !! frontFace !! 2, myCube !! rightFace !! 3, myCube !! rightFace !! 4, myCube !! rightFace !! 5, myCube !! rightFace !! 6, myCube !! rightFace !! 7, myCube !! rightFace !! 8]
        face5 = [myCube !! backFace !! 0, myCube !! backFace !! 1, myCube !! backFace !! 2, myCube !! backFace !! 3, myCube !! backFace !! 4, myCube !! backFace !! 5, myCube !! rightFace !! 2, myCube !! rightFace !! 1, myCube !! rightFace !! 0]

moveD :: Cube -> Cube
moveD myCube = face0:face1:(myCube !! upFace):face3:face4:face5:[]
    where
        face0 = [myCube !! frontFace !! 0, myCube !! frontFace !! 1, myCube !! frontFace !! 2, myCube !! frontFace !! 3, myCube !! frontFace !! 4, myCube !! frontFace !! 5, myCube !! leftFace !! 6, myCube !! leftFace !! 7, myCube !! leftFace !! 8]
        face1 = [myCube !! leftFace !! 0, myCube !! leftFace !! 1, myCube !! leftFace !! 2, myCube !! leftFace !! 3, myCube !! leftFace !! 4, myCube !! leftFace !! 5, myCube !! backFace !! 2, myCube !! backFace !! 1, myCube !! backFace !! 0]
        face4 = [myCube !! downFace !! 6, myCube !! downFace !! 3, myCube !! downFace !! 0, myCube !! downFace !! 7, myCube !! downFace !! 4, myCube !! downFace !! 1, myCube !! downFace !! 8, myCube !! downFace !! 5, myCube !! downFace !! 2]
        face3 = [myCube !! rightFace !! 0, myCube !! rightFace !! 1, myCube !! rightFace !! 2, myCube !! rightFace !! 3, myCube !! rightFace !! 4, myCube !! rightFace !! 5, myCube !! frontFace !! 6, myCube !! frontFace !! 7, myCube !! frontFace !! 8]
        face5 = [myCube !! rightFace !! 8, myCube !! rightFace !! 7, myCube !! rightFace !! 6, myCube !! backFace !! 3, myCube !! backFace !! 4, myCube !! backFace !! 5, myCube !! backFace !! 6, myCube !! backFace !! 7, myCube !! backFace !! 8]

moveD2 :: Cube -> Cube
moveD2 = moveD . moveD

moveD' :: Cube -> Cube
moveD' myCube = face0:face1:(myCube !! upFace):face3:face4:face5:[]
    where
        face0 = [myCube !! frontFace !! 0, myCube !! frontFace !! 1, myCube !! frontFace !! 2, myCube !! frontFace !! 3, myCube !! frontFace !! 4, myCube !! frontFace !! 5, myCube !! rightFace !! 6, myCube !! rightFace !! 7, myCube !! rightFace !! 8]
        face1 = [myCube !! leftFace !! 0, myCube !! leftFace !! 1, myCube !! leftFace !! 2, myCube !! leftFace !! 3, myCube !! leftFace !! 4, myCube !! leftFace !! 5, myCube !! frontFace !! 6, myCube !! frontFace !! 7, myCube !! frontFace !! 8]
        face3 = [myCube !! rightFace !! 0, myCube !! rightFace !! 1, myCube !! rightFace !! 2, myCube !! rightFace !! 3, myCube !! rightFace !! 4, myCube !! rightFace !! 5, myCube !! backFace !! 2, myCube !! backFace !! 1, myCube !! backFace !! 0]
        face4 = [myCube !! downFace !! 2, myCube !! downFace !! 5, myCube !! downFace !! 8, myCube !! downFace !! 1, myCube !! downFace !! 4, myCube !! downFace !! 7, myCube !! downFace !! 0, myCube !! downFace !! 3, myCube !! downFace !! 6]
        face5 = [myCube !! leftFace !! 8, myCube !! leftFace !! 7, myCube !! leftFace !! 6, myCube !! backFace !! 3, myCube !! backFace !! 4, myCube !! backFace !! 5, myCube !! backFace !! 6, myCube !! backFace !! 7, myCube !! backFace !! 8]

moveB :: Cube -> Cube
moveB myCube = (myCube !! frontFace):face1:face2:face3:face4:face5:[]
    where
        face1 = [myCube !! upFace !! 2, myCube !! leftFace !! 1, myCube !! leftFace !! 2, myCube !! upFace !! 1, myCube !! leftFace !! 4, myCube !! leftFace !! 5, myCube !! upFace !! 0, myCube !! leftFace !! 7, myCube !! leftFace !! 8]
        face2 = [myCube !! rightFace !! 2, myCube !! rightFace !! 5, myCube !! rightFace !! 8, myCube !! upFace !! 3, myCube !! upFace !! 4, myCube !! upFace !! 5, myCube !! upFace !! 6, myCube !! upFace !! 7, myCube !! upFace !! 8]
        face3 = [myCube !! rightFace !! 0, myCube !! rightFace !! 1, myCube !! downFace !! 8, myCube !! rightFace !! 3, myCube !! rightFace !! 4, myCube !! downFace !! 7, myCube !! rightFace !! 6, myCube !! rightFace !! 7, myCube !! downFace !! 6]
        face4 = [myCube !! downFace !! 0, myCube !! downFace !! 1, myCube !! downFace !! 2, myCube !! downFace !! 3, myCube !! downFace !! 4, myCube !! downFace !! 5, myCube !! leftFace !! 0, myCube !! leftFace !! 3, myCube !! leftFace !! 6]
        face5 = [myCube !! backFace !! 6, myCube !! backFace !! 3, myCube !! backFace !! 0, myCube !! backFace !! 7, myCube !! backFace !! 4, myCube !! backFace !! 1, myCube !! backFace !! 8, myCube !! backFace !! 5, myCube !! backFace !! 2]

moveB2 :: Cube -> Cube
moveB2 = moveB . moveB

moveB' :: Cube -> Cube
moveB' myCube = (myCube !! frontFace):face1:face2:face3:face4:face5:[]
    where
        face1 = [myCube !! downFace !! 0, myCube !! leftFace !! 1, myCube !! leftFace !! 2, myCube !! downFace !! 3, myCube !! leftFace !! 4, myCube !! leftFace !! 5, myCube !! downFace !! 6, myCube !! leftFace !! 7, myCube !! leftFace !! 8]
        face2 = [myCube !! leftFace !! 0, myCube !! leftFace !! 3, myCube !! leftFace !! 6, myCube !! upFace !! 3, myCube !! upFace !! 4, myCube !! upFace !! 5, myCube !! upFace !! 6, myCube !! upFace !! 7, myCube !! upFace !! 8]
        face3 = [myCube !! rightFace !! 0, myCube !! rightFace !! 1, myCube !! upFace !! 0, myCube !! rightFace !! 3, myCube !! rightFace !! 4, myCube !! upFace !! 1, myCube !! rightFace !! 6, myCube !! rightFace !! 7, myCube !! upFace !! 2]
        face4 = [myCube !! downFace !! 0, myCube !! downFace !! 1, myCube !! downFace !! 2, myCube !! downFace !! 3, myCube !! downFace !! 4, myCube !! downFace !! 5, myCube !! rightFace !! 8, myCube !! rightFace !! 5, myCube !! rightFace !! 2]
        face5 = [myCube !! backFace !! 2, myCube !! backFace !! 5, myCube !! backFace !! 8, myCube !! backFace !! 1, myCube !! backFace !! 4, myCube !! backFace !! 7, myCube !! backFace !! 0, myCube !! backFace !! 3, myCube !! backFace !! 6]
