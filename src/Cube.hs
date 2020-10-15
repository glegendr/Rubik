module Cube
( Cube
, Face
, get
, colors
, faceToList
, allTrueMoves
, allTrueMovesKey
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
, frontFace
, leftFace
, upFace
, rightFace
, downFace
, backFace
, moveToAction
) where

import Moves
import Data.List
import Data.Text (pack)
import Data.Function ((&))
import Rainbow
import Data.Bits

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
type Face = Int
type Cube = [Face]

allMoves = ["F", "F\'", "F2", "R", "R\'", "R2", "U", "U\'", "U2", "B", "B\'", "B2", "L", "L\'", "L2", "D", "D\'", "D2"]

frontFace = 0
leftFace = 1
upFace = 2
rightFace = 3
downFace = 4
backFace = 5

{--
    012
    345  -> 8 7 6 5 3 2 1 0
    678

white = 0
green = 1
orange = 2
blue = 3
red = 4
yellow = 5
--}

newCube :: Cube
newCube = [0, 286331153, 572662306, 858993459, 1145324612, 1431655765]
colors = ['W', 'G', 'O', 'B', 'R', 'Y']

faceToList :: Face -> [Int]
faceToList = faceToList' 8

faceToList' :: Int -> Face -> [Int]
faceToList' 0 _ = []
faceToList' count x = x .&. 0xf : faceToList' (count - 1) (shiftR x 4)

cubeToString :: Cube -> String
cubeToString = cubeToString' . addCenter 0 . changeToString

addCenter :: Int -> [String] -> [String]
addCenter _ [] = []
addCenter color (x:xs) = (bef ++ [colors !! color] ++ aft) : addCenter (color + 1) xs
    where (bef, aft) = splitAt 4 x

changeToString :: Cube -> [String]
changeToString [] = []
changeToString (x:xs) = changeToString' 8 x : changeToString xs

changeToString' :: Int -> Int -> String
changeToString' 0 _ = []
changeToString' count x = colors !! (x .&. 0xf) : changeToString' (count - 1) (shiftR x 4)

cubeToString' :: [String] -> String
cubeToString' (ff:lf:uf:rf:df:bf:[]) =
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
cubeToString' _ = []

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


buildFace :: [Int] -> Int
buildFace = buildFace' 0

buildFace' :: Int -> [Int] -> Int
buildFace' ret [] = ret
buildFace' ret (x:xs) = buildFace' ((shiftL ret 4) + x) xs

get :: Int -> Int -> Int
get face nb = shiftR (face .&. (shiftL 0xf (nb * 4))) (4 * nb)

-- 0001 0010 1101 0110 1101 0011

moveF :: Cube -> Cube
moveF myCube = face0:face1:face2:face3:face4:(myCube !! backFace):[]
    where
        
        face0 = buildFace [get (myCube !! frontFace) 2,get (myCube !! frontFace) (5 - 1),get (myCube !! frontFace) (8 - 1),get (myCube !! frontFace) 1,get (myCube !! frontFace) (7 - 1),get (myCube !! frontFace) 0,get (myCube !! frontFace) 3,get (myCube !! frontFace) (6 - 1)]
        face1 = buildFace [get (myCube !! downFace) 2,get (myCube !! leftFace) (7 - 1),get (myCube !! leftFace) (6 - 1),get (myCube !! downFace) 1,get (myCube !! leftFace) 3,get (myCube !! downFace) 0,get (myCube !! leftFace) 1,get (myCube !! leftFace) 0]
        face2 = ((myCube !! upFace) .&. 0xfffff) + (shiftL (buildFace [get (myCube !! leftFace) 2,get (myCube !! leftFace) (5 - 1),get (myCube !! leftFace) (8 - 1)]) (5 * 4))
        face3 = buildFace [get (myCube !! rightFace) 7,get (myCube !! rightFace) (7 - 1),get (myCube !! upFace) (8 - 1),get (myCube !! rightFace) (5 - 1),get (myCube !! upFace) (7 - 1),get (myCube !! rightFace) 2,get (myCube !! rightFace) 1,get (myCube !! upFace) (6 - 1)]
        face4 = (buildFace [get (myCube !! rightFace) 0,get (myCube !! rightFace) 3,get (myCube !! rightFace) (6 - 1)]) + ((myCube !! downFace) .&. 0xfffff000)

moveF2 :: Cube -> Cube
moveF2 = moveF . moveF

moveF' :: Cube -> Cube
moveF' myCube = face0:face1:face2:face3:face4:(myCube !! backFace):[]
    where
        face0 = buildFace [get (myCube !! frontFace) 6,get (myCube !! frontFace) 3,get (myCube !! frontFace) 0,get (myCube !! frontFace) (7 - 1),get (myCube !! frontFace) 1,get (myCube !! frontFace) (8 - 1),get (myCube !! frontFace) (5 - 1),get (myCube !! frontFace) 2]
        face1 = buildFace [get (myCube !! upFace) 6,get (myCube !! leftFace) (7 - 1),get (myCube !! leftFace) (6 - 1),get (myCube !! upFace) (7 - 1),get (myCube !! leftFace) 3,get (myCube !! upFace) (8 - 1),get (myCube !! leftFace) 1,get (myCube !! leftFace) 0]
        face2 = ((myCube !! upFace) .&. 0xfffff) + (shiftL (buildFace [get (myCube !! rightFace) 6,get (myCube !! rightFace) 3,get (myCube !! rightFace) 0]) (5 * 4))
        face3 = buildFace [get (myCube !! rightFace) 7,get (myCube !! rightFace) (7 - 1),get (myCube !! downFace) 0,get (myCube !! rightFace) (5 - 1),get (myCube !! downFace) 1,get (myCube !! rightFace) 2,get (myCube !! rightFace) 1,get (myCube !! downFace) 2]
        face4 = (buildFace [get (myCube !! leftFace) 7,get (myCube !! leftFace) (5 - 1),get (myCube !! leftFace) 2]) + ((myCube !! downFace) .&. 0xfffff000)

moveR :: Cube -> Cube
moveR myCube = face0:(myCube !! leftFace):face2:face3:face4:face5:[]
        where
        face0 = buildFace [get (myCube !! downFace) 7,get (myCube !! frontFace) (7 - 1),get (myCube !! frontFace) (6 - 1),get (myCube !! downFace) (5 - 1),get (myCube !! frontFace) 3,get (myCube !! downFace) 2,get (myCube !! frontFace) 1,get (myCube !! frontFace) 0]
        face2 = buildFace [get (myCube !! frontFace) 7,get (myCube !! upFace) (7 - 1),get (myCube !! upFace) (6 - 1),get (myCube !! frontFace) (5 - 1),get (myCube !! upFace) 3,get (myCube !! frontFace) 2,get (myCube !! upFace) 1,get (myCube !! upFace) 0]
        face3 = buildFace[get (myCube !! rightFace) 2,get (myCube !! rightFace) (5 - 1),get (myCube !! rightFace) (8 - 1),get (myCube !! rightFace) 1,get (myCube !! rightFace) (7 - 1),get (myCube !! rightFace) 0,get (myCube !! rightFace) 3,get (myCube !! rightFace) (6 - 1)]
        face4 = buildFace [get (myCube !! backFace) 7,get (myCube !! downFace) (7 - 1),get (myCube !! downFace) (6 - 1),get (myCube !! backFace) (5 - 1),get (myCube !! downFace) 3,get (myCube !! backFace) 2,get (myCube !! downFace) 1,get (myCube !! downFace) 0]
        face5 = buildFace [get (myCube !! upFace) 7,get (myCube !! backFace) (7 - 1),get (myCube !! backFace) (6 - 1),get (myCube !! upFace) (5 - 1),get (myCube !! backFace) 3,get (myCube !! upFace) 2,get (myCube !! backFace) 1,get (myCube !! backFace) 0]

moveR2 :: Cube -> Cube
moveR2 = moveR . moveR

moveR' :: Cube -> Cube
moveR' myCube = face0:(myCube !! leftFace):face2:face3:face4:face5:[]
        where
        face0 = buildFace [get (myCube !! upFace) 7,get (myCube !! frontFace) (7 - 1),get (myCube !! frontFace) (6 - 1),get (myCube !! upFace) (5 - 1),get (myCube !! frontFace) 3,get (myCube !! upFace) 2,get (myCube !! frontFace) 1,get (myCube !! frontFace) 0]
        face2 = buildFace [get (myCube !! backFace) 7,get (myCube !! upFace) (7 - 1),get (myCube !! upFace) (6 - 1),get (myCube !! backFace) (5 - 1),get (myCube !! upFace) 3,get (myCube !! backFace) 2,get (myCube !! upFace) 1,get (myCube !! upFace) 0]
        face3 = buildFace [get (myCube !! rightFace) 6,get (myCube !! rightFace) 3,get (myCube !! rightFace) 0,get (myCube !! rightFace) (7 - 1),get (myCube !! rightFace) 1,get (myCube !! rightFace) (8 - 1),get (myCube !! rightFace) (5 - 1),get (myCube !! rightFace) 2]
        face4 = buildFace [get (myCube !! frontFace) 7,get (myCube !! downFace) (7 - 1),get (myCube !! downFace) (6 - 1),get (myCube !! frontFace) (5 - 1),get (myCube !! downFace) 3,get (myCube !! frontFace) 2,get (myCube !! downFace) 1,get (myCube !! downFace) 0]
        face5 = buildFace [get (myCube !! downFace) 7,get (myCube !! backFace) (7 - 1),get (myCube !! backFace) (6 - 1),get (myCube !! downFace) (5 - 1),get (myCube !! backFace) 3,get (myCube !! downFace) 2,get (myCube !! backFace) 1,get (myCube !! backFace) 0]

moveL :: Cube -> Cube
moveL myCube =  face0:face1:face2:(myCube !! rightFace):face4:face5:[]
    where
        face0 = buildFace [get (myCube !! frontFace) 7,get (myCube !! frontFace) (7 - 1),get (myCube !! upFace) (6 - 1),get (myCube !! frontFace) (5 - 1),get (myCube !! upFace) 3,get (myCube !! frontFace) 2,get (myCube !! frontFace) 1,get (myCube !! upFace) 0]
        face1 = buildFace [get (myCube !! leftFace) 2,get (myCube !! leftFace) (5 - 1),get (myCube !! leftFace) (8 - 1),get (myCube !! leftFace) 1,get (myCube !! leftFace) (7 - 1),get (myCube !! leftFace) 0,get (myCube !! leftFace) 3,get (myCube !! leftFace) (6 - 1)]
        face2 = buildFace [get (myCube !! upFace) 7,get (myCube !! upFace) (7 - 1),get (myCube !! backFace) (6 - 1),get (myCube !! upFace) (5 - 1),get (myCube !! backFace) 3,get (myCube !! upFace) 2,get (myCube !! upFace) 1,get (myCube !! backFace) 0]
        face4 = buildFace [get (myCube !! downFace) 7,get (myCube !! downFace) (7 - 1),get (myCube !! frontFace) (6 - 1),get (myCube !! downFace) (5 - 1),get (myCube !! frontFace) 3,get (myCube !! downFace) 2,get (myCube !! downFace) 1,get (myCube !! frontFace) 0]
        face5 = buildFace [get (myCube !! backFace) 7,get (myCube !! backFace) (7 - 1),get (myCube !! downFace) (6 - 1),get (myCube !! backFace) (5 - 1),get (myCube !! downFace) 3,get (myCube !! backFace) 2,get (myCube !! backFace) 1,get (myCube !! downFace) 0]

moveL2 :: Cube -> Cube
moveL2 = moveL . moveL

moveL' :: Cube -> Cube
moveL' myCube = face0:face1:face2:(myCube !! rightFace):face4:face5:[]
    where
        face0 = buildFace [get (myCube !! frontFace) 7,get (myCube !! frontFace) (7 - 1),get (myCube !! downFace) (6 - 1),get (myCube !! frontFace) (5 - 1),get (myCube !! downFace) 3,get (myCube !! frontFace) 2,get (myCube !! frontFace) 1,get (myCube !! downFace) 0]
        face1 = buildFace [get (myCube !! leftFace) 6,get (myCube !! leftFace) 3,get (myCube !! leftFace) 0,get (myCube !! leftFace) (7 - 1),get (myCube !! leftFace) 1,get (myCube !! leftFace) (8 - 1),get (myCube !! leftFace) (5 - 1),get (myCube !! leftFace) 2]
        face2 = buildFace [get (myCube !! upFace) 7,get (myCube !! upFace) (7 - 1),get (myCube !! frontFace) (6 - 1),get (myCube !! upFace) (5 - 1),get (myCube !! frontFace) 3,get (myCube !! upFace) 2,get (myCube !! upFace) 1,get (myCube !! frontFace) 0]
        face4 = buildFace [get (myCube !! downFace) 7,get (myCube !! downFace) (7 - 1),get (myCube !! backFace) (6 - 1),get (myCube !! downFace) (5 - 1),get (myCube !! backFace) 3,get (myCube !! downFace) 2,get (myCube !! downFace) 1,get (myCube !! backFace) 0]
        face5 = buildFace[get (myCube !! backFace) 7,get (myCube !! backFace) (7 - 1),get (myCube !! upFace) (6 - 1),get (myCube !! backFace) (5 - 1),get (myCube !! upFace) 3,get (myCube !! backFace) 2,get (myCube !! backFace) 1,get (myCube !! upFace) 0]

moveU :: Cube -> Cube
moveU myCube = face0:face1:face2:face3:(myCube !! downFace):face5:[]
    where
        face0 = buildFace [get (myCube !! frontFace) 7,get (myCube !! frontFace) (7 - 1),get (myCube !! frontFace) (6 - 1),get (myCube !! frontFace) (5 - 1),get (myCube !! frontFace) 3,get (myCube !! rightFace) 2,get (myCube !! rightFace) 1,get (myCube !! rightFace) 0]
        face1 = buildFace [get (myCube !! leftFace) 7,get (myCube !! leftFace) (7 - 1),get (myCube !! leftFace) (6 - 1),get (myCube !! leftFace) (5 - 1),get (myCube !! leftFace) 3,get (myCube !! frontFace) 2,get (myCube !! frontFace) 1,get (myCube !! frontFace) 0]
        face2 = buildFace [get (myCube !! upFace) 2,get (myCube !! upFace) (5 - 1),get (myCube !! upFace) (8 - 1),get (myCube !! upFace) 1,get (myCube !! upFace) (7 - 1),get (myCube !! upFace) 0,get (myCube !! upFace) 3,get (myCube !! upFace) (6 - 1)]
        face3 = buildFace [get (myCube !! rightFace) 7,get (myCube !! rightFace) (7 - 1),get (myCube !! rightFace) (6 - 1),get (myCube !! rightFace) (5 - 1),get (myCube !! rightFace) 3,get (myCube !! backFace) (6 - 1),get (myCube !! backFace) (7 - 1),get (myCube !! backFace) (8 - 1)]
        face5 = buildFace [get (myCube !! leftFace) 0,get (myCube !! leftFace) 1,get (myCube !! leftFace) 2,get (myCube !! backFace) (5 - 1),get (myCube !! backFace) 3,get (myCube !! backFace) 2,get (myCube !! backFace) 1,get (myCube !! backFace) 0]   

moveU2 :: Cube -> Cube
moveU2 = moveU . moveU

moveU' :: Cube -> Cube
moveU' myCube = face0:face1:face2:face3:(myCube !! downFace):face5:[]
    where
        face0 = buildFace [get (myCube !! frontFace) 7,get (myCube !! frontFace) (7 - 1),get (myCube !! frontFace) (6 - 1),get (myCube !! frontFace) (5 - 1),get (myCube !! frontFace) 3,get (myCube !! leftFace) 2,get (myCube !! leftFace) 1,get (myCube !! leftFace) 0]
        face1 = buildFace [get (myCube !! leftFace) 7,get (myCube !! leftFace) (7 - 1),get (myCube !! leftFace) (6 - 1),get (myCube !! leftFace) (5 - 1),get (myCube !! leftFace) 3,get (myCube !! backFace) (6 - 1),get (myCube !! backFace) (7 - 1),get (myCube !! backFace) (8 - 1)]
        face2 = buildFace [get (myCube !! upFace) 6,get (myCube !! upFace) 3,get (myCube !! upFace) 0,get (myCube !! upFace) (7 - 1),get (myCube !! upFace) 1,get (myCube !! upFace) (8 - 1),get (myCube !! upFace) (5 - 1),get (myCube !! upFace) 2]
        face3 = buildFace [get (myCube !! rightFace) 7,get (myCube !! rightFace) (7 - 1),get (myCube !! rightFace) (6 - 1),get (myCube !! rightFace) (5 - 1),get (myCube !! rightFace) 3,get (myCube !! frontFace) 2,get (myCube !! frontFace) 1,get (myCube !! frontFace) 0]
        face5 = buildFace [get (myCube !! rightFace) 0,get (myCube !! rightFace) 1,get (myCube !! rightFace) 2,get (myCube !! backFace) (5 - 1),get (myCube !! backFace) 3,get (myCube !! backFace) 2,get (myCube !! backFace) 1,get (myCube !! backFace) 0]

moveD :: Cube -> Cube
moveD myCube = face0:face1:(myCube !! upFace):face3:face4:face5:[]
    where
        face0 = buildFace [get (myCube !! leftFace) 7,get (myCube !! leftFace) (7 - 1),get (myCube !! leftFace) (6 - 1),get (myCube !! frontFace) (5 - 1),get (myCube !! frontFace) 3,get (myCube !! frontFace) 2,get (myCube !! frontFace) 1,get (myCube !! frontFace) 0]
        face1 = buildFace [get (myCube !! backFace) 0,get (myCube !! backFace) 1,get (myCube !! backFace) 2,get (myCube !! leftFace) (5 - 1),get (myCube !! leftFace) 3,get (myCube !! leftFace) 2,get (myCube !! leftFace) 1,get (myCube !! leftFace) 0]
        face4 = buildFace [get (myCube !! downFace) 2,get (myCube !! downFace) (5 - 1),get (myCube !! downFace) (8 - 1),get (myCube !! downFace) 1,get (myCube !! downFace) (7 - 1),get (myCube !! downFace) 0,get (myCube !! downFace) 3,get (myCube !! downFace) (6 - 1)]
        face3 = buildFace [get (myCube !! frontFace) 7,get (myCube !! frontFace) (7 - 1),get (myCube !! frontFace) (6 - 1),get (myCube !! rightFace) (5 - 1),get (myCube !! rightFace) 3,get (myCube !! rightFace) 2,get (myCube !! rightFace) 1,get (myCube !! rightFace) 0]
        face5 = buildFace [get (myCube !! backFace) 7,get (myCube !! backFace) (7 - 1),get (myCube !! backFace) (6 - 1),get (myCube !! backFace) (5 - 1),get (myCube !! backFace) 3,get (myCube !! rightFace) (6 - 1),get (myCube !! rightFace) (7 - 1),get (myCube !! rightFace) (8 - 1)]

moveD2 :: Cube -> Cube
moveD2 = moveD . moveD

moveD' :: Cube -> Cube
moveD' myCube = face0:face1:(myCube !! upFace):face3:face4:face5:[]
    where
        face0 = buildFace [get (myCube !! rightFace) 7,get (myCube !! rightFace) (7 - 1),get (myCube !! rightFace) (6 - 1),get (myCube !! frontFace) (5 - 1),get (myCube !! frontFace) 3,get (myCube !! frontFace) 2,get (myCube !! frontFace) 1,get (myCube !! frontFace) 0]
        face1 = buildFace [get (myCube !! frontFace) 7,get (myCube !! frontFace) (7 - 1),get (myCube !! frontFace) (6 - 1),get (myCube !! leftFace) (5 - 1),get (myCube !! leftFace) 3,get (myCube !! leftFace) 2,get (myCube !! leftFace) 1,get (myCube !! leftFace) 0]
        face3 = buildFace [get (myCube !! backFace) 0,get (myCube !! backFace) 1,get (myCube !! backFace) 2,get (myCube !! rightFace) (5 - 1),get (myCube !! rightFace) 3,get (myCube !! rightFace) 2,get (myCube !! rightFace) 1,get (myCube !! rightFace) 0]
        face4 = buildFace [get (myCube !! downFace) 6,get (myCube !! downFace) 3,get (myCube !! downFace) 0,get (myCube !! downFace) (7 - 1),get (myCube !! downFace) 1,get (myCube !! downFace) (8 - 1),get (myCube !! downFace) (5 - 1),get (myCube !! downFace) 2]
        face5 = buildFace [get (myCube !! backFace) 7,get (myCube !! backFace) (7 - 1),get (myCube !! backFace) (6 - 1),get (myCube !! backFace) (5 - 1),get (myCube !! backFace) 3,get (myCube !! leftFace) (6 - 1),get (myCube !! leftFace) (7 - 1),get (myCube !! leftFace) (8 - 1)]

moveB :: Cube -> Cube
moveB myCube = (myCube !! frontFace):face1:face2:face3:face4:face5:[]
    where
        face1 = buildFace [get (myCube !! leftFace) 7,get (myCube !! leftFace) (7 - 1),get (myCube !! upFace) 0,get (myCube !! leftFace) (5 - 1),get (myCube !! upFace) 1,get (myCube !! leftFace) 2,get (myCube !! leftFace) 1,get (myCube !! upFace) 2]
        face2 = buildFace [get (myCube !! upFace) 7,get (myCube !! upFace) (7 - 1),get (myCube !! upFace) (6 - 1),get (myCube !! upFace) (5 - 1),get (myCube !! upFace) 3,get (myCube !! rightFace) (8 - 1),get (myCube !! rightFace) (5 - 1),get (myCube !! rightFace) 2]
        face3 = buildFace [get (myCube !! downFace) 6,get (myCube !! rightFace) (7 - 1),get (myCube !! rightFace) (6 - 1),get (myCube !! downFace) (7 - 1),get (myCube !! rightFace) 3,get (myCube !! downFace) (8 - 1),get (myCube !! rightFace) 1,get (myCube !! rightFace) 0]
        face4 = buildFace [get (myCube !! leftFace) 6,get (myCube !! leftFace) 3,get (myCube !! leftFace) 0,get (myCube !! downFace) (5 - 1),get (myCube !! downFace) 3,get (myCube !! downFace) 2,get (myCube !! downFace) 1,get (myCube !! downFace) 0]
        face5 = buildFace [get (myCube !! backFace) 2,get (myCube !! backFace) (5 - 1),get (myCube !! backFace) (8 - 1),get (myCube !! backFace) 1,get (myCube !! backFace) (7 - 1),get (myCube !! backFace) 0,get (myCube !! backFace) 3,get (myCube !! backFace) (6 - 1)]

moveB2 :: Cube -> Cube
moveB2 = moveB . moveB

moveB' :: Cube -> Cube
moveB' myCube = (myCube !! frontFace):face1:face2:face3:face4:face5:[]
    where
        face1 = buildFace [get (myCube !! leftFace) 7,get (myCube !! leftFace) (7 - 1),get (myCube !! downFace) (6 - 1),get (myCube !! leftFace) (5 - 1),get (myCube !! downFace) 3,get (myCube !! leftFace) 2,get (myCube !! leftFace) 1,get (myCube !! downFace) 0]
        face2 = buildFace [get (myCube !! upFace) 7,get (myCube !! upFace) (7 - 1),get (myCube !! upFace) (6 - 1),get (myCube !! upFace) (5 - 1),get (myCube !! upFace) 3,get (myCube !! leftFace) (6 - 1),get (myCube !! leftFace) 3,get (myCube !! leftFace) 0]
        face3 = buildFace [get (myCube !! upFace) 2,get (myCube !! rightFace) (7 - 1),get (myCube !! rightFace) (6 - 1),get (myCube !! upFace) 1,get (myCube !! rightFace) 3,get (myCube !! upFace) 0,get (myCube !! rightFace) 1,get (myCube !! rightFace) 0]
        face4 = buildFace [get (myCube !! rightFace) 2,get (myCube !! rightFace) (5 - 1),get (myCube !! rightFace) (8 - 1),get (myCube !! downFace) (5 - 1),get (myCube !! downFace) 3,get (myCube !! downFace) 2,get (myCube !! downFace) 1,get (myCube !! downFace) 0]
        face5 = buildFace [get (myCube !! backFace) 6,get (myCube !! backFace) 3,get (myCube !! backFace) 0,get (myCube !! backFace) (7 - 1),get (myCube !! backFace) 1,get (myCube !! backFace) (8 - 1),get (myCube !! backFace) (5 - 1),get (myCube !! backFace) 2]

moveToAction :: Move -> (Cube -> Cube)
moveToAction (MFront ONothing) = moveF
moveToAction (MFront OStrokes) = moveF2
moveToAction (MFront ODirection) = moveF'
moveToAction (MRight ONothing) = moveR
moveToAction (MRight OStrokes) = moveR2
moveToAction (MRight ODirection) = moveR'
moveToAction (MUp ONothing) = moveU
moveToAction (MUp OStrokes) = moveU2
moveToAction (MUp ODirection) = moveU'
moveToAction (MBack ONothing) = moveB
moveToAction (MBack OStrokes) = moveB2
moveToAction (MBack ODirection) = moveB'
moveToAction (MLeft ONothing) = moveL
moveToAction (MLeft OStrokes) = moveL2
moveToAction (MLeft ODirection) = moveL'
moveToAction (MDown ONothing) = moveD
moveToAction (MDown OStrokes) = moveD2
moveToAction (MDown ODirection) = moveD'

allTrueMoves = [MFront ONothing, MFront ODirection, MFront OStrokes, MRight ONothing, MRight ODirection, MRight OStrokes, MUp ONothing, MUp ODirection, MUp OStrokes, MBack ONothing, MBack ODirection, MBack OStrokes, MLeft ONothing, MLeft ODirection, MLeft OStrokes, MDown ONothing, MDown ODirection, MDown OStrokes]

allTrueMovesKey key
    | key == 0 = allTrueMoves
    | key == 1 = [MFront ONothing, MFront ODirection, MFront OStrokes, MRight ONothing, MRight ODirection, MRight OStrokes, MUp OStrokes, MBack ONothing, MBack ODirection, MBack OStrokes, MLeft ONothing, MLeft ODirection, MLeft OStrokes, MDown OStrokes]
    | key == 2 = [MFront OStrokes, MRight ONothing, MRight ODirection, MRight OStrokes, MUp OStrokes, MBack OStrokes, MLeft ONothing, MLeft ODirection, MLeft OStrokes, MDown OStrokes]
    | key == 3 = [MFront OStrokes, MRight OStrokes, MUp OStrokes, MBack OStrokes, MLeft OStrokes, MDown OStrokes]