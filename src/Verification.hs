module Verification
( testAll
, group1Verification
, group2Verification
, group3Verification)
 where

import Cube
import Moves
import Data.Bits

{--
    groupe 0 : cube mélanger
    groupe 1 : edges bien orientés
        Si couleur du edge = couleur du centre alors GOOD sauf haut et bas
        Si couleur du edge = couleur du centre opposé alors GOOD sauf haut et bas
        Sinon = BAD

    Look at the F/B faces. If you see:
    
       - L/R colour (orange/red) it's bad.
       - U/D colour means you need to look round the side of the edge. If the side is F/B (white/yellow) it is bad.
    
    
    Then look at the U/D faces of the M-slice (middle layer). The same rules apply. If you see:
    
       - L/R colour (orange/red) it's bad.
       - U/D colour (green/blue) means you need to look round the side of the edge. If the side is F/B (white/yellow) it is bad.
    
        U, U', D, D'
    groupe 2 : corner bien orientés + raie du milieu bien faite
        (edge haut face + edge bas face + edge haut arrière + edge bas arrière bien placés)
        F, F', B, B'
    groupe 3 : mettre corner dans bon orbit + bien mettre les edges
        bon orbit = avec un 180 degrés mouvement, le coin et dans sa bonne position
        L, L', R, R'

        | O  O  O | 
        | O  O  O |
        | O  O  O |
------- | ------- | -------
G  G  G | 0  1  2 | B  B  B    
G  G  G | 3  W  5 | B  B  B  
G  G  G | 6  7  8 | B  B  B  
------- | ------- | -------
        | R  R  R |
        | R  R  R |
        | R  R  R |
        | ------- |
        | Y  Y  Y |
        | Y  Y  Y |
        | Y  Y  Y |
--}

oppose :: Int -> Int
oppose 0 = 5
oppose 1 = 3
oppose 2 = 4
oppose 3 = 1
oppose 4 = 2
oppose 5 = 0

colorVerificationLoop :: [Int] -> [Face] -> [Int] -> Bool
colorVerificationLoop _ [] _ = True
colorVerificationLoop lst  (x:xs) (y:ys)
    | colorVerification lst x y == False = False
    | otherwise = colorVerificationLoop lst xs ys


colorVerification :: [Int] -> Face -> Int -> Bool
colorVerification lst face color =
    all (== color) $ map (\(a, _) -> a) $ filter (\ (_, b) -> b `elem` lst) $ zip (faceToList face) [0..]

isFinished :: Cube -> Bool
isFinished cube = cube == newCube

edgeSide = [[(2, 6), (1, 4), (3, 3), (4, 1)], [(2, 3), (5, 3), (0, 3), (4, 3)], [(5, 6), (1, 1), (3, 1), (0, 1)], [(2, 4), (0, 4), (5, 4), (4, 4)], [(0, 6), (1, 6), (3, 6), (5, 1)], [(4, 6), (1, 3), (3, 4), (2, 1)]] -- edgeSide !! side !! edge

getEdges :: Face -> [Int]
getEdges face = [get face 1, get face 3, get face 4, get face 6]

getEdgeSide :: Cube -> Int -> [Int] -> [Int]
getEdgeSide _ _ [] = []
getEdgeSide cube side (x:xs) = get (cube !! newSide) newIndex : getEdgeSide cube side xs
    where
        (newSide, newIndex) = (edgeSide !! side !! x)

group1Verification :: Cube -> Bool
group1Verification cube
    | any (\ z -> z == 1 || z == 3) (getEdges faceF) = False
    | any (\ z -> z == 1 || z == 3) (getEdges faceB) = False
    where
        faceF = cube !! frontFace
        faceB = cube !! backFace
group1Verification cube
    | any (\ z -> z == 0 || z == 5) $ getEdgeSide cube frontFace (map (\(_,z) -> z) $ filter (\(x,_) -> x == 2 || x == 4) $ zip (getEdges faceF) [0..]) = False
    | any (\ z -> z == 0 || z == 5) $ getEdgeSide cube backFace (map (\(_,z) -> z) $ filter (\(x,_) -> x == 2 || x == 4) $ zip (getEdges faceB) [0..]) = False
    where
        faceF = cube !! frontFace
        faceB = cube !! backFace
group1Verification cube
    | any (\ z -> z == 1 || z == 3) $ tail $ init (getEdges faceU) = False
    | any (\ z -> z == 1 || z == 3) $ tail $ init (getEdges faceD) = False
    where
        faceD = cube !! downFace
        faceU = cube !! upFace
group1Verification cube
    | any (\ z -> z == 0 || z == 5) $ getEdgeSide cube upFace (map (\(_,z) -> z) $ filter (\(x,_) -> x == 2 || x == 4) $ init $ tail $ zip (getEdges faceU) [0..]) = False
    | any (\ z -> z == 0 || z == 5) $ getEdgeSide cube downFace (map (\(_,z) -> z) $ filter (\(x,_) -> x == 2 || x == 4) $ init $ tail $ zip (getEdges faceD) [0..]) = False
    where
        faceD = cube !! downFace
        faceU = cube !! upFace
group1Verification _ = True

group2Verification :: Cube -> Bool
group2Verification cube
    | colorVerificationLoop [0, 2, 5, 7] [faceU, faceD] [upFace, downFace] == False = False
    | colorVerificationLoop [1, 6] [faceU, faceD, faceF, faceB] [upFace, downFace, frontFace, backFace] == False = False
    | otherwise = True
    where
        faceU = cube !! upFace
        faceD = cube !! downFace
        faceF = cube !! frontFace
        faceB = cube !! backFace

group3Verification :: Cube -> Bool
group3Verification cube
    | colorVerificationLoop [1, 3, 4, 6] cube [0,1,2,3,4,5] == False = False
    | orbitVerification cube == False = False
    | otherwise = True

orbitVerification :: Cube -> Bool
orbitVerification cube = orbitVerificationLoop [coF1, coF2, coF3, coF4, coL1, coL2, coR1, coR2]
    where
        coF1 = ((get (cube !! frontFace) 0, get (cube !! leftFace) 2,  get (cube !! upFace) 6),      (get (cube !! frontFace) 4, get (cube !! leftFace) 4,  get (cube !! upFace) 4))
        coF2 = ((get (cube !! frontFace) 2, get (cube !! upFace) 8,    get (cube !! rightFace) 0),   (get (cube !! frontFace) 4, get (cube !! upFace) 4,    get (cube !! rightFace) 4))
        coF3 = ((get (cube !! frontFace) 8, get (cube !! rightFace) 6, get (cube !! downFace) 2),    (get (cube !! frontFace) 4, get (cube !! rightFace) 4, get (cube !! downFace) 4))
        coF4 = ((get (cube !! frontFace) 6, get (cube !! downFace) 0,  get (cube !! leftFace) 8),    (get (cube !! frontFace) 4, get (cube !! downFace) 4,  get (cube !! leftFace) 4))
        coL1 = ((get (cube !! leftFace) 0,  get (cube !! backFace) 6,  get (cube !! upFace) 0),      (get (cube !! leftFace) 4,  get (cube !! backFace) 4,  get (cube !! upFace) 4))
        coL2 = ((get (cube !! leftFace) 6,  get (cube !! downFace) 6,  get (cube !! backFace) 0),    (get (cube !! leftFace) 4,  get (cube !! downFace) 4,  get (cube !! backFace) 4))
        coR1 = ((get (cube !! rightFace) 2, get (cube !! upFace) 2,    get (cube !! backFace) 8),    (get (cube !! rightFace) 4, get (cube !! upFace) 4,    get (cube !! backFace) 4))
        coR2 = ((get (cube !! rightFace) 8, get (cube !! downFace) 8,  get (cube !! backFace) 2),    (get (cube !! rightFace) 4, get (cube !! downFace) 4,  get (cube !! backFace) 4))

orbitVerificationLoop :: [((Int, Int, Int), (Int, Int, Int))] -> Bool
orbitVerificationLoop [] = True
orbitVerificationLoop (x:xs)
    | orbitVerificationLoop' x == False = False
    | otherwise = orbitVerificationLoop xs

orbitVerificationLoop' :: ((Int, Int, Int), (Int, Int, Int)) -> Bool
orbitVerificationLoop' (a, b)
    | a == b = True
orbitVerificationLoop' ((co1, co2, co3), (ce1, ce2, ce3)) = all (\ (co, ce) -> co == oppose ce || co == ce) newList
    where
        newList = [(co1, ce1), (co2, ce2), (co3, ce3)]

testAll = do
    putStrLn "Test isFinished"
    putStrLn $ "newCube      True  -> " ++ (show $ isFinished newCube)
    putStrLn $ "moveR        False -> " ++ (show $ isFinished $ moveR newCube)
    putStrLn $ "U2U2R'R      True  -> " ++ (show $ isFinished $ moveR $ moveR' $ moveU2 $ moveU2 newCube)
    putStrLn "Test group1Verification"
    putStrLn $ "newCube      True  -> " ++ (show $ group1Verification newCube)
    putStrLn $ "LF           True  -> " ++ (show $ group1Verification $ moveF $ moveL newCube)
    putStrLn $ "L            True  -> " ++ (show $ group1Verification $ moveL newCube)
    let cubeTest = moveR $ moveU2 $ moveD $ moveL' $ moveF' $ moveL $ moveF $ moveU $ moveL $ moveU newCube 
    putStrLn $ "ULUFLF'L'DU2 False -> " ++ (show $ group1Verification cubeTest)
    putStrLn $ "U            False -> " ++ (show $ group1Verification $ moveU newCube)
    putStrLn $ "D2           True  -> " ++ (show $ group1Verification $ moveD2 newCube)
    putStrLn "Test group2Verification"
    putStrLn $ "newCube      True  -> " ++ (show $ isFinished newCube)
    -- putStrLn $ "newCube      True  -> " ++ (show $ isFinished newCube)
    -- putCubeColor cubeTest
   {-- ULUFLF'L'DU2R--}
