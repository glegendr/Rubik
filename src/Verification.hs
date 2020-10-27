module Verification
( testAll
, group1Verification
, group2Verification
, group3Verification)
 where

import Cube
import Moves

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

oppose :: Char -> Char
oppose 'W' = 'Y'
oppose 'G' = 'B'
oppose 'O' = 'R'
oppose 'B' = 'G'
oppose 'R' = 'O'
oppose 'Y' = 'W'

colorVerificationLoop :: Int -> Int -> [Face] -> Bool
colorVerificationLoop _ _ [] = True
colorVerificationLoop modBy equalTo  (x:xs)
    | colorVerification modBy equalTo x == False = False
    | otherwise = colorVerificationLoop modBy equalTo xs


colorVerification :: Int -> Int -> Face -> Bool
colorVerification modBy equalTo face =
    all (\ (a, _) -> a == (face !! 4)) $ filter (\ (_, b) -> mod b modBy == equalTo) $ zip face [0..]

isFinished :: Cube -> Bool
isFinished cube = cube == newCube

edgeSide = [[(2, 7), (1, 5), (3, 3), (4, 1)], [(2, 3), (5, 3), (0, 3), (4, 3)], [(5, 7), (1, 1), (3, 1), (0, 1)], [(2, 5), (0, 5), (5, 5), (4, 5)], [(0, 7), (1, 7), (3, 7), (5, 1)], [(4, 7), (1, 3), (3, 5), (2, 1)]] -- edgeSide !! side !! edge

getEdges :: Face -> String
getEdges face = [face !! 1, face !! 3, face !! 5, face !! 7]

getEdgeSide :: Cube -> Int -> [Int] -> String
getEdgeSide _ _ [] = []
getEdgeSide cube side (x:xs) = cube !! newSide !! newIndex : getEdgeSide cube side xs
    where
        (newSide, newIndex) = (edgeSide !! side !! x)

group1Verification :: Cube -> Bool
group1Verification cube
    | any (\ z -> z == 'O' || z == 'R') (getEdges faceF) = False -- B G == O R
    | any (\ z -> z == 'O' || z == 'R') (getEdges faceB) = False
    where
        faceF = cube !! frontFace
        faceB = cube !! backFace
group1Verification cube
    | any (\ z -> z == 'W' || z == 'Y') $ getEdgeSide cube frontFace (map (\(_,z) -> z) $ filter (\(x,_) -> x == 'B' || x == 'G') $ zip (getEdges faceF) [0..]) = False
    | any (\ z -> z == 'W' || z == 'Y') $ getEdgeSide cube backFace (map (\(_,z) -> z) $ filter (\(x,_) -> x == 'B' || x == 'G') $ zip (getEdges faceB) [0..]) = False
    where
        faceF = cube !! frontFace
        faceB = cube !! backFace
group1Verification cube
    | any (\ z -> z == 'O' || z == 'R') $ tail $ init (getEdges faceL) = False
    | any (\ z -> z == 'O' || z == 'R') $ tail $ init (getEdges faceR) = False
    where
        faceR = cube !! rightFace
        faceL = cube !! leftFace
group1Verification cube
    | any (\ z -> z == 'W' || z == 'Y') $ getEdgeSide cube upFace (map (\(_,z) -> z) $ filter (\(x,_) -> x == 'B' || x == 'G') $ init $ tail $ zip (getEdges faceL) [0..]) = False
    | any (\ z -> z == 'W' || z == 'Y') $ getEdgeSide cube downFace (map (\(_,z) -> z) $ filter (\(x,_) -> x == 'B' || x == 'G') $ init $ tail $ zip (getEdges faceR) [0..]) = False
    where
        faceR = cube !! rightFace
        faceL = cube !! leftFace
group1Verification _ = True

checkMiddleLine :: [Face] -> Bool
checkMiddleLine [] = True
checkMiddleLine (x:xs)
    | all (== color) mid == False = False
    | otherwise = checkMiddleLine xs
    where
        mid = take 3 $ drop 3 x
        color = head mid

group2Verification :: Cube -> Bool
group2Verification cube
    | colorVerificationLoop 2 0 [faceR, faceL] == False = False
    | checkMiddleLine [faceF, faceB, faceR, faceL] == False = False
    -- | colorVerificationLoop 3 1 [faceU, faceD, faceF, faceB] == False = False
    | otherwise = True
    where
        faceR = cube !! rightFace
        faceL = cube !! leftFace
        faceF = cube !! frontFace
        faceB = cube !! backFace

group3Verification :: Cube -> Bool
group3Verification cube
    | colorVerificationLoop 2 1 cube == False = False
    | orbitVerification cube == False = False
    | otherwise = True

orbitVerification :: Cube -> Bool
orbitVerification cube = orbitVerificationLoop [coF1, coF2, coF3, coF4, coL1, coL2, coR1, coR2]
    where
        coF1 = ((cube !! frontFace !! 0, cube !! leftFace !! 2,  cube !! upFace !! 6),      (cube !! frontFace !! 4, cube !! leftFace !! 4,  cube !! upFace !! 4))
        coF2 = ((cube !! frontFace !! 2, cube !! upFace !! 8,    cube !! rightFace !! 0),   (cube !! frontFace !! 4, cube !! upFace !! 4,    cube !! rightFace !! 4))
        coF3 = ((cube !! frontFace !! 8, cube !! rightFace !! 6, cube !! downFace !! 2),    (cube !! frontFace !! 4, cube !! rightFace !! 4, cube !! downFace !! 4))
        coF4 = ((cube !! frontFace !! 6, cube !! downFace !! 0,  cube !! leftFace !! 8),    (cube !! frontFace !! 4, cube !! downFace !! 4,  cube !! leftFace !! 4))
        coL1 = ((cube !! leftFace !! 0,  cube !! backFace !! 6,  cube !! upFace !! 0),      (cube !! leftFace !! 4,  cube !! backFace !! 4,  cube !! upFace !! 4))
        coL2 = ((cube !! leftFace !! 6,  cube !! downFace !! 6,  cube !! backFace !! 0),    (cube !! leftFace !! 4,  cube !! downFace !! 4,  cube !! backFace !! 4))
        coR1 = ((cube !! rightFace !! 2, cube !! upFace !! 2,    cube !! backFace !! 8),    (cube !! rightFace !! 4, cube !! upFace !! 4,    cube !! backFace !! 4))
        coR2 = ((cube !! rightFace !! 8, cube !! downFace !! 8,  cube !! backFace !! 2),    (cube !! rightFace !! 4, cube !! downFace !! 4,  cube !! backFace !! 4))

orbitVerificationLoop :: [((Char, Char, Char), (Char, Char, Char))] -> Bool
orbitVerificationLoop [] = True
orbitVerificationLoop (x:xs)
    | orbitVerificationLoop' x == False = False
    | otherwise = orbitVerificationLoop xs

orbitVerificationLoop' :: ((Char, Char, Char), (Char, Char, Char)) -> Bool
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
    putStrLn $ "LF           False  -> " ++ (show $ group1Verification $ moveF $ moveL newCube)
    putStrLn $ "L            False  -> " ++ (show $ group1Verification $ moveL newCube)
    let cubeTest = moveR $ moveU2 $ moveD $ moveL' $ moveF' $ moveL $ moveF $ moveU $ moveL $ moveU newCube 
    putStrLn $ "ULUFLF'L'DU2 False -> " ++ (show $ group1Verification cubeTest)
    putStrLn $ "L            False -> " ++ (show $ group1Verification $ moveL newCube)
    putStrLn $ "R2           True  -> " ++ (show $ group1Verification $ moveR2 newCube)
    putStrLn "Test group2Verification"
    putStrLn $ "newCube      True  -> " ++ (show $ isFinished newCube)
    putStrLn $ "newCube      True  -> " ++ (show $ isFinished newCube)
    -- putCubeColor cubeTest
   {-- ULUFLF'L'DU2R--}
