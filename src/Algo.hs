module Algo (algo, newCubeNumber, applyMove, makeMovesNumber) where

import Moves
import Cube
import Data.HashMap (insert, singleton, member, Map, (!))
import Data.List (partition)
import Data.Maybe (isJust, fromJust)
import Data.Bits

-- Moves  
-- U    --> 0
-- U2   --> 1
-- U'   --> 2
-- D    --> 3
-- D2   --> 4
-- D'   --> 5
-- F    --> 6
-- F2   --> 7
-- F'   --> 8
-- B    --> 9
-- B2   --> 10
-- B'   --> 11
-- L    --> 12
-- L2   --> 13
-- L'   --> 14
-- R    --> 15
-- R2   --> 16
-- R'   --> 17

algo = initBfsNumber [] (-1)

affectedCubies :: [[Int]]
affectedCubies = [[0, 1, 2, 3, 0, 1, 2, 3],[4, 7, 6, 5, 4, 5, 6, 7],[0, 9, 4, 8,0, 3, 5, 4],[2, 10, 6, 11, 2, 1, 7, 6],[3, 11, 7, 9, 3, 2, 6, 5],[1, 8, 5, 10, 1,0, 4, 7]]

newCubeNumber :: [Int]
newCubeNumber = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

applyMove :: Int -> [Int] -> [Int]
applyMove move cube =
    applyMove' turn face cube
    where
        turn = (mod move 3) + 1
        face = div move 3

applyMove' 0 _ cube = cube
applyMove' turn face cube = applyMove' (turn - 1) face (applyMove'' 0 face cube cube (turn - 1))

applyMove'' 8 _ _ cube _ = cube
applyMove'' index face oldCube cube turn =
    let isCorner = boolToInt (index > 3)
        target = (affectedCubies !! face !! index) + isCorner * 12
        killer = (affectedCubies !! face !! (if (index .&. 3) == 3 then index - 3 else index + 1)) + isCorner * 12
        orientationDelta = if (index < 4) then boolToInt (face > 1 && face < 4) else if (face < 2) then 0 else 2 - (index .&. 1)
        tmpCube = buildMe (target + 20) ((oldCube !! (killer + 20)) + orientationDelta) $ buildMe target (oldCube !! killer) cube
        retCube = if turn == 0 then buildMe (target + 20) (mod (tmpCube !! (target + 20)) (2 + isCorner)) tmpCube else tmpCube
    in applyMove'' (index + 1) face oldCube retCube turn

buildMe :: Int -> Int -> [Int] -> [Int]
buildMe index replace cube = take index cube ++ [replace] ++ drop (index + 1) cube 

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0 

getID' :: Int -> [Int] -> [Int]
getID' 0 cube = slice (20, 32) cube -- Edge Orientation
getID' 1 cube = makeEslice cube 0 $ slice (31, 40) cube -- Corner Orientation + E Slice
getID' 2 cube = makeMagic cube -- M & S Slice, orbit
getID' _ cube = cube -- end

makeMagic :: [Int] -> [Int]
makeMagic cube = [repeatMagic (0, 12) (makeMagic12 cube), repeatMagic (0, 8) (makeMagic8 cube), repeatMagic (12, 20) (makeMagic20 cube)]

repeatMagic = repeatMagic' 0

repeatMagic' :: Int -> (Int, Int) -> (Int -> Int -> Int) -> Int
repeatMagic' ret (i, end) f
    | i == end = ret
    | otherwise = repeatMagic' (f i ret) ((i + 1), end) f

makeMagic12 :: [Int] -> Int -> Int -> Int
makeMagic12 cube e ret 
    | ce > 7 = ret .|. (2 `shiftL` (2 * e))
    | otherwise = ret .|. ((ce .&. 1) `shiftL` (2 * e))
    where ce = cube !! e

makeMagic8 :: [Int] -> Int -> Int -> Int
makeMagic8 cube e ret = ret .|. ((((cube !! (e + 12)) - 12) .&. 5) `shiftL` (3 * e))

makeMagic20 :: [Int] -> Int -> Int -> Int
makeMagic20 cube e ret = makeMagic20' cube e (e + 1) ret

makeMagic20' :: [Int] -> Int -> Int -> Int -> Int
makeMagic20' cube e j ret
    | j < 20 = makeMagic20' cube e (j + 1) (ret `xor` (boolToInt ((cube !! e) > (cube !! j))))
    | otherwise = ret

makeEslice :: [Int] -> Int -> [Int] -> [Int]
makeEslice _ 12 ret = ret
makeEslice cube e (x:xs) = makeEslice cube (e + 1) ((x .|. (((div (cube !! e) 8)) `shiftL` e)):xs)

slice :: (Int, Int) -> [a] -> [a]
slice (start, end) lst = take (end - start) $ drop start lst

makeMovesNumber :: [Move] -> [Int] -> [Int]
makeMovesNumber [] cube = cube
makeMovesNumber (x:xs) cube = makeMovesNumber xs $ applyMove (moveToInt x) cube

initBfsNumber :: [Move] -> Int -> [Int] -> [Move]
initBfsNumber oldMoves key cube
    | newKey == 4 = oldMoves
    | cubeID == newCubeID = initBfsNumber oldMoves newKey cube
    | makeMovesNumber moves cube == newCubeNumber = oldMoves ++ moves
    | otherwise = initBfsNumber (oldMoves ++ moves) newKey (makeMovesNumber moves cube)
    where
        newKey = key + 1
        cubeID = getID' newKey cube
        newCubeID = getID' newKey newCubeNumber
        startingList = [cube, newCubeNumber]
        startingMap = insert newCubeID (True, []) $ singleton cubeID (False, [])
        moves = bfsNumber startingList startingMap newKey

bfsNumber :: [[Int]] -> Map [Int] (Bool, [Move]) -> Int -> [Move]
bfsNumber [] _ _ = []
bfsNumber (x:xs) mapId key
    | dir == False && isJust isFinished = myMoves ++ (reverse $ map changeDirection foundedMoves) -- False = Standard ->
    | dir == True  && isJust isFinished = foundedMoves ++ (reverse $ map changeDirection myMoves) -- True  = Backward <-
    | otherwise = bfsNumber (xs ++ pushedChildren) (insertChildrenNumber mapId notMemberChildren dir key) key
    where
        cubeId = getID' key x
        (dir, moves) = mapId ! cubeId
        children = createChildrenNumber x 0 moves key
        (memberChildren, notMemberChildren) = partition (\ (z, _) -> member (getID' key z) mapId) children
        pushedChildren = map (\ (x, _) -> x) notMemberChildren
        isFinished = checkChildrenNumber memberChildren mapId dir key
        (myMoves, foundedMoves) = fromJust isFinished
        oui = testNumber memberChildren mapId dir key

insertChildrenNumber :: Map [Int] (Bool, [Move]) -> [([Int], [Move])] -> Bool -> Int -> Map [Int] (Bool, [Move])
insertChildrenNumber mapId [] _ _ = mapId
insertChildrenNumber mapId ((cube, moves):xs) dir key = insertChildrenNumber (insert id (dir, moves) mapId) xs dir key
    where id = getID' key cube 

checkChildrenNumber :: [([Int], [Move])] -> Map [Int] (Bool, [Move]) -> Bool -> Int -> Maybe ([Move], [Move])
checkChildrenNumber [] _ _ _ = Nothing
checkChildrenNumber ((cube, moves):xs) mapId oldDir key
    | oldDir /= newDir = Just (moves, newMoves)
    | otherwise = checkChildrenNumber xs mapId oldDir key
    where
        (newDir, newMoves) = mapId ! (getID' key cube)

testNumber :: [([Int], [Move])] -> Map [Int] (Bool, [Move]) -> Bool -> Int -> Maybe [Int]
testNumber [] _ _ _ = Nothing
testNumber ((cube, moves):xs) mapId oldDir key
    | oldDir /= newDir = Just (getID' key cube)
    | otherwise = testNumber xs mapId oldDir key
    where
        (newDir, newMoves) = mapId ! (getID' key cube)

applicableMoves :: [Int]
applicableMoves = [262143, 259263, 74943, 74898]

createChildrenNumber :: [Int] -> Int -> [Move] -> Int -> [([Int], [Move])]
createChildrenNumber _ 18 _  _ = []
createChildrenNumber cube m moves key
    | (applicableMoves !! key) .&. (1 `shiftL` m) == 0 = createChildrenNumber cube (m + 1) moves key
    | otherwise = (applyMove m cube, moves ++ [move]) : createChildrenNumber cube (m + 1) moves key
    where
        move = (intToMove m)