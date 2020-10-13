module Algo (algo) where

import Moves
import Cube
import Verification
-- 
-- 
-- algo :: Cube -> [Move]
-- algo cube = algo' openList closeList
--     where
--         openList = Bs.singelton cube -- binaryHeap
--         closeList = Hs.singelton cube  -- hasset
-- 
-- algo' :: BinaryHeap Cube -> HashSet Cube -> [Move]
-- algo' ol cl =
--     createChildren ol
--     where
--         first = head ol
--         newOl = tail ol
-- 
--

algo = iDDFS 0 [] 0

iDDFS :: Int -> [Move] -> Int -> Cube -> [Move]
iDDFS depth oldMoves key root
    | makeMoves moves root == newCube = oldMoves ++ moves
    | moves /= [] = iDDFS 0 (oldMoves ++ moves) newKey (makeMoves moves root)
    | otherwise = iDDFS (depth + 1) oldMoves newKey root
    where (moves, newKey) = dLS root depth [] key

dLS :: Cube -> Int -> [Move] -> Int -> ([Move], Int)
dLS node depth moves key
    | depth == 0 = ([], key)
    | node == newCube = (moves, 4)
    | key < 1 && group1Verification node = (moves, 1)
    | key < 2 && group2Verification node = (moves, 2)
    | key < 3 && group3Verification node = (moves, 3)
    | otherwise = applyDLS (allTrueMovesKey key) node (depth - 1) moves key

applyDLS :: [Move] -> Cube -> Int -> [Move] -> Int -> ([Move], Int)
applyDLS [] _ _ _ key = ([], key)
applyDLS (x:xs) cube depth moves key
    | dlsReturn == [] = applyDLS xs cube depth moves newKey
    | otherwise = (dlsReturn, newKey)
    where (dlsReturn, newKey) = dLS (moveToAction x cube) depth (moves ++ [x]) key

makeMoves :: [Move] -> Cube -> Cube
makeMoves moves cube = foldl (\ newCube f -> f newCube) cube $ map moveToAction moves