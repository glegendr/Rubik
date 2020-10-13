module Algo (algo) where

import Moves
import Cube
import Verification
import Debug.Trace

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
    | moves /= [] = trace (show $ length moves) $ iDDFS 0 (oldMoves ++ moves) newKey (makeMoves moves root)
    | otherwise = iDDFS (depth + 1) oldMoves newKey root
    where (moves, newKey) = dLS root depth [] key

dLS :: Cube -> Int -> [Move] -> Int -> ([Move], Int)
dLS node depth moves key
    | depth /= 0 = applyDLS (allTrueMovesKey key) node (depth - 1) moves key
    | node == newCube = (moves, 4)
    | key < 1 && group1Verification node = trace "ETAPE 1" $ (moves, 1)
    | key < 2 && group2Verification node = trace "ETAPE 2" $ (moves, 2)
    | key < 3 && group3Verification node = trace "ETAPE 3" $ (moves, 3)
    | otherwise = ([], key)

applyDLS :: [Move] -> Cube -> Int -> [Move] -> Int -> ([Move], Int)
applyDLS [] _ _ _ key = ([], key)
applyDLS (x:xs) cube depth moves key
    | skipMove x moves = applyDLS xs cube depth moves key
    | dlsReturn == [] = applyDLS xs cube depth moves key
    | otherwise = (dlsReturn, newKey)
    where (dlsReturn, newKey) = dLS (moveToAction x cube) depth (moves ++ [x]) key

makeMoves :: [Move] -> Cube -> Cube
makeMoves moves cube = foldl (\ newCube f -> f newCube) cube $ map moveToAction moves

skipMove x lst = skipMove' (clear x) (map clear lst)

skipMove' :: Move -> [Move] -> Bool
skipMove' _ [] = False
skipMove' x lst
    | x == last lst = True
    | head subList == x && all (== opposite) (tail subList) = True
    | otherwise = False
    where
        subList = reverse $ takeWhileEq (/= x) $ reverse lst
        opposite = getOpposite x
    
takeWhileEq :: (a -> Bool) -> [a] -> [a]
takeWhileEq _ [] = []
takeWhileEq f (x:xs)
    | f x = x : takeWhileEq f xs
    | otherwise = [x]

getOpposite (MRight _ ) = MLeft ONothing
getOpposite (MLeft _ ) = MRight ONothing
getOpposite (MUp _ ) = MDown ONothing
getOpposite (MDown _ ) = MUp ONothing
getOpposite (MFront _ ) = MBack ONothing
getOpposite (MBack _ ) = MFront ONothing

clear (MRight _ ) = MRight ONothing
clear (MLeft _ ) = MLeft ONothing
clear (MUp _ ) = MUp ONothing
clear (MDown _ ) = MDown ONothing
clear (MFront _ ) = MFront ONothing
clear (MBack _ ) = MBack ONothing