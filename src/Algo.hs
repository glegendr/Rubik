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
-- algo = initBfs [] (-1)

iDDFS :: Int -> [Move] -> Int -> Cube -> [Move]
iDDFS depth oldMoves key root
    | makeMoves moves root == newCube = oldMoves ++ moves
    | moves /= [] = trace (show $ length moves) $ iDDFS 0 (oldMoves ++ moves) newKey (makeMoves moves root)
    | otherwise = iDDFS (depth + 1) oldMoves key root
    where (moves, newKey) = trace ((show depth) ++ " -> " ++ (show key)) $ dLS root depth [] key (allTrueMovesKey key)

dLS :: Cube -> Int -> [Move] -> Int -> [Move] -> ([Move], Int)
dLS node depth moves key moveKey
    | depth /= 0 = applyDLS moveKey node (depth - 1) moves key moveKey
    | key == 0 && group1Verification node = trace "ETAPE 1" $ (moves, 1)
    | key == 1 && group2Verification node = trace "ETAPE 2" $ (moves, 2)
    | key == 2 && group3Verification node = trace "ETAPE 3" $ (moves, 3)
    | key == 3 && node == newCube = (moves, 4)
    | otherwise = ([], key)

applyDLS :: [Move] -> Cube -> Int -> [Move] -> Int -> [Move] -> ([Move], Int)
applyDLS [] _ _ _ key _ = ([], key)
applyDLS (x:xs) cube depth moves key moveKey
    | skipMove x moves = applyDLS xs cube depth moves key moveKey
    | dlsReturn == [] = applyDLS xs cube depth moves key moveKey
    | otherwise = (dlsReturn, newKey)
    where (dlsReturn, newKey) = dLS (moveToAction x cube) depth (moves ++ [x]) key moveKey

makeMoves :: [Move] -> Cube -> Cube
makeMoves moves cube = foldl (\ newCube f -> f newCube) cube $ map moveToAction moves

skipMove x lst = skipMove' (clear x) (map clear lst)

skipMove' :: Move -> [Move] -> Bool
skipMove' _ [] = False
skipMove' x lst
    | x == last lst = True
    | length lst > 2 && x == (last $ init $ init lst) = True
    | otherwise = False

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

initBfs :: [Move] -> Int -> Cube -> [Move]
initBfs oldMoves key cube
    | makeMoves moves cube == newCube = oldMoves ++ moves
    | otherwise = initBfs (oldMoves ++ moves) newKey (makeMoves moves cube)
    where
        newKey = key + 1
        moves = trace ("KEY: " ++ show newKey) $ bfs [(cube, [])] newKey (allTrueMovesKey newKey)

bfs :: [(Cube, [Move])] -> Int -> [Move] -> [Move]
bfs [] _ _ = []
bfs ((x, moves):xs) key allowedMoves
    | key == 0 && group1Verification x = trace "ETAPE 1" $ moves
    | key == 1 && group2Verification x = trace "ETAPE 2" $ moves
    | key == 2 && group3Verification x = trace "ETAPE 3" $ moves
    | key == 3 && x == newCube = moves
    | otherwise = bfs (xs ++ (createChildren x allowedMoves moves)) key allowedMoves

createChildren :: Cube -> [Move] -> [Move] -> [(Cube, [Move])]
createChildren _ [] _ = []
createChildren cube (x:xs) moves
    | skipMove x moves = createChildren cube xs moves
    | otherwise = (moveToAction x cube, moves ++ [x]) : createChildren cube xs moves