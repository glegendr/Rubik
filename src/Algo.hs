module Algo (algo, algoNumber, newCube', applyMove, makeMovesNumber) where

import Moves
import Cube
import Verification
import Debug.Trace
import Data.HashMap (insert, singleton, member, Map, (!))
import Data.List (partition)
import Data.Maybe (isJust, fromJust)
import Data.Bits

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

-- algo = iDDFS 0 [] 0
algo = initBfs [] (-1)

algoNumber = initBfsNumber [] (-1)

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
    | newKey == 4 = oldMoves
    | makeMoves moves cube == newCube = oldMoves ++ moves
    | otherwise = trace (cubeToString (makeMoves moves cube) ++ (show $ getId newKey (makeMoves moves cube)) ++ "\n" ++ (show $ getId newKey newCube)) $ initBfs (oldMoves ++ moves) newKey (makeMoves moves cube)
    where
        newKey = key + 1
        startingList = [cube, newCube]
        startingMap = insert (getId newKey newCube) (True, []) $ singleton (getId newKey cube) (False, [])
        moves = trace ("KEY: " ++ show newKey) $ bfs startingList startingMap newKey (allTrueMovesKey newKey)

bfs :: [Cube] -> Map String (Bool, [Move]) -> Int -> [Move] -> [Move]
bfs [] _ _ _ = []
bfs (x:xs) mapId key allowedMoves
    -- | key == 0 && group1Verification x = trace "ETAPE 1" $ moves
    -- | key == 1 && group2Verification x = trace "ETAPE 2" $ moves
    -- | key == 2 && group3Verification x = trace "ETAPE 3" $ moves
    -- | key == 3 && x == newCube = moves
    | dir == False && isJust isFinished = trace ("->: " ++ show myMoves ++ " > " ++ show (reverse $ map changeDirection foundedMoves) ++ " | " ++ (show $ fromJust oui)) $ myMoves ++ (reverse $ map changeDirection foundedMoves) -- False = Standard ->
    | dir == True  && isJust isFinished = trace ("<-: " ++ show foundedMoves ++ " < " ++ show (reverse $ map changeDirection myMoves) ++ " | " ++ (show $ fromJust oui)) $ foundedMoves ++ (reverse $ map changeDirection myMoves) -- True  = Backward <-
    | otherwise = bfs (xs ++ pushedChildren) (insertChildren mapId notMemberChildren dir key) key allowedMoves
    where
        cubeId = getId key x
        (dir, moves) = mapId ! cubeId
        children = createChildren x allowedMoves moves
        (memberChildren, notMemberChildren) = partition (\ (z, _) -> member (getId key z) mapId) children
        pushedChildren = map (\ (x, _) -> x) notMemberChildren
        isFinished = checkChildren memberChildren mapId dir key
        (myMoves, foundedMoves) = fromJust isFinished
        oui = test memberChildren mapId dir key

insertChildren :: Map String (Bool, [Move]) -> [(Cube, [Move])] -> Bool -> Int -> Map String (Bool, [Move])
insertChildren mapId [] _ _ = mapId
insertChildren mapId ((cube, moves):xs) dir key = insertChildren (insert id (dir, moves) mapId) xs dir key
    where id = getId key cube 

checkChildren :: [(Cube, [Move])] -> Map String (Bool, [Move]) -> Bool -> Int -> Maybe ([Move], [Move])
checkChildren [] _ _ _ = Nothing
checkChildren ((cube, moves):xs) mapId oldDir key
    | oldDir /= newDir = Just (moves, newMoves)
    | otherwise = checkChildren xs mapId oldDir key
    where
        (newDir, newMoves) = mapId ! (getId key cube)

test :: [(Cube, [Move])] -> Map String (Bool, [Move]) -> Bool -> Int -> Maybe String
test [] _ _ _ = Nothing
test ((cube, moves):xs) mapId oldDir key
    | oldDir /= newDir = Just (getId key cube)
    | otherwise = test xs mapId oldDir key
    where
        (newDir, newMoves) = mapId ! (getId key cube)

createChildren :: Cube -> [Move] -> [Move] -> [(Cube, [Move])]
createChildren _ [] _  = []
createChildren cube (x:xs) moves
    | skipMove x moves = createChildren cube xs moves
    | otherwise = (moveToAction x cube, moves ++ [x]) : createChildren cube xs moves

getId :: Int -> Cube -> String
getId key cube
    | key == 0 = getAllEdges cube
    | key == 1 = getRightLeftCorner cube
    | otherwise = []

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
applyMove move cube = -- F = 6
    applyMove' turn face cube
    where
        turn = (mod move 3) + 1 -- 1
        face = div move 3 -- 2

applyMove' 0 _ cube = cube
applyMove' turn face cube = applyMove' (turn - 1) face (applyMove'' 0 face cube cube (turn - 1))

applyMove'' 8 _ _ cube _ = cube
applyMove'' index face oldCube cube turn = -- 0
    let isCorner = boolToInt (index > 3) -- 0
        target = (affectedCubies !! face !! index) + isCorner * 12 -- 0
        killer = (affectedCubies !! face !! (if (index .&. 3) == 3 then index - 3 else index + 1)) + isCorner * 12 -- 0
        orientationDelta = if (index < 4) then boolToInt (face > 1 && face < 4) else if (face < 2) then 0 else 2 - (index .&. 1) --
        tmpCube = buildMe (target + 20) ((oldCube !! (killer + 20)) + orientationDelta) $ buildMe target (oldCube !! killer) cube
        retCube = if turn == 0 then buildMe (target + 20) (mod (tmpCube !! (target + 20)) (2 + isCorner)) tmpCube else tmpCube
    in applyMove'' (index + 1) face oldCube retCube turn

buildMe :: Int -> Int -> [Int] -> [Int]
buildMe index replace cube = take index cube ++ [replace] ++ drop (index + 1) cube 

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

getID' :: Int -> [Int] -> [Int]
getID' 0 cube = slice (20, 32) cube
getID' 1 cube = makeEslice cube 0 $ slice (31, 40) cube
getID' 2 cube = makeMagic cube
getID' _ cube = cube

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
-- RubiksCubeSolver.prototype.getId = function(state) {
-- 	//--- Phase 1: Edge orientations.
-- 	if(this.phase < 2)
-- 		return JSON.stringify(state.slice(20,32));
-- 	
-- 	//-- Phase 2: Corner orientations, E slice edges.
-- 	if(this.phase < 3){
-- 		var result = state.slice(31,40);
-- 		for(var e=0; e<12; e++)
-- 			result[0] |= (Math.floor(state[e] / 8)) << e;
-- 		return JSON.stringify(result);
-- 	}
-- 	
-- 	//--- Phase 3: Edge slices M and S, corner tetrads, overall parity.
-- 	if(this.phase < 4){
-- 		var result = [0,0,0];
-- 		for(var e=0; e<12; e++)
-- 			result[0] |= ((state[e] > 7) ? 2 : (state[e] & 1)) << (2*e);
-- 		for(var c=0; c<8; c++)
-- 			result[1] |= ((state[c+12]-12) & 5) << (3*c);
-- 		for(var i=12; i<20; i++)
-- 			for(var j=i+1; j<20; j++)
-- 				result[2] ^= state[i] > state[j];
-- 		return JSON.stringify(result);
-- 	}
-- 	
-- 	//--- Phase 4: The rest.
-- 	return JSON.stringify(state);
-- }

makeMovesNumber :: [Move] -> [Int] -> [Int]
makeMovesNumber [] cube = cube
makeMovesNumber (x:xs) cube = makeMovesNumber xs $ applyMove (moveToInt x) cube

initBfsNumber :: [Move] -> Int -> [Int] -> [Move]
initBfsNumber oldMoves key cube
    | newKey == 4 = oldMoves
    | cubeID == newCubeID = trace ("SAME ->" ++ show newCubeID) $ initBfsNumber oldMoves newKey cube
    | makeMovesNumber moves cube == newCube' = oldMoves ++ moves
    | otherwise = trace (("Cube:   " ++ show (makeMovesNumber moves cube)) ++ "\nLength: " ++ (show $ length (makeMovesNumber moves cube)) ++"\nID:     " ++(show $ getID' newKey (makeMovesNumber moves cube)) ++ "\nID New: " ++ (show $ getID' newKey newCube')) $ initBfsNumber (oldMoves ++ moves) newKey (makeMovesNumber moves cube)
    where
        newKey = key + 1
        cubeID = getID' newKey cube
        newCubeID = getID' newKey newCube'
        startingList = [cube, newCube']
        startingMap = insert newCubeID (True, []) $ singleton cubeID (False, [])
        moves = trace ("KEY: " ++ show newKey) $ bfsNumber startingList startingMap newKey (allTrueMovesKey newKey)

bfsNumber :: [[Int]] -> Map [Int] (Bool, [Move]) -> Int -> [Move] -> [Move]
bfsNumber [] _ _ _ = []
bfsNumber (x:xs) mapId key allowedMoves
    -- | key == 0 && group1Verification x = trace "ETAPE 1" $ moves
    -- | key == 1 && group2Verification x = trace "ETAPE 2" $ moves
    -- | key == 2 && group3Verification x = trace "ETAPE 3" $ moves
    -- | key == 3 && x == newCube = moves
    | dir == False && isJust isFinished = trace ("->: " ++ show myMoves ++ " > " ++ show (reverse $ map changeDirection foundedMoves) ++ " | " ++ (show $ fromJust oui)) $ myMoves ++ (reverse $ map changeDirection foundedMoves) -- False = Standard ->
    | dir == True  && isJust isFinished = trace ("<-: " ++ show foundedMoves ++ " < " ++ show (reverse $ map changeDirection myMoves) ++ " | " ++ (show $ fromJust oui)) $ foundedMoves ++ (reverse $ map changeDirection myMoves) -- True  = Backward <-
    | otherwise = bfsNumber (xs ++ pushedChildren) (insertChildrenNumber mapId notMemberChildren dir key) key allowedMoves
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
    -- | skipMove move moves = createChildrenNumber cube (m + 1) moves key
    | otherwise = (applyMove m cube, moves ++ [move]) : createChildrenNumber cube (m + 1) moves key
    where
        move = (intToMove m)