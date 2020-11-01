module Cube
( Cube
, Face
, allTrueMoves
, allTrueMovesKey
, rtxOnCube
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
, makeMoves
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
type Face = String
type Cube = [Face]

allMoves = ["F", "F\'", "F2", "R", "R\'", "R2", "U", "U\'", "U2", "B", "B\'", "B2", "L", "L\'", "L2", "D", "D\'", "D2"]

frontFace = 0
leftFace = 1
upFace = 2
rightFace = 3
downFace = 4
backFace = 5

newCube :: Cube
newCube = ["WWWWWWWWW", "GGGGGGGGG", "OOOOOOOOO", "BBBBBBBBB", "RRRRRRRRR", "YYYYYYYYY"]

cubeToString :: Cube -> Face
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

rtxOnCube :: Cube -> IO ()
rtxOnCube = putCubeColor' . rtxOnCube' 0 rtxOn2 . foldl1 (++) . take 3 
    where  
        rtxOn = "\n              %JJJJJJJJJ%@\n      ,@%ZZZZZZZZZ#@&%KKKKKKKKK%*\n@@PPPPPPPP#%@@%NNNNNNNNN%@@&LLLLLLLLL%@\n11111@@@QQQQQQQQQ#%@@UUUUUUUUUUU&@@CCCC&\n11111@%222@@@@++++++++++++@@@@SSS@CCCCC@\n*1111@@22222@333@@@@&@@&AA@%SSSSS@CCCCC@\n@.@@&@@22222@333333@%AAAAA&&SSSSS@&@@@%#\n#44444@@@@@5@333333@%AAAAA@%S%@@@@XXXXX\n444444@55555@@@@666@&AA@@@@EEEEEE@XXXXX\n&@@444@55555@66666@@@DDDDD@EEEEEE@XXX@@,\n*7777*@@/888@666666@DDDDDD@EEEE%@%(IIII\n 77777@8888@@@66666@DDDDDD@@@HHH&@IIIII\n *7777@88888@99 @@@@@@@TTT@HHHHHH%@III&\n      (88888@99999@@TTTTTT@HHHHH@&\n           #@99999&@TTTTTT@&(\n               *&&&@TT@@\n"
        rtxOn2 = "\n                                    ::JJ::\n                              ::JJJJJJJJJJJJJ::\n                         ::*VFVVVJJJJJJJJJJJVVVFFV:::\n                   :::ZZZZZZZVVFFMFFVV*VVFFMMFFVVKKKKKKKK:: \n            :::*VVVZZZZZZZZZZZZZZZVFMMMMMFVKKKKKKKKKKKKKKKKV**::::\n       ::PPPPPPVVFFFFVVVZZZZZVVFFFFFVVNNVVFFFFVVKKKKKKVVFFFFVVLLLLLLL::\n :PPPPPPPPPPPPPPPPPPVVFFFFFMMFFVVNNNNNNNNNNNNVVFFFFFFFFVVLLLLLLLLLLLLLLLLLL:\n:FMMMFVVVPPPPPPPVVVFFMMFFVVFFFFVVVNNNNNNNNNNVVFFMFFVFFMMMFVVVLLLLLLLLVVFMMMM*\n:V11VVMMMMFVVFFMMFFVVQQQQQQQQQVVFFFFFNNNNFMMFFVUUUUUUUUUVVFFMMFFVFFMMMMMFFCF*\n V1111111VF$M$MMVVQQQQQQQQQQQQQQVVVFMMFMMMVVUUUUUUUUUUUUUUUVVFM$$MMFFCCCCCCF*\n V111111111MMMFM$$MMFVVVQQQVVFFMMFFVV+VVVVFFFFFVVUUUUUVVFMM$$MMMMMCCCCCCCCCF*\n V111111111MMF2222VFM$$MMFMMMFVV++++++++++VVVVVFMMFFFM$$$MMFFSSFMMCCCCCCCCCF:\n 1111111111MMF222222222VMMMMMFVVV+++++++++VVVVFMM$$$MMFFSSSSSSSFMMCCCCCCCCCF:\n 1111111111FMM2222222222MMVVVMMMMMFVVVVVVFMMM$$MMFF$$FSSSSSSSSSFMMCCCCCCCCCF:\n 1111111111FMM2222222222MMV33333VFM$$MMM$$MMFFAAAAF$$FSSSSSSSSSMMMCCCCCCCCFF:\n :MMMFV1111FMM2222222222MM3333333333VMMMFFAAAAAAAAF$$FSSSSSSSSSMMMCCCCFFMMMV\n :44VVMMMMFF$M2222222222MM33333333333MMMFAAAAAAAAAF$$FSSSSSSSSSMMMFMM$$$MMFM\n :4444444VF$M$FV22222222M$33333333333MMMFAAAAAAAAAFM$FSSSSSSSSFMM$$MMFFXXXXF\n :V44444444VM$M$$MFV2222M$33333333333MMMFAAAAAAAAAF$$FSSSFFMM$$M$MXXXXXXXXXX\n  V44444444V$M55VFM$$MFFM$33333333333MMMFAAAAAAAAAF$$MMM$$MMFFFM$FXXXXXXXXXX\n  V44444444V$M5555555VFMMMMFV33333333MMMFAAAAAAFFMMM$$MMFFEEEEEM$FXXXXXXXXX*\n  V444444444$$5555555555$MM$M$MFVV333M$MFAAFFMM$MM$MMFEEEEEEEEEM$FXXXXXXXXX*\n  V444444444$$5555555555$M666VFM$M$MFM$MMMMMMMFFDF$MFEEEEEEEEEEM$FXXXXXXXXF*\n :M$MFV44444$$5555555555$$6666666VFM$M$MMFFDDDDDDF$$FEEEEEEEEEEM$XXXXXFFMM$I\n :V77VFM$MFF$$5555555555$$6666666666FMMFDDDDDDDDDFM$MEEEEEEEEEFMMFFMM$$MMFII\n  V777777VFM$$FV55555555$$6666666666FM$FDDDDDDDDDDM$MEEEEEEEEEF$$$$MMFIIIIII\n  V777777777$MM$$MVV5555$$6666666666VM$FDDDDDDDDDDM$MEEEEEFMM$M$MMIIIIIIIII*\n  V777777777$M888VFM$MFV$$V666666666V$$FDDDDDDDDDDM$MFFMM$MMFFF$$FIIIIIIIII*\n  V777777777$M8888888VFMMMMV66666666V$$FDDDDDDDDFFM$$MMFFHHHHHF$$FIIIIIIIII*\n  V777777777$$888888888V$MFM$MFV6666V$$FDDDFFMMMMMMMFHHHHHHHHHF$$FIIIIIIIIF*\n  :777777777$$8888888888$M999VF$$MFVF$$FFMMMMMFFTFMMHHHHHHHHHHF$$FIIIIIIIFV:\n    ::777777$M8888888888$M999999VVM$$M$MMFFFTTTTTFMMHHHHHHHHHHF$MFIIIFFV*:\n        :77VM$V888888888$M9999999999$$FTTTTTTTTTTFMMHHHHHHHHHHF$MFII*:\n            ::*VV8888888$M9999999999M$MTTTTTTTTTTFM$FHHHHHHHHFV**:\n                :**VV888M$9999999999M$FTTTTTTTTTTFM$HHHHFFV**:\n                    :*VVMM9999999999M$MTTTTTTTTTTFM$HHV**\n                        :*VV99999999M$MTTTTTTTTFFFV*:\n                           :*VVV9999M$MTTTTFFFV*:\n                               :*VV*M$MFFT**:\n                                  :**V*:\n"

rtxOnIndex = ['1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'S', 'C', 'D', 'E', 'X', 'T', 'H', 'I', 'J', 'K', 'L', 'Z', 'N', 'U', 'P', 'Q', '+']

rtxOnCube' :: Int -> String -> String -> String
rtxOnCube' _ rtxOn [] = rtxOn
rtxOnCube' i rtxOn (x:xs) = rtxOnCube' (i + 1) (replace (rtxOnIndex !! i) x rtxOn) xs

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace a b (x:xs)
    | x == a = b : replace a b xs
    | otherwise = x : replace a b xs

turn ::  [Int] -> Face -> Face
turn [] _ = []
turn (x:xs) face = face !! x : turn xs face

turnClock :: Face -> Face
turnClock = turn [6, 3, 0, 7, 4, 1, 8, 5, 2]

turnCounterClock :: Face -> Face
turnCounterClock = turn [2, 5, 8, 1, 4, 7, 0, 3, 6]

moveF :: Cube -> Cube
moveF myCube = face0:face1:face2:face3:face4:(last myCube):[]
    where
        face0 = turnClock (myCube !! frontFace)
        face1 = [myCube !! leftFace !! 0, myCube !! leftFace !! 1, myCube !! downFace !! 0, myCube !! leftFace !! 3, myCube !! leftFace !! 4, myCube !! downFace !! 1, myCube !! leftFace !! 6, myCube !! leftFace !! 7, myCube !! downFace !! 2]
        face2 = take 6 (myCube !! upFace) ++ [myCube !! leftFace !! 8, myCube !! leftFace !! 5, myCube !! leftFace !! 2]
        face3 = [myCube !! upFace !! 6, myCube !! rightFace !! 1, myCube !! rightFace !! 2, myCube !! upFace !! 7, myCube !! rightFace !! 4, myCube !! rightFace !! 5, myCube !! upFace !! 8, myCube !! rightFace !! 7, myCube !! rightFace !! 8]
        face4 = [myCube !! rightFace !! 6, myCube !! rightFace !! 3, myCube !! rightFace !! 0] ++ drop 3 (myCube !! downFace)

moveF2 :: Cube -> Cube
moveF2 myCube = face0:face1:face2:face3:face4:(last myCube):[]
    where
        face0 = reverse $ myCube !! frontFace
        face1 = [myCube !! leftFace !! 0, myCube !! leftFace !! 1, myCube !! rightFace !! 6, myCube !! leftFace !! 3, myCube !! leftFace !! 4, myCube !! rightFace !! 3, myCube !! leftFace !! 6, myCube !! leftFace !! 7, myCube !! rightFace !! 0]
        face2 = take 6 (myCube !! upFace) ++ (reverse $ take 3 (myCube !! downFace))
        face3 = [myCube !! leftFace !! 8, myCube !! rightFace !! 1, myCube !! rightFace !! 2, myCube !! leftFace !! 5, myCube !! rightFace !! 4, myCube !! rightFace !! 5, myCube !! leftFace !! 2, myCube !! rightFace !! 7, myCube !! rightFace !! 8]
        face4 = [myCube !! upFace !! 8, myCube !! upFace !! 7, myCube !! upFace !! 6] ++ drop 3 (myCube !! downFace)

moveF' :: Cube -> Cube
moveF' myCube = face0:face1:face2:face3:face4:(last myCube):[]
    where
        face0 = turnCounterClock (myCube !! frontFace)
        face1 = [myCube !! leftFace !! 0, myCube !! leftFace !! 1, myCube !! upFace !! 8, myCube !! leftFace !! 3, myCube !! leftFace !! 4, myCube !! upFace !! 7, myCube !! leftFace !! 6, myCube !! leftFace !! 7, myCube !! upFace !! 6]
        face2 = take 6 (myCube !! upFace) ++ [myCube !! rightFace !! 0, myCube !! rightFace !! 3, myCube !! rightFace !! 6]
        face3 = [myCube !! downFace !! 2, myCube !! rightFace !! 1, myCube !! rightFace !! 2, myCube !! downFace !! 1, myCube !! rightFace !! 4, myCube !! rightFace !! 5, myCube !! downFace !! 0, myCube !! rightFace !! 7, myCube !! rightFace !! 8]
        face4 = [myCube !! leftFace !! 2, myCube !! leftFace !! 5, myCube !! leftFace !! 8] ++ drop 3 (myCube !! downFace)

moveR :: Cube -> Cube
moveR myCube = face0:(myCube !! leftFace):face2:face3:face4:face5:[]
        where
        face0 = [myCube !! frontFace !! 0, myCube !! frontFace !! 1, myCube !! downFace !! 2, myCube !! frontFace !! 3, myCube !! frontFace !! 4, myCube !! downFace !! 5, myCube !! frontFace !! 6, myCube !! frontFace !! 7, myCube !! downFace !! 8]
        face2 = [myCube !! upFace !! 0, myCube !! upFace !! 1, myCube !! frontFace !! 2, myCube !! upFace !! 3, myCube !! upFace !! 4, myCube !! frontFace !! 5, myCube !! upFace !! 6, myCube !! upFace !! 7, myCube !! frontFace !! 8]
        face3 = turnClock (myCube !! rightFace)
        face4 = [myCube !! downFace !! 0, myCube !! downFace !! 1, myCube !! backFace !! 2, myCube !! downFace !! 3, myCube !! downFace !! 4, myCube !! backFace !! 5, myCube !! downFace !! 6, myCube !! downFace !! 7, myCube !! backFace !! 8]
        face5 = [myCube !! backFace !! 0, myCube !! backFace !! 1, myCube !! upFace !! 2, myCube !! backFace !! 3, myCube !! backFace !! 4, myCube !! upFace !! 5, myCube !! backFace !! 6, myCube !! backFace !! 7, myCube !! upFace !! 8]

moveR2 :: Cube -> Cube
moveR2 myCube = face0:(myCube !! leftFace):face2:face3:face4:face5:[]
        where
        face0 = [myCube !! frontFace !! 0, myCube !! frontFace !! 1, myCube !! backFace !! 2, myCube !! frontFace !! 3, myCube !! frontFace !! 4, myCube !! backFace !! 5, myCube !! frontFace !! 6, myCube !! frontFace !! 7, myCube !! backFace !! 8]
        face2 = [myCube !! upFace !! 0, myCube !! upFace !! 1, myCube !! downFace !! 2, myCube !! upFace !! 3, myCube !! upFace !! 4, myCube !! downFace !! 5, myCube !! upFace !! 6, myCube !! upFace !! 7, myCube !! downFace !! 8]
        face3 = reverse $ myCube !! rightFace
        face4 = [myCube !! downFace !! 0, myCube !! downFace !! 1, myCube !! upFace !! 2, myCube !! downFace !! 3, myCube !! downFace !! 4, myCube !! upFace !! 5, myCube !! downFace !! 6, myCube !! downFace !! 7, myCube !! upFace !! 8]
        face5 = [myCube !! backFace !! 0, myCube !! backFace !! 1, myCube !! frontFace !! 2, myCube !! backFace !! 3, myCube !! backFace !! 4, myCube !! frontFace !! 5, myCube !! backFace !! 6, myCube !! backFace !! 7, myCube !! frontFace !! 8]

moveR' :: Cube -> Cube
moveR' myCube = face0:(myCube !! leftFace):face2:face3:face4:face5:[]
        where
        face0 = [myCube !! frontFace !! 0, myCube !! frontFace !! 1, myCube !! upFace !! 2, myCube !! frontFace !! 3, myCube !! frontFace !! 4, myCube !! upFace !! 5, myCube !! frontFace !! 6, myCube !! frontFace !! 7, myCube !! upFace !! 8]
        face2 = [myCube !! upFace !! 0, myCube !! upFace !! 1, myCube !! backFace !! 2, myCube !! upFace !! 3, myCube !! upFace !! 4, myCube !! backFace !! 5, myCube !! upFace !! 6, myCube !! upFace !! 7, myCube !! backFace !! 8]
        face3 = turnCounterClock (myCube !! rightFace)
        face4 = [myCube !! downFace !! 0, myCube !! downFace !! 1, myCube !! frontFace !! 2, myCube !! downFace !! 3, myCube !! downFace !! 4, myCube !! frontFace !! 5, myCube !! downFace !! 6, myCube !! downFace !! 7, myCube !! frontFace !! 8]
        face5 = [myCube !! backFace !! 0, myCube !! backFace !! 1, myCube !! downFace !! 2, myCube !! backFace !! 3, myCube !! backFace !! 4, myCube !! downFace !! 5, myCube !! backFace !! 6, myCube !! backFace !! 7, myCube !! downFace !! 8]

moveL :: Cube -> Cube
moveL myCube =  face0:face1:face2:(myCube !! rightFace):face4:face5:[]
    where
        face0 = [myCube !! upFace !! 0, myCube !! frontFace !! 1, myCube !! frontFace !! 2, myCube !! upFace !! 3, myCube !! frontFace !! 4, myCube !! frontFace !! 5, myCube !! upFace !! 6, myCube !! frontFace !! 7, myCube !! frontFace !! 8]
        face1 = turnClock (myCube !! leftFace)
        face2 = [myCube !! backFace !! 0, myCube !! upFace !! 1, myCube !! upFace !! 2, myCube !! backFace !! 3, myCube !! upFace !! 4, myCube !! upFace !! 5, myCube !! backFace !! 6, myCube !! upFace !! 7, myCube !! upFace !! 8]
        face4 = [myCube !! frontFace !! 0, myCube !! downFace !! 1, myCube !! downFace !! 2, myCube !! frontFace !! 3, myCube !! downFace !! 4, myCube !! downFace !! 5, myCube !! frontFace !! 6, myCube !! downFace !! 7, myCube !! downFace !! 8]
        face5 = [myCube !! downFace !! 0, myCube !! backFace !! 1, myCube !! backFace !! 2, myCube !! downFace !! 3, myCube !! backFace !! 4, myCube !! backFace !! 5, myCube !! downFace !! 6, myCube !! backFace !! 7, myCube !! backFace !! 8]

moveL2 :: Cube -> Cube
moveL2 myCube =  face0:face1:face2:(myCube !! rightFace):face4:face5:[]
    where
        face0 = [myCube !! backFace !! 0, myCube !! frontFace !! 1, myCube !! frontFace !! 2, myCube !! backFace !! 3, myCube !! frontFace !! 4, myCube !! frontFace !! 5, myCube !! backFace !! 6, myCube !! frontFace !! 7, myCube !! frontFace !! 8]
        face1 = reverse $ myCube !! leftFace
        face2 = [myCube !! downFace !! 0, myCube !! upFace !! 1, myCube !! upFace !! 2, myCube !! downFace !! 3, myCube !! upFace !! 4, myCube !! upFace !! 5, myCube !! downFace !! 6, myCube !! upFace !! 7, myCube !! upFace !! 8]
        face4 = [myCube !! upFace !! 0, myCube !! downFace !! 1, myCube !! downFace !! 2, myCube !! upFace !! 3, myCube !! downFace !! 4, myCube !! downFace !! 5, myCube !! upFace !! 6, myCube !! downFace !! 7, myCube !! downFace !! 8]
        face5 = [myCube !! frontFace !! 0, myCube !! backFace !! 1, myCube !! backFace !! 2, myCube !! frontFace !! 3, myCube !! backFace !! 4, myCube !! backFace !! 5, myCube !! frontFace !! 6, myCube !! backFace !! 7, myCube !! backFace !! 8]

moveL' :: Cube -> Cube
moveL' myCube = face0:face1:face2:(myCube !! rightFace):face4:face5:[]
    where
        face0 = [myCube !! downFace !! 0, myCube !! frontFace !! 1, myCube !! frontFace !! 2, myCube !! downFace !! 3, myCube !! frontFace !! 4, myCube !! frontFace !! 5, myCube !! downFace !! 6, myCube !! frontFace !! 7, myCube !! frontFace !! 8]
        face1 = turnCounterClock (myCube !! leftFace)
        face2 = [myCube !! frontFace !! 0, myCube !! upFace !! 1, myCube !! upFace !! 2, myCube !! frontFace !! 3, myCube !! upFace !! 4, myCube !! upFace !! 5, myCube !! frontFace !! 6, myCube !! upFace !! 7, myCube !! upFace !! 8]
        face4 = [myCube !! backFace !! 0, myCube !! downFace !! 1, myCube !! downFace !! 2, myCube !! backFace !! 3, myCube !! downFace !! 4, myCube !! downFace !! 5, myCube !! backFace !! 6, myCube !! downFace !! 7, myCube !! downFace !! 8]
        face5 = [myCube !! upFace !! 0, myCube !! backFace !! 1, myCube !! backFace !! 2, myCube !! upFace !! 3, myCube !! backFace !! 4, myCube !! backFace !! 5, myCube !! upFace !! 6, myCube !! backFace !! 7, myCube !! backFace !! 8]

moveU :: Cube -> Cube
moveU myCube = face0:face1:face2:face3:(myCube !! downFace):face5:[]
    where
        face0 = take 3 (myCube !! rightFace) ++ drop 3 (myCube !! frontFace)
        face1 = take 3 (myCube !! frontFace) ++ drop 3 (myCube !! leftFace)
        face2 = turnClock (myCube !! upFace)
        face3 = (reverse $ drop 6 (myCube !! backFace)) ++ drop 3 (myCube !! rightFace)
        face5 = take 6 (myCube !! backFace) ++ (reverse $ take 3 (myCube !! leftFace))

moveU2 :: Cube -> Cube
moveU2 myCube = face0:face1:face2:face3:(myCube !! downFace):face5:[]
    where
        face0 = (reverse $ drop 6 (myCube !! backFace)) ++ drop 3 (myCube !! frontFace)
        face1 = take 3 (myCube !! rightFace) ++ drop 3 (myCube !! leftFace)
        face2 = reverse (myCube !! upFace)
        face3 = take 3 (myCube !! leftFace) ++ drop 3 (myCube !! rightFace)
        face5 = take 6 (myCube !! backFace) ++ (reverse $ take 3 (myCube !! frontFace))

moveU' :: Cube -> Cube
moveU' myCube = face0:face1:face2:face3:(myCube !! downFace):face5:[]
    where
        face0 = take 3 (myCube !! leftFace) ++ drop 3 (myCube !! frontFace)
        face1 = (reverse $ drop 6 (myCube !! backFace)) ++ drop 3 (myCube !! leftFace)
        face2 = turnCounterClock (myCube !! upFace)
        face3 = take 3 (myCube !! frontFace) ++ drop 3 (myCube !! rightFace)
        face5 = take 6 (myCube !! backFace) ++ (reverse $ take 3 (myCube !! rightFace))

moveD :: Cube -> Cube
moveD myCube = face0:face1:(myCube !! upFace):face3:face4:face5:[]
    where
        face0 = take 6 (myCube !! frontFace) ++ drop 6 (myCube !! leftFace)
        face1 = take 6 (myCube !! leftFace) ++ (reverse $ take 3 (myCube !! backFace))
        face3 = take 6 (myCube !! rightFace) ++ drop 6 (myCube !! frontFace)
        face4 = turnClock (myCube !! downFace)
        face5 = (reverse $ drop 6 (myCube !! rightFace)) ++ drop 3 (myCube !! backFace)

moveD2 :: Cube -> Cube
moveD2 myCube = face0:face1:(myCube !! upFace):face3:face4:face5:[]
    where
        face0 = take 6 (myCube !! frontFace) ++ (reverse $ take 3 (myCube !! backFace))
        face1 = take 6 (myCube !! leftFace) ++ drop 6 (myCube !! rightFace)
        face3 = take 6 (myCube !! rightFace) ++ drop 6 (myCube !! leftFace)
        face4 = reverse $ myCube !! downFace
        face5 = (reverse $ drop 6 (myCube !! frontFace)) ++ drop 3 (myCube !! backFace)

moveD' :: Cube -> Cube
moveD' myCube = face0:face1:(myCube !! upFace):face3:face4:face5:[]
    where
        face0 = take 6 (myCube !! frontFace) ++ drop 6 (myCube !! rightFace)
        face1 = take 6 (myCube !! leftFace) ++ drop 6 (myCube !! frontFace)
        face3 = take 6 (myCube !! rightFace) ++ (reverse $ take 3 (myCube !! backFace))
        face4 = turnCounterClock (myCube !! downFace)
        face5 = (reverse $ drop 6 (myCube !! leftFace)) ++ drop 3 (myCube !! backFace)

moveB :: Cube -> Cube
moveB myCube = (head myCube):face1:face2:face3:face4:face5:[]
    where
        face1 = [myCube !! upFace !! 2, myCube !! leftFace !! 1, myCube !! leftFace !! 2, myCube !! upFace !! 1, myCube !! leftFace !! 4, myCube !! leftFace !! 5, myCube !! upFace !! 0, myCube !! leftFace !! 7, myCube !! leftFace !! 8]
        face2 = [myCube !! rightFace !! 2, myCube !! rightFace !! 5, myCube !! rightFace !! 8] ++ drop 3 (myCube !! upFace)
        face3 = [myCube !! rightFace !! 0, myCube !! rightFace !! 1, myCube !! downFace !! 8, myCube !! rightFace !! 3, myCube !! rightFace !! 4, myCube !! downFace !! 7, myCube !! rightFace !! 6, myCube !! rightFace !! 7, myCube !! downFace !! 6]
        face4 = take 6 (myCube !! downFace) ++ [myCube !! leftFace !! 0, myCube !! leftFace !! 3, myCube !! leftFace !! 6]
        face5 = turnClock (myCube !! backFace)

moveB2 :: Cube -> Cube
moveB2 myCube = (head myCube):face1:face2:face3:face4:face5:[]
    where
        face1 = [myCube !! rightFace !! 8, myCube !! leftFace !! 1, myCube !! leftFace !! 2, myCube !! rightFace !! 5, myCube !! leftFace !! 4, myCube !! leftFace !! 5, myCube !! rightFace !! 2, myCube !! leftFace !! 7, myCube !! leftFace !! 8]
        face2 = (reverse $ drop 6 (myCube !! downFace)) ++ drop 3 (myCube !! upFace)
        face3 = [myCube !! rightFace !! 0, myCube !! rightFace !! 1, myCube !! leftFace !! 6, myCube !! rightFace !! 3, myCube !! rightFace !! 4, myCube !! leftFace !! 3, myCube !! rightFace !! 6, myCube !! rightFace !! 7, myCube !! leftFace !! 0]
        face4 = take 6 (myCube !! downFace) ++ (reverse $ take 3 (myCube !! upFace))
        face5 = reverse $ myCube !! backFace


moveB' :: Cube -> Cube
moveB' myCube = (head myCube):face1:face2:face3:face4:face5:[]
    where
        face1 = [myCube !! downFace !! 6, myCube !! leftFace !! 1, myCube !! leftFace !! 2, myCube !! downFace !! 7, myCube !! leftFace !! 4, myCube !! leftFace !! 5, myCube !! downFace !! 8, myCube !! leftFace !! 7, myCube !! leftFace !! 8]
        face2 = [myCube !! leftFace !! 6, myCube !! leftFace !! 3, myCube !! leftFace !! 0] ++ drop 3 (myCube !! upFace)
        face3 = [myCube !! rightFace !! 0, myCube !! rightFace !! 1, myCube !! upFace !! 0, myCube !! rightFace !! 3, myCube !! rightFace !! 4, myCube !! upFace !! 1, myCube !! rightFace !! 6, myCube !! rightFace !! 7, myCube !! upFace !! 2]
        face4 = take 6 (myCube !! downFace) ++ [myCube !! rightFace !! 8, myCube !! rightFace !! 5, myCube !! rightFace !! 2]
        face5 = turnCounterClock (myCube !! backFace)

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
    | key == 1 = [MFront ONothing, MFront ODirection, MFront OStrokes, MRight OStrokes, MUp ONothing, MUp ODirection, MUp OStrokes, MBack ONothing, MBack ODirection, MBack OStrokes, MLeft OStrokes, MDown ONothing, MDown ODirection, MDown OStrokes]
    | key == 2 = [MFront OStrokes, MRight OStrokes, MUp ONothing, MUp ODirection, MUp OStrokes, MBack OStrokes, MLeft OStrokes, MDown ONothing, MDown ODirection, MDown OStrokes]
    | key == 3 = [MFront OStrokes, MRight OStrokes, MUp OStrokes, MBack OStrokes, MLeft OStrokes, MDown OStrokes]
    | key == 4 = []
   
makeMoves :: [Move] -> Cube -> Cube
makeMoves moves cube = foldl (\ newCube f -> f newCube) cube $ map moveToAction moves