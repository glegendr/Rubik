module Lexer
( Lex(..)
, lexMe
, getShuffle
) where

data Lex = LexError !String | LexFlag !String | LexMoves ![String] deriving (Eq)
allFlags = ["-h", "--help"]
allMoves = ["F", "F\'", "F2", "R", "R\'", "R2", "U", "U\'", "U2", "B", "B\'", "B2", "L", "L\'", "L2", "D", "D\'", "D2"]

lexMe :: [String] -> [Lex]
lexMe args
    | lex == [] = [LexError "No Argument given"]
    | isError == True = [last lex]
    | count == 0 = [LexError "No Shuffle given"]
    | count > 1 = [LexError "Multiple Shuffle given"]
    | otherwise = lex
    where
        lex = createLex args
        isError = case last lex of
            LexError _ -> True
            otherwise -> False
        count = foldl (\ acc x -> case x of
            LexMoves _ -> acc + 1
            otherwise -> acc
            ) 0 lex

createLex :: [String] -> [Lex]
createLex [] = []
createLex (x:xs)
    | x == [] = [LexError "Empty Argument"]
    | x `elem` allFlags = LexFlag x : createLex xs
    | head x == '-' = [LexError "Unknown Flag"]
    | True == foldl (\ isOk move -> if move `elem` allMoves then isOk else False) True (words x) = LexMoves (words x) : createLex xs
    | otherwise = [(LexError ("Unknown Move \"" ++ head (filter ([] /=) $ map (\ z -> if z `elem` allMoves then [] else z) (words x)) ++ "\""))]

getShuffle :: [Lex] -> [String]
getShuffle [] = []
getShuffle ((LexMoves moves):_) = moves
getShuffle (_:xs) = getShuffle xs 
