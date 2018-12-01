module Day1 where

import Debug.Trace

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Text.Parsec.Number (int)
import Text.Parsec.Char

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as S

import Solution 

solution :: Solution FreqSeq Int
solution  = Solution {
    name = "Day 1",
    parser = inputParser,
    parts = [
        part1,
        part2
    ],
    examples = [
        -- ("+1, -2, +3, +1", [3, 0]),
        -- ("+1, +1, +1", [3, 0]),
        -- ("+1, +1, -2", [0, 0]),
        -- ("-1, -2, -3", [-6, 0]),
        ("+1, -1", [0, 0]),
        ("+3, +3, +4, -2, -4", [4, 10]),
        ("-6, +3, +8, +5, -6", [4, 5]),
        ("+7, +7, -2, -7, -4", [1, 14])
    ],
    inputFile = "./data/days/Day1.in"
}


-----------------
-- Data
-----------------

data Op = Plus Int | Minus Int
    deriving (Show)

type FreqSeq = [Op]

-----------------
-- Solution
-----------------

part1 :: FreqSeq -> Int
part1 = foldl performOp 0

part2 :: FreqSeq -> Int
part2 fseq = evalState (findFirstRepeatedFreq 0 fseq 0) S.empty

performOp :: Int -> Op -> Int
performOp f (Plus n) = f + n
performOp f (Minus n) = f - n
    
findFirstRepeatedFreq :: Int -> FreqSeq -> Int -> State (Set Int) Int
findFirstRepeatedFreq f fseq idx = do
    s <- get
    put $ S.insert f s
    let nextF = performOp f (fseq !! idx)
    if S.member nextF (S.insert f s) then
        return nextF
    else
        findFirstRepeatedFreq nextF fseq ((idx + 1) `mod` (length fseq))
    
-----------------
-- Input Parsing
-----------------

plusParser :: Parser Op
plusParser = do
    char '+'
    x <- int
    return $ Plus x

minusParser :: Parser Op
minusParser = do
    char '-'
    x <- int
    return $ Minus x

opParser :: Parser Op
opParser =  plusParser <|> minusParser

inputParser :: Parser FreqSeq
inputParser = opParser `sepBy` (string ", " <|> (string "\n"))