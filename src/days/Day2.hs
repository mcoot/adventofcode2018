module Day2 where

import Debug.Trace

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Text.Parsec.Number (int)
import Text.Parsec.Char

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Solution 

solutionPt1 :: Solution [String] Int
solutionPt1  = Solution {
    name = "Day 2 Pt 1",
    parser = inputParser,
    parts = [
        part1
    ],
    examples = [
        ("abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab", [12])
    ],
    inputFile = "./data/days/Day2.in"
}

solutionPt2 :: Solution [String] String
solutionPt2  = Solution {
    name = "Day 2 Pt 2",
    parser = inputParser,
    parts = [
        part2
    ],
    examples = [
        ("abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz", ["fgij"])
    ],
    inputFile = "./data/days/Day2.in"
}

-----------------
-- Data
-----------------

-----------------
-- Solution
-----------------

hasLetterNTimes :: Int -> String -> Bool
hasLetterNTimes n s = go s M.empty 
    where go [] m     = any (\v -> v == n) (M.elems m)
          go (c:cs) m = go cs (M.insert c ((M.findWithDefault 0 c m) + 1) m)
                            

getCountsFor2And3 :: [String] -> (Int, Int)
getCountsFor2And3 = foldr 
                        (\s (count2, count3) -> 
                            (count2 + (fromEnum $ hasLetterNTimes 2 s), count3 + (fromEnum $ hasLetterNTimes 3 s)))
                        (0, 0)

part1 :: [String] -> Int
part1 input = twos * threes
    where (twos, threes) = getCountsFor2And3 input

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

addBlank :: String -> Int -> String
addBlank str a = (slice 0 a str) ++ "_" ++ (slice (a + 1) (length str) str)

getBlankedVersions :: String -> Set String
getBlankedVersions str = S.fromList $ (addBlank str) <$> [0..(length str)]

part2 :: [String] -> String
part2 strs = go strs S.empty
    where go [] _ = "[FAILED]"
          go (str:strs) s = if not $ S.null common then
                                [c | c <- S.elemAt 0 common, c /= '_']
                            else
                                go strs (S.union s blanked)
            where blanked = getBlankedVersions str
                  common  = S.intersection s blanked
    
-----------------
-- Input Parsing
-----------------

inputParser :: Parser [String]
inputParser = (many alphaNum) `sepBy` (char '\n')