module Day3 where

import Debug.Trace

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Text.Parsec.Number (int)
import Text.Parsec.Char

import Data.Maybe
import Data.List
import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as S

import Solution 

solution :: Solution [Claim] Int
solution = Solution {
    name = "Day 3",
    parser = inputParser,
    parts = [
        part1,
        part2
    ],
    examples = [
        ("#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2", [4, 3])
    ],
    inputFile = "./data/days/Day3.in"
}


-----------------
-- Data
-----------------

data Claim = Claim {
    claimId :: Int,
    claimLeft :: Int,
    claimTop :: Int,
    claimWidth :: Int,
    claimHeight :: Int
}
    deriving (Show, Eq)

data Overlap = Overlap {
    ovLeft :: Int,
    ovTop :: Int,
    ovWidth :: Int,
    ovHeight :: Int
}
    deriving (Show, Eq)

-----------------
-- Solution
-----------------

emptyOverlap :: Overlap
emptyOverlap = Overlap 0 0 0 0

part1 :: [Claim] -> Int
part1 = getTotalOverlappingArea . getAllOverlaps

claimToOverlap :: Claim -> Overlap
claimToOverlap (Claim _ x y w h) = Overlap x y w h

getClaimOverlap :: Claim -> Claim -> Overlap
getClaimOverlap (Claim _ x1 y1 w1 h1) (Claim _ x2 y2 w2 h2)
    | x1 + w1 <= x2 = emptyOverlap
    | x2 + w2 <= x1 = emptyOverlap
    | y1 + h1 <= y2 = emptyOverlap
    | y2 + h2 <= y1 = emptyOverlap
    | otherwise = Overlap xi yi wi hi
    where xi = max x1 x2
          yi = max y1 y2
          wi = (min (x1 + w1) (x2 + w2)) - xi
          hi = (min (y1 + h1) (y2 + h2)) - yi

getClaimOverlaps :: Claim -> [Claim] -> [Overlap]
getClaimOverlaps c others = foldr (\c2 acc -> (getClaimOverlap c c2) : acc) [] others

getAllOverlaps :: [Claim] -> [Overlap]
getAllOverlaps [] = []
getAllOverlaps (c:cs) = (getClaimOverlaps c cs) ++ (getAllOverlaps cs)

getOverlapPoints :: Overlap -> [(Int, Int)]
getOverlapPoints overlap = go overlap (ovHeight overlap)
    where go (Overlap _ _ 0 _) _  = []
          go (Overlap _ _ _ 0) _  = []
          go (Overlap x y 1 1) _  = [(x, y)]
          go (Overlap x y w 1) oh = (x + w - 1, y) : (go (Overlap x y (w - 1) oh) oh)
          go (Overlap x y w h) oh  = (x + w - 1, y + h - 1) : (go (Overlap x y w (h - 1)) oh)

getTotalOverlappingArea :: [Overlap] -> Int
getTotalOverlappingArea overlaps = go overlaps S.empty
    where go [] s     = S.size s
          go (o:os) s = go os (foldr (\pt acc -> S.insert pt acc) s (getOverlapPoints o))

part2 :: [Claim] -> Int
part2 = (maybe (-1) claimId) . findClaimWithNoOverlap

findClaimWithNoOverlap :: [Claim] -> Maybe Claim
findClaimWithNoOverlap cs = go cs (length cs - 1)
    where go cs (-1) = Nothing
          go cs i = if onlyEmptyClaims then Just c else go cs (i - 1)
            where onlyEmptyClaims = not $ any (\(Overlap _ _ w h) -> w /= 0 && h /= 0) (getClaimOverlaps c (delete c cs))
                  c = cs !! i

-----------------
-- Input Parsing
-----------------

claimParser :: Parser Claim
claimParser = do
    char '#'
    cId <- int
    string " @ "
    cLeft <- int
    char ','
    cTop <- int
    string ": "
    cWidth <- int
    char 'x'
    cHeight <- int
    return $ Claim cId cLeft cTop cWidth cHeight

inputParser :: Parser [Claim]
inputParser = claimParser `sepBy` (char '\n')