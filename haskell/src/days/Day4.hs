{-# LANGUAGE QuasiQuotes #-}

module Day4 where

import Debug.Trace

import Text.RawString.QQ

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Text.Parsec.Number (int)
import Text.Parsec.Char

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock

import Data.Maybe
import Data.Either
import Data.List
import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as S

import Solution 

solution :: Solution [Record] Int
solution = Solution {
    name = "Day 4",
    parser = inputParser,
    parts = [
        part1,
        part2
    ],
    examples = [
        ([r|[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up|], [4, 3])
    ],
    inputFile = "./data/days/Day4.in"
}


-----------------
-- Data
-----------------

data Action = StartsShift | FallsAsleep | WakesUp
    deriving (Show, Eq)

data Record = Record {
    rTime :: UTCTime,
    rGuard :: Maybe Int,
    rAction :: Action
}
    deriving (Show, Eq)

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

-----------------
-- Solution
-----------------

part1 :: [Record] -> Int
part1 = undefined

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

-- Sort records and fill in guard IDs
sortRecords :: [Record] -> [Record]
sortRecords = (fillIn 0) . (sortBy (\r1 r2 -> compare (rTime r1) (rTime r2)))
    where fillIn i rs
            | i >= (length rs) = rs
            | otherwise        = fillIn (i + 1) newRs
            where r = rs !! i
                  newGuard = if isNothing $ rGuard r then
                                 rGuard (rs !! (i - 1))
                             else
                                 rGuard r
                  newRs = (slice 0 i rs) ++ ((Record (rTime r) newGuard (rAction r)) : (slice (i + 1) (length rs) rs))

-- Minutes asleep
minutesAsleep :: [Record] -> Int -> Int
minutesAsleep rs gId = minutesAsleepHlpr rs Gid zeroTime False False

isRecordForGuard :: Record -> Int -> Bool
isRecordForGuard (Record _ g _) gId = case g of
                                          Just actualG -> actualG = gId
                                          Nothing -> False

minutesAsleepHlpr :: [Record] -> Int -> UTCTime -> Bool -> Bool -> Int
minutesAsleepHlpr [] gId lastTime isOnShift isAwake                  = 0
minutesAsleepHlpr ((Record t g a):rs) gId lastTime isOnShift isAwake = 
                            addedTime + (minutesAsleepHlpr rs gId t onShiftNext isAwakeNext)
    where deltaTime   = t - lastTime
          addedTime   = if isAwake && isOnShift then deltaTime else 0
          onShiftNext = case a of
                            StartsShift -> isRecordForGuard (Record t g a) gId
                            otherwise   -> isOnShift
          awakeNext   = if isOnShift then
                            undefined
                        else
                            case a of
                                StartsShift -> 


part2 :: [Record] -> Int
part2 = undefined

-----------------
-- Input Parsing
-----------------

timeParser :: Parser UTCTime
timeParser = do
    year <- int
    char '-'
    month <- int
    char '-'
    day <- int
    char ' '
    hour <- int
    char ':'
    minute <- int
    return $ UTCTime (fromGregorian year month day) (secondsToDiffTime $ hour * 3600 + minute * 60)

startShiftParser :: Parser Action
startShiftParser = string "Guard #" >> (return StartsShift)

fallsAsleepParser :: Parser Action
fallsAsleepParser = string "falls asleep" >> (return FallsAsleep)

wakesUpParser :: Parser Action
wakesUpParser = string "wakes up" >> (return WakesUp)

actionParser :: Parser Action
actionParser = startShiftParser <|> fallsAsleepParser <|> wakesUpParser

recordParser :: Parser Record
recordParser = do
    char '['
    t <- timeParser
    string "] "
    a <- actionParser
    if a == StartsShift then do
        i <- int
        string " begins shift"
        return $ Record t (Just i) a
    else
        return $ Record t Nothing a 

inputParser :: Parser [Record]
inputParser = recordParser `sepBy` (char '\n')