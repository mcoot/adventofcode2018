{-# LANGUAGE DuplicateRecordFields #-}

module Solution (Solution(Solution, name, parser, parts, examples, input), test, solve) where

import System.CPUTime
import Control.Exception
import Control.Monad
import Data.List

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

-- Solution to a problem
data Solution a b = Solution {
    -- Name of the problem
    name :: String,
    -- Parser for the problem input
    parser :: Parser a,
    -- List of functions solving each part of the problem
    parts :: [a -> b],
    -- Example inputs with expected results for each part
    examples :: [(String, [b])],
    input :: String
}

time :: t -> IO (t, Double)
time a = do
    start <- getCPUTime
    res <- evaluate a
    end <- getCPUTime
    let ms = (fromIntegral $ end - start) / (10^9)
    return (res, ms)

-- forM_ but with indices
idxForM_ :: (Monad m) => [a] -> ((a, Int) -> m b) -> m ()
idxForM_ xs = forM_ (zipWith (,) xs [1..(length xs)])

applyToFst :: (a -> b) -> (a, c) -> (b, c)
applyToFst f (x, y) = (f x, y)

extractEither :: (Either a b, c) -> Either a (b, c)
extractEither (e, c) = case e of
    Left a -> Left a
    Right b -> Right (b, c)

test :: (Show a, Show b) => Solution a b -> IO ()
test (Solution name parser parts examples _) = do
    putStrLn $ "Testing solution to problem \"" ++ name ++ "\""
    let parsedExamples = sequence $ (extractEither . (applyToFst (parse parser ""))) <$> examples
    case parsedExamples of
        Left err -> putStrLn $ "Failed to parse example: " ++ (show err)
        Right exs -> do
            idxForM_ parts $ \(part, pIdx) -> do
                putStrLn $ "Part " ++ (show pIdx) ++ ":"
                idxForM_ exs $ \(ex, eIdx) -> do
                    putStrLn $ "\tExample " ++ (show eIdx) ++ ":"
                    let (exInput, expectedVals) = ex
                    if pIdx - 1 < 0 || pIdx - 1 >= length expectedVals then
                        putStrLn $ "\t\tExpected: " ++ "[No expected output]"
                    else
                        putStrLn $ "\t\tExpected: " ++ (show $ expectedVals !! (pIdx - 1))
                    (actual, evalTime) <- time $ part exInput
                    putStrLn $ "\t\tActual:   " ++ (show actual) ++ "\t(" ++ (show evalTime) ++ "ms)"
                            
solve :: (Show b) => Solution a b -> IO ()
solve (Solution name parser parts _ input) = do
    putStrLn $ "Solving problem \"" ++ name ++ "\""
    -- Parse input
    case parse parser "" input of
        Left err -> putStrLn $ "Failed to parse input: " ++ (show err)
        Right d -> do
            -- Solve each part and print it out in turn
            -- Done in turn so that we get the first part printed even if the second part takes ages
            idxForM_ parts $ \(part, idx) -> do
                (result, evalTime) <- time $ part d
                putStrLn $ "Part " ++ (show idx) ++ ":"
                putStrLn $ "\tResult: " ++ (show result) ++ "\t(" ++ (show evalTime) ++ "ms)"
            putStrLn $ "[Done]"

    