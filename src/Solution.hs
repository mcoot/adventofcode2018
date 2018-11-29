{-# LANGUAGE DuplicateRecordFields #-}

module Solution where

import System.CPUTime
import Control.Exception
import Control.Monad

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

-- Solution to a problem
data Solution a b = Solution {
    -- Name of the problem
    name :: String,
    -- Parser for the problem input
    parser :: Parser a,
    -- List of functions solving each part of the problem
    parts :: [a -> b]
}

type Result b = [(b, Int)]

time :: t -> IO (t, Double)
time a = do
    start <- getCPUTime
    res <- evaluate a
    end <- getCPUTime
    let ms = (fromIntegral $ end - start) / (10^9)
    return (res, ms)

showSolution :: (Show b) => Solution a b -> String -> IO ()
showSolution (Solution name parser parts) input = do
    putStrLn $ "Problem: " ++ name
    -- Parse input
    case parse parser "" input of
        Left err -> putStrLn $ "Failed to parse input: " ++ (show err)
        Right d -> do
            -- Solve each part and print it out in turn
            -- Done in turn so that we get the first part printed even if the second part takes ages
            forM_ (zipWith (,) parts [1..(length parts)]) $ \(part, idx) -> do
                (result, evalTime) <- time $ part d
                putStrLn $ "Part " ++ (show idx) ++ ": " ++ (show result) ++ " (" ++ (show evalTime) ++ "ms)"
            putStrLn $ "[Done]"

    