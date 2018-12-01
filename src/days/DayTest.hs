module DayTest where

-- Test day:
-- Part 1: input x, solution is x^2
-- Part 2: input x, solution is x - 1
-- Real input: 10

import Text.Parsec.Number (int)
import Solution 

solution :: Solution Int Int
solution  = Solution {
    name = "Day Test",
    parser = int,
    parts = [
        \x -> x ^ 2,
        \x -> x - 1
    ],
    examples = [
        ("5", [25, 4]),
        ("7", [49, 6]),
        ("2", [4, 1])
    ],
    inputFile = "./data/days/DayTest.in"
}