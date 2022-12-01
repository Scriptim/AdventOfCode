module Main (main) where

import AdventOfCode (aocTest)
import CalorieCounting (parseInput, part1, part2)

main :: IO ()
main = aocTest "01_calorie-counting" parseInput (part1, "24000") (part2, "45000")
