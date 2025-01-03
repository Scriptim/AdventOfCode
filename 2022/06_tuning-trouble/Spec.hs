module Main (main) where

import AdventOfCode (aocTest)
import TuningTrouble (parseInput, part1, part2)

main :: IO ()
main = aocTest "06_tuning-trouble" parseInput (part1, "7") (part2, "19")
