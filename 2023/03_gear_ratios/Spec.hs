module Main (main) where

import AdventOfCode (aocTest)
import GearRatios (parseInput, part1, part2)

main :: IO ()
main = aocTest "03_gear_ratios" parseInput (part1, "4361") (part2, "467835")
