module Main (main) where

import AdventOfCode (aocTest)
import RucksackReorganization (parseInput, part1, part2)

main :: IO ()
main = aocTest "03_rucksack-reorganization" parseInput (part1, "157") (part2, "70")
