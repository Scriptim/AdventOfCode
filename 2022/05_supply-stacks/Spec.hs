module Main (main) where

import AdventOfCode (aocTest)
import SupplyStacks (parseInput, part1, part2)

main :: IO ()
main = aocTest "05_supply-stacks" parseInput (part1, "CMZ") (part2, "MCD")
