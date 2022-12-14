module Main (main) where

import AdventOfCode (aocTest)
import RegolithReservoir (parseInput, part1, part2)

main :: IO ()
main = aocTest "14_regolith-reservoir" parseInput (part1, "24") (part2, "93")
