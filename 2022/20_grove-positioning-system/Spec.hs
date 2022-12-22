module Main (main) where

import AdventOfCode (aocTest)
import GrovePositioningSystem (parseInput, part1, part2)

main :: IO ()
main = aocTest "20_grove-positioning-system" parseInput (part1, "3") (part2, "1623178306")
