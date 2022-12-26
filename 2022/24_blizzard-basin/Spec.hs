module Main (main) where

import AdventOfCode (aocTest)
import BlizzardBasin (parseInput, part1, part2)

main :: IO ()
main = aocTest "24_blizzard-basin" parseInput (part1, "10") (part2, "30")
