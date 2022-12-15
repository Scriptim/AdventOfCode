module Main (main) where

import AdventOfCode (aocTest)
import BeaconExclusionZone (parseInput, part1, part2)

main :: IO ()
main = aocTest "15_beacon-exclusion-zone" parseInput (part1, "0") (part2, "24")
