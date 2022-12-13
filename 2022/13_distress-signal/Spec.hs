module Main (main) where

import AdventOfCode (aocTest)
import DistressSignal (parseInput, part1, part2)

main :: IO ()
main = aocTest "13_distress-signal" parseInput (part1, "13") (part2, "140")
