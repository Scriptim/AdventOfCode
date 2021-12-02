module Tests where

import AdventOfCode (aocTest)
import Dive

main :: IO ()
main = aocTest "02-dive" parseInput (part1, "150") (part2, "900")
