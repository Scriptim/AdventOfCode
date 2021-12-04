module Tests where

import AdventOfCode (aocTest)
import GiantSquid

main :: IO ()
main = aocTest "04-giant_squid" parseInput (part1, "4512") (part2, "1924")
