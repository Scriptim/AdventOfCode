module Main (main) where

import AdventOfCode (aocTest)
import TreetopTreeHouse (parseInput, part1, part2)

main :: IO ()
main = aocTest "08_treetop-tree-house" parseInput (part1, "21") (part2, "8")
