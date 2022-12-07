module Main (main) where

import AdventOfCode (aocTest)
import NoSpaceLeftOnDevice (parseInput, part1, part2)

main :: IO ()
main = aocTest "07_no-space-left-on-device" parseInput (part1, "95437") (part2, "24933642")
