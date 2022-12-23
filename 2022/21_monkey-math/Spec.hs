module Main (main) where

import AdventOfCode (aocTest)
import MonkeyMath (parseInput, part1, part2)

main :: IO ()
main = aocTest "21_monkey-math" parseInput (part1, "152") (part2, "301")
