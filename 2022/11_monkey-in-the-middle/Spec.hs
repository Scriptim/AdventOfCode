module Main (main) where

import AdventOfCode (aocTest)
import MonkeyInTheMiddle (parseInput, part1, part2)

main :: IO ()
main = aocTest "11_monkey-in-the-middle" parseInput (part1, "10605") (part2, "2713310158")
