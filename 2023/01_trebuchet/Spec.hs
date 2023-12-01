module Main (main) where

import AdventOfCode (aocTest)
import Trebuchet (parseInput, part1, part2)

main :: IO ()
main = aocTest "01_trebuchet" parseInput (part1, "142") (part2, "281")
