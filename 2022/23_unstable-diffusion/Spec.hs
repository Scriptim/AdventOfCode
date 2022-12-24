module Main (main) where

import AdventOfCode (aocTest)
import UnstableDiffusion (parseInput, part1, part2)

main :: IO ()
main = aocTest "23_unstable-diffusion" parseInput (part1, "110") (part2, "20")
