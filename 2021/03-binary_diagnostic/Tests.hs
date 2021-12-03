module Tests where

import AdventOfCode (aocTest)
import BinaryDiagnostic

main :: IO ()
main = aocTest "03-binary_diagnostic" parseInput (part1, "198") (part2, "230")
