module Benchmarks where

import AdventOfCode (aocBench)
import BinaryDiagnostic

main :: IO ()
main = aocBench "03-binary_diagnostic" parseInput part1 part2
