module Benchmarks where

import AdventOfCode (aocBench)
import HydrothermalVenture

main :: IO ()
main = aocBench "05-hydrothermal_venture" parseInput part1 part2
