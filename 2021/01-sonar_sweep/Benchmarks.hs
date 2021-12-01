module Benchmarks where

import AdventOfCode (aocBench)
import SonarSweep

main :: IO ()
main = aocBench "01-sonar_sweep" parseInput part1 part2
