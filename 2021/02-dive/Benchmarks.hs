module Benchmarks where

import AdventOfCode (aocBench)
import Dive

main :: IO ()
main = aocBench "02-dive" parseInput part1 part2
