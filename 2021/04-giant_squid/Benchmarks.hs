module Benchmarks where

import AdventOfCode (aocBench)
import GiantSquid

main :: IO ()
main = aocBench "04-giant_squid" parseInput part1 part2
