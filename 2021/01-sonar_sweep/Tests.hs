module Tests where

import AdventOfCode (aocTest)
import SonarSweep

main :: IO ()
main = aocTest "01-sonar_sweep" parseInput (part1, "7") (part2, "5")
