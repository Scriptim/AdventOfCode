module SonarSweep (parseInput, part1, part2) where

import Control.Monad (ap)
import Data.List (tails)

numIncreases :: Ord a => [a] -> Int
numIncreases = length . filter (LT ==) . ap (zipWith compare) tail

windows :: (Num a) => [a] -> [a]
windows = map (sum . take 3) . tails

part1 :: [Integer] -> String
part1 = show . numIncreases

part2 :: [Integer] -> String
part2 = part1 . windows

parseInput :: String -> [Integer]
parseInput = map read . lines
