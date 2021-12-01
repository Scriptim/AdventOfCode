module SonarSweep (parseInput, part1) where

numIncreases :: Ord a => [a] -> Int
numIncreases = length . filter (LT ==) . ap (zipWith compare) tail

part1 :: [Integer] -> String
part1 = show . numIncreases

parseInput :: String -> [Integer]
parseInput = map read . lines
