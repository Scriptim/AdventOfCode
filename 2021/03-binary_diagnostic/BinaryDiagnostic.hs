module BinaryDiagnostic (parseInput, part1, part2) where

import Control.Applicative (liftA2)
import Data.List (foldl', group, maximumBy, sort, transpose)
import Data.Ord (comparing)

bin2dec :: String -> Integer
bin2dec = foldl' (\acc x -> acc * 2 + read [x]) 0

mostCommonElems :: (Ord a) => [[a]] -> [a]
mostCommonElems = map (head . maximumBy (comparing length) . group . sort) . transpose

bitNot :: String -> String
bitNot = map (\bit -> if bit == '0' then '1' else '0')

iterateMatch :: Eq a => ([[a]] -> [a]) -> [[a]] -> [a]
iterateMatch criterion = head . snd . until ((== 1) . length . snd) iterator . (,) 0
  where
    iterator (n, xs) = (n + 1, filter ((criterion xs !! n ==) . (!! n)) xs)

part1 :: [String] -> String
part1 = show . liftA2 (*) bin2dec (bin2dec . bitNot) . mostCommonElems

part2 :: [String] -> String
part2 xs = show $ bin2dec (iterateMatch mostCommonElems xs) * bin2dec (iterateMatch (bitNot . mostCommonElems) xs)

parseInput :: String -> [String]
parseInput = lines
