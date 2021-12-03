module BinaryDiagnostic (parseInput, part1) where

import Control.Applicative (liftA2)
import Data.List (foldl', group, maximumBy, sort, transpose)
import Data.Ord (comparing)

bin2dec :: String -> Integer
bin2dec = foldl' (\acc x -> acc * 2 + read [x]) 0

mostCommonElems :: (Ord a) => [[a]] -> [a]
mostCommonElems = map (head . maximumBy (comparing length) . group . sort) . transpose

bitNot :: String -> String
bitNot = map (\bit -> if bit == '0' then '1' else '0')

part1 :: [String] -> String
part1 = show . liftA2 (*) bin2dec (bin2dec . bitNot) . mostCommonElems

parseInput :: String -> [String]
parseInput = lines
