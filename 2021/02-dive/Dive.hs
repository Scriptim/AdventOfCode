{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Dive (parseInput, part1, part2) where

import Control.Arrow ((***))
import Control.DeepSeq (NFData)
import Data.Char (toUpper)
import GHC.Generics (Generic)

data Direction = Forward | Up | Down deriving (Read, Generic, NFData)

dive1 :: (Integer, Integer) -> [(Direction, Integer)] -> (Integer, Integer)
dive1 pos [] = pos
dive1 (x, d) ((Forward, units) : xs) = dive1 (x + units, d) xs
dive1 (x, d) ((Down, units) : xs) = dive1 (x, d + units) xs
dive1 (x, d) ((Up, units) : xs) = dive1 (x, d - units) xs

dive2 :: (Integer, Integer, Integer) -> [(Direction, Integer)] -> (Integer, Integer, Integer)
dive2 pos [] = pos
dive2 (x, d, a) ((Forward, units) : xs) = dive2 (x + units, d + a * units, a) xs
dive2 (x, d, a) ((Down, units) : xs) = dive2 (x, d, a + units) xs
dive2 (x, d, a) ((Up, units) : xs) = dive2 (x, d, a - units) xs

part1 :: [(Direction, Integer)] -> String
part1 = show . uncurry (*) . dive1 (0, 0)

part2 :: [(Direction, Integer)] -> String
part2 = show . (\(x, d, _) -> x * d) . dive2 (0, 0, 0)

parseInput :: String -> [(Direction, Integer)]
parseInput = map ((read *** read) . span (' ' /=) . capitalize) . lines
  where
    capitalize (x : xs) = (toUpper x) : xs
