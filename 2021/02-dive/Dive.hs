module Dive (parseInput, part1) where

import Control.Arrow ((***))
import Data.Char (toUpper)

data Direction = Forward | Up | Down deriving (Read)

dive1 :: (Integer, Integer) -> [(Direction, Integer)] -> (Integer, Integer)
dive1 pos [] = pos
dive1 (x, d) ((Forward, units) : xs) = dive1 (x + units, d) xs
dive1 (x, d) ((Down, units) : xs) = dive1 (x, d + units) xs
dive1 (x, d) ((Up, units) : xs) = dive1 (x, d - units) xs

part1 :: [(Direction, Integer)] -> String
part1 = show . uncurry (*) . dive1 (0, 0)

parseInput :: String -> [(Direction, Integer)]
parseInput = map ((read *** read) . span (' ' /=) . capitalize) . lines
  where
    capitalize (x : xs) = (toUpper x) : xs
