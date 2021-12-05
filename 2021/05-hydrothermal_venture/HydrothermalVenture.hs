module HydrothermalVenture (parseInput, part1) where

import Data.List (group, sort)
import Data.List.Split (splitOn)

type Point = (Integer, Integer)

interpolate :: Point -> Point -> [Point]
interpolate (x1, y1) (x2, y2)
  | x1 == x2 = zip (repeat x1) (range y1 y2)
  | y1 == y2 = zip (range x1 x2) (repeat y1)
  | otherwise = []
  where
    range a b = [a, a + signum (b - a) .. b]

intersections :: [(Point, Point)] -> [Point]
intersections = map head . filter ((> 1) . length) . group . sort . (uncurry interpolate =<<)

part1 :: [(Point, Point)] -> String
part1 = show . length . intersections

parseInput :: String -> [(Point, Point)]
parseInput = map (tuplify . map (map read . splitOn ",") . splitOn " -> ") . lines
  where
    tuplify [[x1, y1], [x2, y2]] = ((x1, y1), (x2, y2))
