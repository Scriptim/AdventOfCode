module GiantSquid (parseInput, part1) where

import Control.Applicative (liftA2)
import Data.Either (isRight, lefts)
import Data.List (find, transpose)
import Data.List.Split (splitOn)

type BingoBoard = [[Either Integer Integer]]

drawNum :: Integer -> [BingoBoard] -> [BingoBoard]
drawNum num = map (map (map (\n -> if n == Left num then Right num else n)))

boardWinning :: BingoBoard -> Bool
boardWinning = liftA2 (||) rowWinning (rowWinning . transpose)
  where
    rowWinning = any (all isRight)

playAnyWin :: [Integer] -> [BingoBoard] -> (Integer, BingoBoard)
playAnyWin (n : ns) bingoBoards = case find boardWinning newBoards of
  Just winningBoard -> (n, winningBoard)
  Nothing -> playAnyWin ns newBoards
  where
    newBoards = drawNum n bingoBoards

score :: Integer -> BingoBoard -> Integer
score winningNum = (* winningNum) . sum . lefts . concat

part1 :: ([Integer], [BingoBoard]) -> String
part1 = show . uncurry score . uncurry playAnyWin

parseInput :: String -> ([Integer], [BingoBoard])
parseInput input = (bingoNums, bingoBoards)
  where
    bingoNums = map read $ splitOn "," rawNums
    bingoBoards = map (map (map (Left . read) . words) . lines) rawBoards
    (rawNums:rawBoards) = splitOn "\n\n" input
