module SupplyStacks (parseInput, part1, part2) where

import AdventOfCode (Parser)
import Data.Char (isSpace)
import Data.List (foldl', transpose)
import Data.Text (pack)
import Text.Megaparsec (anySingle, endBy, sepBy, skipManyTill, (<|>))
import Text.Megaparsec.Char (char, letterChar, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Crate = Char

type Stack = [Crate]

data Move = Move Int Int Int

parseInput :: Parser ([Stack], [Move])
parseInput = (,) <$> stacks <* skipManyTill anySingle newline <* newline <*> move `endBy` newline
  where
    stacks = map (dropWhile isSpace) . transpose <$> crate `sepBy` char ' ' `endBy` newline
    crate = (char '[' *> letterChar <* char ']') <|> (' ' <$ string (pack "   "))
    move = Move <$> (string (pack "move ") *> decimal) <*> (pred <$> (string (pack " from ") *> decimal)) <*> (pred <$> (string (pack " to ") *> decimal))

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f n xs = take n xs ++ f (xs !! n) : drop (n + 1) xs

performMove :: [Stack] -> Move -> [Stack]
performMove stacks (Move n from to) = adjust (drop n) from $ adjust (crates ++) to stacks
  where
    crates = reverse . take n $ stacks !! from

part1 :: ([Stack], [Move]) -> String
part1 = map head . uncurry (foldl' performMove)

part2 :: ([Stack], [Move]) -> String
part2 = undefined
