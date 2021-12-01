module AdventOfCode (aocExec, aocTest, aocBench) where

import Control.DeepSeq (NFData)
import Criterion.Main (defaultMainWith, defaultConfig, env, bgroup, bench, nf)
import Criterion.Types (csvFile)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec (Spec, beforeAll, describe, hspec, it, shouldBe)

readTestInput :: String -> IO (FilePath)
readTestInput dir = getCurrentDirectory >>= \cwd -> readFile (cwd </> dir </> "input.txt")

aocExec :: (String -> a) -> (a -> String) -> (a -> String) -> IO ()
aocExec parseInput part1 part2 = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ part1 input
  putStrLn $ part2 input

aocTest :: String -> (String -> a) -> (a -> String, String) -> (a -> String, String) -> IO ()
aocTest dir parseInput (part1, expected1) (part2, expected2) = hspec . beforeAll (readTestInput dir) . describe dir $ do
  it ("returns " ++ expected1 ++ " for part 1") $ \input -> part1 (parseInput input) `shouldBe` expected1
  it ("returns " ++ expected2 ++ " for part 2") $ \input -> part2 (parseInput input) `shouldBe` expected2

aocBench :: (NFData a) => String -> (String -> a) -> (a -> String) -> (a -> String) -> IO ()
aocBench dir parseInput part1 part2 = do
  cwd <- getCurrentDirectory
  defaultMainWith defaultConfig {csvFile = Just (cwd </> dir </> "benchmark.csv")}
    [ env benchEnv $ \ ~(rawInput, input) ->
        bgroup
          dir
          [ bench "parse input" $ nf parseInput rawInput,
            bench "part 1" $ nf part1 input,
            bench "part 2" $ nf part2 input
          ]
    ]
  where
    benchEnv = readTestInput dir >>= \rawInput -> return (rawInput, parseInput rawInput)
