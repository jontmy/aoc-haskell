module Main where

import Day1
import Day2
import Day3
import Day4
import Day5

import Data.Time
import Data.Fixed

type Day = Int
type Part = Int

main :: IO ()
main = do
  putStrLn $ "\n" ++ "Advent of Code 2019"
  benchmarkDay 1 Day1.solvePartOne Day1.solvePartTwo
  benchmarkDay 2 Day2.solvePartOne Day2.solvePartTwo
  benchmarkDay 3 Day3.solvePartOne Day3.solvePartTwo
  benchmarkDay 4 Day4.solvePartOne Day4.solvePartTwo
  benchmarkDay 5 Day5.solvePartOne Day5.solvePartTwo

benchmarkDay :: Show a => Main.Day -> IO a -> IO a -> IO ()
benchmarkDay day solver1 solver2 =
  do
    putStrLn ""
    putStrLn $ "Day " ++ show day ++ ":"
    benchmarkPart 1 solver1
    benchmarkPart 2 solver2

benchmarkPart :: Show a => Part -> IO a -> IO ()
benchmarkPart part solver =
  do
    putStr $ "part " ++ show part ++ " - "
    tic <- getCurrentTime
    res <- solver
    toc <- getCurrentTime
    putStr $ show res
    putStrLn $ " (" ++ showFixed True (nominalDiffTimeToSeconds (diffUTCTime toc tic) * 1000) ++ " ms)"