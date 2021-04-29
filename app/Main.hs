module Main where

import Day1
import Day2

main :: IO ()
main = do
  putStrLn $ "\n" ++ "Advent of Code 2019"
  dayStr 1 Day1.solvePartOne Day1.solvePartTwo >>= putStrLn
  dayStr 2 Day2.solvePartOne Day2.solvePartTwo >>= putStrLn
    
dayStr :: Integer -> IO Integer -> IO Integer -> IO String
dayStr dayNum partOneResultIO partTwoResultIO = do
  partOneResult <- partOneResultIO
  partTwoResult <- partTwoResultIO
  return $ "\n" ++ "Day " ++ show dayNum ++ ":" ++ "\n" ++ partStr 1 partOneResult ++ "\n" ++ partStr 2 partTwoResult
  
partStr :: Integer -> Integer -> String
partStr partNum partResult = do
  if partResult == 0
    then "part " ++ show partNum ++ " - unsolved"
  else "part " ++ show partNum ++ " - " ++ show partResult