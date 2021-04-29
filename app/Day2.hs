module Day2 where

import InputReader
import Data.List.Split

solvePartOne :: IO Integer
solvePartOne = do
  input <- readStrings "app/input/Day2.txt"
  let opcodes = map strtoi $ splitOn "," $ head input
  return $ head (operate 0 opcodes)

strtoi :: String -> Integer
strtoi = read

operate :: Int -> [Integer] -> [Integer]
operate position opcodes = do
  let opcode = opcodes !! position
  if opcode == 1
    then operate (position + 4) (addition position opcodes)
  else if opcode == 2
    then operate (position + 4) (multiplication position opcodes)
  else opcodes

addition :: Int -> [Integer] -> [Integer]
addition position opcodes = do
  let first = opcodes !! fromIntegral (opcodes !! (position + 1))
  let second = opcodes !! fromIntegral (opcodes !! (position + 2))
  replace (fromIntegral (opcodes !! fromIntegral (position + 3))) (first + second) opcodes

multiplication :: Int -> [Integer] -> [Integer]
multiplication position opcodes = do
  let first = opcodes !! fromIntegral (opcodes !! (position + 1))
  let second = opcodes !! fromIntegral (opcodes !! (position + 2))
  replace (fromIntegral (opcodes !! fromIntegral (position + 3))) (first * second) opcodes

replace :: Int -> Integer -> [Integer] -> [Integer]
replace index replacement list = do
  let (first, second) = splitAt index list
  first ++ [replacement] ++ tail second

solvePartTwo :: IO Integer
solvePartTwo = return 0