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
solvePartTwo = result 0

result :: Integer -> IO Integer
result nounverb = do
  tmp <- calculate nounverb
  if tmp == 19690720
    then return nounverb
  else result (nounverb + 1)
  
calculate :: Integer -> IO Integer
calculate nounverb = do
  opcodes <- reset nounverb
  let resultOpcodes = operate 0 opcodes
  return $ head resultOpcodes
  
reset :: Integer -> IO [Integer]
reset nounverb = do
  input <- readStrings "app/input/Day2.txt"
  let opcodes = map strtoi $ splitOn "," $ head input
  let (noun, verb) = decompose nounverb
  return $ replace 2 verb $ replace 1 noun opcodes
  
decompose :: Integer -> (Integer, Integer)
decompose n = (n `div` 100, n `mod` 100)
  