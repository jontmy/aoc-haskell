module Day5 where

import Data.List.Split
import InputReader

import Data.Vector hiding ((++))

parseInput :: String -> Vector Int
parseInput str = fromList lst
  where lst = Prelude.map read (splitOn "," str)
  
parseOpcode :: Int -> Vector Int -> Vector Int
parseOpcode pos vec =
  case (vec ! pos) of
    1  -> parseOpcode (pos + 4) $ parseOpAddition pos vec
    2  -> parseOpcode (pos + 4) $ parseOpMultiplication pos vec
    99 -> vec
    _  -> error $ "parseOpcode :: Invalid opcode " ++ show (vec ! pos)
  
parseOpAddition :: Int -> Vector Int -> Vector Int
parseOpAddition pos vec = vec // [(vec ! (pos + 3), result)]
  where
    first  = vec ! (vec ! (pos + 1))
    second = vec ! (vec ! (pos + 2))
    result = first + second
    
parseOpMultiplication :: Int -> Vector Int -> Vector Int
parseOpMultiplication pos vec = vec // [(vec ! (pos + 3), result)]
 where
   first  = vec ! (vec ! (pos + 1))
   second = vec ! (vec ! (pos + 2))
   result = first * second

solvePartOne :: IO Integer
solvePartOne =
  do
    input <- readStrings "app/input/Day2.txt"
    let result = parseOpcode 0 $ parseInput (input !! 0)
    return (fromIntegral (result ! 0))

solvePartTwo :: IO Integer
solvePartTwo = return 0