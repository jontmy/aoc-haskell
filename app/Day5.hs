module Day5 where

import Data.List.Split
import InputReader

import Data.Vector hiding ((++))

type Position = Int
type Opcode = Int

parseInput :: String -> Vector Int
parseInput str = fromList lst
  where lst = Prelude.map read (splitOn "," str)

getOpcode :: Position -> Vector Int -> Opcode
getOpcode pos vec = opcode
  where
    ins = Prelude.reverse (show $ (vec ! pos))
    opcode = read $ Prelude.reverse (Prelude.take 2 ins)
    
isPositional :: Position -> Int -> Bool
isPositional pos ins
  | pos >= (Prelude.length $ show ins) - 2 = True
  | otherwise = (Prelude.reverse $ show ins) !! (pos + 2) == '0'

operate :: Int -> Vector Int -> Vector Int
operate pos vec =
  case (getOpcode pos vec) of
    1  -> operate (pos + 4) $ oppAdd pos vec
    2  -> operate (pos + 4) $ opMultiply pos vec
    3  -> operate (pos + 2) $ opStore pos 1 vec
    4  -> operate (pos + 2) $ opRetrieve pos vec
    99 -> vec
    _  -> error $ "parseOpcode :: Invalid opcode " ++ (show $ getOpcode pos vec) ++ " at position " ++ show pos

opStore :: Position -> Int -> Vector Int -> Vector Int
opStore pos input vec = vec // [(vec ! (pos + 1), input)]

opRetrieve :: Position -> Vector Int -> Vector Int
opRetrieve pos vec = output
  where 
    output = if isPositional 0 (vec ! pos)
      then snoc vec (vec ! (vec ! (pos + 1)))
      else snoc vec (vec ! (pos + 1))

oppAdd :: Position -> Vector Int -> Vector Int
oppAdd pos vec = vec // [(vec ! (pos + 3), result)]
  where
    first = if isPositional 0 (vec ! pos)
      then vec ! (vec ! (pos + 1))
      else vec ! (pos + 1)
    second = if isPositional 1 (vec ! pos)
     then vec ! (vec ! (pos + 2))
     else vec ! (pos + 2)
    result = first + second

opMultiply :: Position -> Vector Int -> Vector Int
opMultiply pos vec = vec // [(vec ! (pos + 3), result)]
 where
   first = if isPositional 0 (vec ! pos)
     then vec ! (vec ! (pos + 1))
     else vec ! (pos + 1)
   second = if isPositional 1 (vec ! pos)
    then vec ! (vec ! (pos + 2))
    else vec ! (pos + 2)
   result = first * second

solvePartOne :: IO Integer
solvePartOne =
  do
    input <- readStrings "app/input/Day5.txt"
    let result = operate 0 $ parseInput (input !! 0)
    return (fromIntegral (Data.Vector.last result))

solvePartTwo :: IO Integer
solvePartTwo = return 0