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
isPositional pos ins = result
  where
    modes = Prelude.drop 2 $ Prelude.reverse (show ins)
    result = if pos > Prelude.length modes then True else (modes !! (pos - 1)) == '0'

-- In immediate mode, reads the value of the parameter.
-- In position mode, reads the value from the pointer provided by the parameter.
getValue :: Position -> Position -> Vector Int -> Int
getValue insPos paramPos vec = value
  where
    ins = vec ! insPos
    param = vec ! (insPos + paramPos)
    value = if isPositional paramPos ins then vec ! param else param

-- Parameters that an instruction writes to will never be in immediate mode.
-- In position mode, overwrites the value stored at the index position with the value given.
setValue :: Position -> Vector Int -> Int -> Vector Int
setValue pos vec val = vec // [(vec ! pos, val)]

operate :: Position -> Vector Int -> Int -> Vector Int
operate pos vec input =
  case (getOpcode pos vec) of
    1  -> operate (pos + 4) (oppAdd pos vec) input
    2  -> operate (pos + 4) (opMultiply pos vec) input
    3  -> operate (pos + 2) (opStore pos vec input) input
    4  -> operate (pos + 2) (opRetrieve pos vec) input
    5  -> operate (opJumpIfTrue pos vec) vec input
    6  -> operate (opJumpIfFalse pos vec) vec input
    7  -> operate (pos + 4) (opLessThan pos vec) input
    8  -> operate (pos + 4) (opEquals pos vec) input
    99 -> vec
    _  -> error $ "parseOpcode :: Invalid opcode " ++ (show $ getOpcode pos vec) ++ " at position " ++ show pos

opJumpIfTrue :: Position -> Vector Int -> Position
opJumpIfTrue pos vec = ptr
  where
    first = getValue pos 1 vec
    second = getValue pos 2 vec
    ptr = if first /= 0 then second else pos + 3

opJumpIfFalse :: Position -> Vector Int -> Position
opJumpIfFalse pos vec = ptr
  where
    first = getValue pos 1 vec
    second = getValue pos 2 vec
    ptr = if first == 0 then second else pos + 3

opLessThan :: Position -> Vector Int -> Vector Int
opLessThan pos vec = setValue (pos + 3) vec intBool
  where
    first = getValue pos 1 vec
    second = getValue pos 2 vec
    intBool = if first < second then 1 else 0

opEquals :: Position -> Vector Int -> Vector Int
opEquals pos vec = setValue (pos + 3) vec intBool
 where
   first = getValue pos 1 vec
   second = getValue pos 2 vec
   intBool = if first == second then 1 else 0

opStore :: Position -> Vector Int -> Int -> Vector Int
opStore pos vec input = vec // [(vec ! (pos + 1), input)]

opRetrieve :: Position -> Vector Int -> Vector Int
opRetrieve pos vec = output
  where
    output = if isPositional 1 (vec ! pos)
      then snoc vec (vec ! (vec ! (pos + 1)))
      else snoc vec (vec ! (pos + 1))

oppAdd :: Position -> Vector Int -> Vector Int
oppAdd pos vec = vec // [(vec ! (pos + 3), result)]
  where
    first = if isPositional 1 (vec ! pos)
      then vec ! (vec ! (pos + 1))
      else vec ! (pos + 1)
    second = if isPositional 2 (vec ! pos)
     then vec ! (vec ! (pos + 2))
     else vec ! (pos + 2)
    result = first + second

opMultiply :: Position -> Vector Int -> Vector Int
opMultiply pos vec = vec // [(vec ! (pos + 3), result)]
 where
   first = if isPositional 1 (vec ! pos)
     then vec ! (vec ! (pos + 1))
     else vec ! (pos + 1)
   second = if isPositional 2 (vec ! pos)
    then vec ! (vec ! (pos + 2))
    else vec ! (pos + 2)
   result = first * second

solvePartOne :: IO Integer
solvePartOne =
  do
    input <- readStrings "app/input/Day5.txt"
    let result = operate 0 (parseInput (input !! 0)) 1
    return (fromIntegral (Data.Vector.last result))

solvePartTwo :: IO Integer
solvePartTwo =
  do
    input <- readStrings "app/input/Day5.txt"
    let result = operate 0 (parseInput (input !! 0)) 5
    return (fromIntegral (Data.Vector.last result))