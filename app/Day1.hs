module Day1 where
  
import InputReader

solvePartOne :: IO Integer
solvePartOne = do
  masses <- readIntegers "app/Day1.txt"
  return . sum $ map calculateFuelRequired masses
   
calculateFuelRequired :: Integer -> Integer
calculateFuelRequired mass = div mass 3 - 2

solvePartTwo :: IO Integer
solvePartTwo = do
  masses <- readIntegers "app/Day1.txt"
  return . sum $ map calculateTotalFuelRequired masses

calculateTotalFuelRequired :: Integer -> Integer
calculateTotalFuelRequired mass
  | calculateFuelRequired mass < 0 = 0
  | otherwise = calculateFuelRequired mass + calculateTotalFuelRequired (calculateFuelRequired mass)