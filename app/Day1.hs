module Day1 where
  
import InputReader

solvePartOne :: IO Integer
solvePartOne = do
  masses <- readIntegers "app/Day1.txt"
  return . sum $ map calculateFuelRequired masses
   
calculateFuelRequired :: Integer -> Integer
calculateFuelRequired mass = div mass 3 - 2

solvePartTwo :: IO Integer
solvePartTwo = return 0