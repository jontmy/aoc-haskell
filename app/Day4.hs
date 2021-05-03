module Day4 where
 
import Data.Char(digitToInt)

twoSameAdjacentDigits :: Int -> Bool
twoSameAdjacentDigits n = a || b || c || d || e
  where
    str = show n
    a = str !! 0 == str !! 1
    b = str !! 1 == str !! 2
    c = str !! 2 == str !! 3
    d = str !! 3 == str !! 4
    e = str !! 4 == str !! 5
    
noDecreasingDigits :: Int -> Bool
noDecreasingDigits n = a && b && c && d && e
  where
    str = show n
    a = digitToInt (str !! 0) <= digitToInt (str !! 1)
    b = digitToInt (str !! 1) <= digitToInt (str !! 2)
    c = digitToInt (str !! 2) <= digitToInt (str !! 3)
    d = digitToInt (str !! 3) <= digitToInt (str !! 4)
    e = digitToInt (str !! 4) <= digitToInt (str !! 5)
    
meetsBothCriteria :: Int -> Bool
meetsBothCriteria n = twoSameAdjacentDigits n && noDecreasingDigits n

solvePartOne :: IO Integer
solvePartOne = return (fromIntegral (length result))
  where
    result = filter meetsBothCriteria [123257..647015]
    
solvePartTwo :: IO Integer
solvePartTwo = return 0