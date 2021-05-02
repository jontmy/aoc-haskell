module Day3 where
  
import InputReader
import Debug.Trace
import Data.List.Split
  
type Coordinates = (Int, Int)
type Line = (Coordinates, Coordinates) -- lines are either horizontal or vertical

x :: Coordinates -> Int
x = fst

y :: Coordinates -> Int
y = snd

isVertical :: Line -> Bool
isVertical line = (x $ fst line) == (x $ snd line)

isHorizontal :: Line -> Bool
isHorizontal line = (y $ fst line) == (y $ snd line)

-- Takes a point and a range, determining whether the point lies on the range.
liesOnRange :: Int -> (Int, Int) -> Bool
liesOnRange p (a, b) = (p >= (min a b)) && (p <= (max a b))

-- Takes 2 ranges, and determines whether they intersect.
intersectsRange :: (Int, Int) -> (Int, Int) -> Bool
intersectsRange (a1, a2) (b1, b2) = (max a1 b1) <= (min a2 b2)

-- Takes 2 lines, and determines whether they intersect.
intersectsLine :: Line -> Line -> Bool
intersectsLine line1 line2 = do
  -- case 1: both lines are vertical
  if isVertical line1 && isVertical line2
    then if x (fst line1) /= x (fst line2)
      then False
      else (y $ fst line1, y $ snd line1) `intersectsRange` (y $ fst line2, y $ snd line2)
  -- case 2: both lines are horizontal
  else if isHorizontal line1 && isHorizontal line2
    then if y (fst line1) /= y (fst line2)
      then False
      else (x $ fst line1, x $ snd line1) `intersectsRange` (x $ fst line2, x $ snd line2)
  -- case 3: line1 is vertical and line2 is horizontal
  else if (isVertical line1) && (isHorizontal line2)
    then (x $ fst line1) `liesOnRange` (x $ fst line2, x $ snd line2) && (y $ fst line2) `liesOnRange` (y $ fst line1, y $ snd line1)
  -- case 4: line1 is horizontal and line2 is vertical
  else (y $ fst line1) `liesOnRange` (y $ fst line2, y $ snd line2) && (x $ fst line2) `liesOnRange` (x $ fst line1, x $ snd line1)

generateLines :: Int -> [String] -> [Line] -> [Line]
generateLines i moves tmpLines = do
  if (i < length moves)
    then generateLines (i + 1) moves (tmpLines ++ [parseMove (snd $ last tmpLines) (moves !! i)])
  else tmpLines

-- Move strings are delimited by commas.
parseMoves :: String -> [Line]
parseMoves str = do
  let moves = splitOn "," str
  let tmpLines = [parseMove (0, 0) (head moves)]
  generateLines 1 moves tmpLines
  
-- Move strings are in the form of <direction><distance>.
parseMove :: Coordinates -> String -> Line
parseMove prev str = do
  let direction = head str
  let distance = read $ tail str
  if direction == 'U'
    then (prev, (x prev, (y prev) + distance))
  else if direction == 'D'
    then (prev, (x prev, (y prev) - distance))
  else if direction == 'L'
    then (prev, ((x prev) - distance, y prev))
  else (prev, ((x prev) + distance, y prev))
  
-- Takes a line and a list of lines, finding the lines in the list that intersect with the first line.
intersections :: Line -> [Line] -> [Line]
intersections path paths = filter (\pathChecking -> path `intersectsLine` pathChecking) paths

-- Takes 2 lines and finds the point at which they intersect.
pointOfIntersection :: Line -> Line -> Coordinates
pointOfIntersection first second = (0, 0)

-- Solves by line intersections rather than a 2D list of points.
solvePartOne :: IO Integer
solvePartOne = do
  wires <- readStrings "app/input/Day3.txt"
  let first = parseMoves $ head wires
  let second = parseMoves $ (wires !! 1)
  let allIntersections = zip first (map (\path -> intersections path second) first)
  mapM_ print allIntersections
  return 0

solvePartTwo :: IO Integer
solvePartTwo = return 0