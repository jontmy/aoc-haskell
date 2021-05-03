module Day3 where
  
import InputReader
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
    then False
  -- case 2: both lines are horizontal
  else if isHorizontal line1 && isHorizontal line2
    then False
  -- case 3: line1 is vertical and line2 is horizontal
  else if isVertical line1 && isHorizontal line2
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
  else if direction == 'R'
    then (prev, ((x prev) + distance, y prev))
  else error "Invalid direction in move string."

-- Takes a line and a list of lines, finding the lines in the list that intersect with the first line.
intersections :: Line -> [Line] -> [Line]
intersections path paths = filter (\pathChecking -> path `intersectsLine` pathChecking) paths

-- Takes 2 lines and finds the point at which they intersect.
pointOfIntersection :: Line -> Line -> Coordinates
pointOfIntersection line1 line2 = do
  if isVertical line1 && isHorizontal line2
    then (x $ fst line1, y $ fst line2)
  else if isHorizontal line1 && isVertical line2
    then (x $ fst line2, y $ fst line1)
  else error "Assumed that parallel lines do not have a point of intersection."

-- Takes a tuple of a line and a list of lines, finding all the points at which the line intersects with lines in the list.
pointsOfIntersection :: (Line, [Line]) -> [Coordinates]
pointsOfIntersection (line1, line2s) = map (\line2 -> pointOfIntersection line1 line2) line2s

-- Takes a point and calculates its Manhattan distance from the origin (0, 0).
manhattanDistance :: Coordinates -> Int
manhattanDistance p = (abs $ x p) + (abs $ y p)

-- Solves by line intersections rather than a 2D list of points.
solvePartOne :: IO Integer
solvePartOne = do
  wires <- readStrings "app/input/Day3.txt"
  let first = parseMoves $ head wires
  let second = parseMoves $ (wires !! 1)
  let lineIntersections = zip first (map (\path -> intersections path second) first)
  let pointIntersections = concat $ filter (\lst -> length lst /= 0) $ map pointsOfIntersection lineIntersections
  return $ fromIntegral $ foldr1 min $ map (\p -> manhattanDistance p) pointIntersections

-- Takes an integer and multiplies it by itself.
square :: Int -> Int
square n = n * n

-- Takes 2 points and finds the length of the line segment between them using the Pythagorean theorem.
euclideanDistance:: Coordinates -> Coordinates -> Int
euclideanDistance (x1, y1) (x2, y2) = (round . sqrt . fromIntegral) (dx + dy)
  where
    dx = square $ x2 - x1
    dy = square $ y2 - y1
    
-- Takes a point P and a line AB, determining whether the point lies on the line by checking whether AB = AP + PB.
liesOnLine :: Coordinates -> Line -> Bool
liesOnLine p (a, b) = ab == ap + pb
  where
    ab = euclideanDistance a b
    ap = euclideanDistance a p
    pb = euclideanDistance p b

-- Takes a point and a list of lines representing a wire, finding the shortest distance from the origin to the point via the wire.
travelDistance :: Coordinates -> [Line] -> Int
travelDistance point wire = wireDistance + remainderDistance
  where
    path = takeWhile (\line -> not $ point `liesOnLine` line) wire -- the lines traversed from the origin to the line with the intersection point
    wireDistance = sum (map (\(p1, p2) -> euclideanDistance p1 p2) path) -- the total length of the lines in the path
    remainderDistance = euclideanDistance (snd $ last path) point -- the distance from the end of the path to the intersection point

-- Take a point and 2 lists of wires representing 2 wires, finding the total distance from the origin to the point via both wires.
totalTravelDistance :: Coordinates -> [Line] -> [Line] -> Int
totalTravelDistance point wire1 wire2 = dist1 + dist2
  where
    dist1 = travelDistance point wire1
    dist2 = travelDistance point wire2

minimumTravelDistance :: [Coordinates] -> [Line] -> [Line] -> Int
minimumTravelDistance points wire1 wire2 = foldr1 min $ distances
  where
    distances = map (\point -> totalTravelDistance point wire1 wire2) points

solvePartTwo :: IO Integer
solvePartTwo = do
  wires <- readStrings "app/input/Day3.txt"
  let 
    result = minimumTravelDistance points wire1 wire2
      where
        wire1 = parseMoves $ head wires
        wire2 = parseMoves $ (wires !! 1)
        lineIntersections = zip wire1 (map (\path -> intersections path wire2) wire1)
        points = concat $ filter (\lst -> length lst /= 0) $ map pointsOfIntersection lineIntersections
  return $ fromIntegral result