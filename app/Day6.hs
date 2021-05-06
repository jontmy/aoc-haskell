module Day6 where
  
import InputReader(readStrings)
import Data.List.Split(splitOn)
import Data.Map as Map
import Data.Tree as Tree
import Data.Graph as Graph

type OrbitalPair = (OrbitalParent, OrbitalChild)
type OrbitalParent = String
type OrbitalChild = String

parseOrbit :: String -> OrbitalPair
parseOrbit str = (parent, child)
  where
    splits = splitOn ")" str
    parent = head splits
    child  = last splits
    
parseOrbits :: [String] -> Map OrbitalParent [OrbitalChild]
parseOrbits strs = Prelude.foldr addOrbit Map.empty orbits
  where orbits = Prelude.map parseOrbit strs

addOrbit :: OrbitalPair -> Map OrbitalParent [OrbitalChild] -> Map OrbitalParent [OrbitalChild]
addOrbit (parent, child) = insertWith (++) parent [child]

solvePartOne :: IO Int
solvePartOne =
  do
    input <- readStrings "app/input/Day5.txt"
    let orbit = parseOrbits input
    return 0

solvePartTwo :: IO Int
solvePartTwo = return 0
