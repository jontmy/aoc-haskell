module Day6 where

import InputReader(readStrings)
import Data.List.Split(splitOn)
import Data.Map as Map
import Data.Graph as Graph

type OrbitalParent = String
type OrbitalChild = String

type OrbitalPair = (OrbitalParent, OrbitalChild)
type OrbitalMap = Graph

-- Take an orbital string in the form of <parent>)<child> and returns an OrbitalPair tuple.
parseOrbit :: String -> OrbitalPair
parseOrbit str = (parent, child)
  where
    splits = splitOn ")" str
    parent = head splits
    child  = last splits

-- Takes a list of orbital strings and returns a map of each orbital parent to their orbital children.
parseOrbits :: [String] -> Map OrbitalParent [OrbitalChild]
parseOrbits strs = Prelude.foldr addOrbit Map.empty orbits
  where
    orbits = Prelude.map parseOrbit strs
    addOrbit (parent, child) = insertWith (++) parent [child]

-- Takes the tuples from parseOrbits and constructs an orbital map represented by a directed acyclic graph.
-- In the graph, parents and children are represented by vertices, while the directed edges are in the form parent -> children
buildOrbitalMap :: Map OrbitalParent [OrbitalChild] -> (OrbitalMap, Vertex -> (OrbitalParent, OrbitalChild, [OrbitalChild]), OrbitalChild -> Maybe Vertex)
buildOrbitalMap orbits = graphFromEdges $ format orbits
  where
    format = Prelude.map (\(parent, child) -> (parent, parent, child)) . toList

-- Calculates the number of direct and indirect orbital children by recursion, starting from the orbital parent given.
calculateOrbits :: OrbitalParent -> Map OrbitalParent [OrbitalChild] -> Int
calculateOrbits parent orbitalMap = if member parent orbitalMap then directOrbits + indirectOrbits else 0
  where
    children = orbitalMap ! parent
    directOrbits = length children
    indirectOrbits = sum $ Prelude.map (`calculateOrbits` orbitalMap) children

-- Calculates the total number of direct and indirect orbits from each orbital body in the orbital map.
solvePartOne :: IO Int
solvePartOne =
  do
    input <- readStrings "app/input/Day6.txt"
    let orbitalMap = parseOrbits input
    let orbitalBodies = keys orbitalMap
    return (sum $ Prelude.map (`calculateOrbits` orbitalMap) orbitalBodies)

solvePartTwo :: IO Int
solvePartTwo = return 0
