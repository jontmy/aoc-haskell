module Day6 where

import InputReader(readStrings)
import Data.List.Split(splitOn)

import Data.Map as Map
import Data.Set as Set

type OrbitalParent = String
type OrbitalChild = String

type OrbitalPair = (OrbitalParent, OrbitalChild)
type OrbitalMap = Map OrbitalParent [OrbitalChild]
type InvertedOrbitalMap = Map OrbitalChild [OrbitalParent]

-- Take an orbital string in the form of <parent>)<child> and returns an OrbitalPair tuple.
parseOrbit :: String -> OrbitalPair
parseOrbit str = (parent, child)
  where
    splits = splitOn ")" str
    parent = head splits
    child  = last splits

-- Takes a list of orbital strings and returns a map of each orbital parent to their orbital children.
parseOrbits :: [String] -> OrbitalMap
parseOrbits strs = Prelude.foldr addOrbit Map.empty orbits
  where
    orbits = Prelude.map parseOrbit strs
    addOrbit (parent, child) = insertWith (++) parent [child]
    
-- Calculates the number of direct and indirect orbital children by recursion, starting from the orbital parent given.
calculateOrbits :: OrbitalParent -> Map OrbitalParent [OrbitalChild] -> Int
calculateOrbits parent orbitalMap = if Map.member parent orbitalMap then directOrbits + indirectOrbits else 0
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
    
-- Inverts a Data.Map, handling the case where a value appears more than once in the "right hand side" assignments.
-- https://stackoverflow.com/questions/21538903/how-can-i-elegantly-invert-a-maps-keys-and-values#21541158
invert :: (Ord k, Ord v) => Map k [v] -> Map v [k]
invert m = Map.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, vs) <- Map.toList m, v <- vs]

-- Finds the parent(s) of an orbital body within an orbital map.
parentsOf :: OrbitalChild -> InvertedOrbitalMap -> [OrbitalParent]
parentsOf = findWithDefault []

-- Finds all the ancestors of an orbital body within an orbital map.
ancestorsOf :: [OrbitalChild] -> InvertedOrbitalMap -> Set OrbitalParent
ancestorsOf children invMap = Set.union (Set.fromList parents) (ancestorsOf parents invMap)
  where
    parents = concatMap (`parentsOf` invMap) children

-- Finds all the common ancestors of 2 orbital bodies within an orbital map.
commonAncestorsOf :: OrbitalChild -> OrbitalChild -> InvertedOrbitalMap -> Set OrbitalParent
commonAncestorsOf c1 c2 invMap = Set.intersection a1 a2
  where
    a1 = ancestorsOf [c1] invMap
    a2 = ancestorsOf [c2] invMap

-- Finds the minimum number of orbital transfers needed to get from one orbital body to another.
-- Assumes that the first orbital body orbits the second orbital body, directly or indirectly.
orbitalTransfers :: OrbitalChild -> OrbitalParent -> InvertedOrbitalMap -> Int
orbitalTransfers child parent invMap = 0

-- Finds the shortest path of orbital transfers between 2 orbital bodies by breadth-first search.
-- Throws an error if there is no path of orbital transfers between the 2 bodies.



solvePartTwo :: IO Int
solvePartTwo =
  do
    input <- readStrings "app/input/Day6.txt"
    let invOrbitalMap = invert $ parseOrbits input -- child -> parent
--    mapM_ print $ toList invOrbitalMap
    return 0