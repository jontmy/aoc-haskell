{-# LANGUAGE TupleSections #-}

module Day6 where

import InputReader(readStrings)
import Data.List.Split(splitOn)

import Data.Map as Map

type OrbitalParent = String
type OrbitalChild = String

type OrbitalPair = (OrbitalParent, OrbitalChild)
type OrbitalMap = Map OrbitalParent [OrbitalChild]         -- maps parents -> child(ren)
type InvertedOrbitalMap = Map OrbitalChild [OrbitalParent] -- maps child -> parent(s)

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
invert :: Ord v => Map k [v] -> Map v [k]
invert m = Map.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, vs) <- Map.toList m, v <- vs]

-- Finds the parent(s) of an orbital body within an orbital map.
parentsOf :: OrbitalChild -> InvertedOrbitalMap -> [OrbitalParent]
parentsOf = findWithDefault []

-- Finds all the ancestors of an orbital body within an orbital map, mapping the degrees of vertical separation between
-- the descendant and the ancestor to the ancestor.
ancestorsOf :: [OrbitalChild] -> Int -> InvertedOrbitalMap -> Map OrbitalParent Int
ancestorsOf children distance invMap = result
  where
    parents = concatMap (`parentsOf` invMap) children
    includeDistance = Prelude.map (, distance)
    result = if Prelude.null children then Map.fromList $ includeDistance parents else Map.union (Map.fromList $ includeDistance parents) (ancestorsOf parents (distance + 1) invMap)
    
-- Finds all the common ancestors of 2 orbital bodies within an orbital map, mapping the minimum number
-- of orbital transfers needed to get from the first orbital body to the other, to the common ancestor
commonAncestorsOf :: OrbitalChild -> OrbitalChild -> InvertedOrbitalMap -> Map OrbitalParent Int
commonAncestorsOf c1 c2 invMap = Map.intersectionWith (+) a1 a2
  where
    a1 = ancestorsOf [c1] 0 invMap
    a2 = ancestorsOf [c2] 0 invMap

-- Finds the minimum number of orbital transfers required to move from the object YOU are orbiting to the object SAN is orbiting.
solvePartTwo :: IO Int
solvePartTwo =
  do
    input <- readStrings "app/input/Day6.txt"
    let invOrbitalMap = invert $ parseOrbits input -- child -> parent
    return $ minimum (commonAncestorsOf "YOU" "SAN" invOrbitalMap)