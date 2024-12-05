module Common 
    ( Vertex
    , Edge
    , Graph
    , generateEdges
    , verifyVertexCover
    , genSubsets
    , createGraph
    , chunksOf
    , genSubsetsParallel
    , genSubsetsMemoized
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Parallel (par, pseq)

-- Type Aliases
type Vertex = String
type Edge = (Vertex, Vertex)
type Graph = Map Vertex [Vertex]

-- Generate all unique edges from the graph
generateEdges :: Graph -> [Edge]
generateEdges graph = 
    [(v1, v2) | (v1, neighbors) <- Map.toList graph,
                v2 <- neighbors,
                v1 < v2]

-- Verify if a set of vertices forms a valid vertex cover
verifyVertexCover :: [Vertex] -> [Edge] -> Bool
verifyVertexCover cover edges =
    all (\(v1, v2) -> v1 `elem` cover || v2 `elem` cover) edges

-- Generate all subsets of a given size
genSubsets :: [a] -> Int -> [[a]]
genSubsets _ 0 = [[]]
genSubsets [] _ = []
genSubsets (x:xs) k = 
    let withX = map (x:) (genSubsets xs (k-1))
        withoutX = genSubsets xs k
    in withX ++ withoutX

-- Memoized version of genSubsets with caching, using lists instead of sets
genSubsetsCache :: (Eq a, Ord a) => [a] -> Int -> Map.Map ([a], Int) [[a]] -> ([[a]], Map.Map ([a], Int) [[a]])
genSubsetsCache _ 0 cache = ([[]], cache)  -- Base case: Only the empty list for size 0
genSubsetsCache [] n cache = ([], cache)  -- No subsets if the list is empty and n > 0
genSubsetsCache list n cache
  | Map.member (list, n) cache = (Map.findWithDefault [] (list, n) cache, cache)  -- Return cached results if available
  | otherwise =
      let (x:xs) = list
          -- Generate subsets with and without x
          (withX, cache1) = genSubsetsCache xs (n - 1) cache  -- subsets including x
          (withoutX, cache2) = genSubsetsCache xs n cache1  -- subsets excluding x
          result = (map (x:) withX ++ withoutX)
          updatedCache = Map.insert (list, n) result cache2  -- Store the result in the cache
      	  -- Clean the cache
      	  cleanedCache = cleanCache (n - 2) updatedCache
      in (result, cleanedCache)

-- Function to remove entries with size <= targetSize from the cache
cleanCache :: Int -> Map.Map ([a], Int) [[a]] -> Map.Map ([a], Int) [[a]]
cleanCache targetSize cache = Map.filterWithKey (\(_, size) _ -> size > targetSize) cache

-- Wrapper function to initialize the cache and get the result
genSubsetsMemoized :: (Eq a, Ord a) => [a] -> Int -> [[a]]
genSubsetsMemoized list n = fst $ genSubsetsCache list n Map.empty

-- Create a graph from a list of edges
createGraph :: [(Vertex, Vertex)] -> Graph
createGraph edges =
    let addEdge m (v1, v2) = 
            let m1 = Map.insertWith (++) v1 [v2] m
                m2 = Map.insertWith (++) v2 [v1] m1
            in m2
    in foldl addEdge Map.empty edges

-- Helper function to split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Generate all subsets of size k with depth control
genSubsetsParallel :: Int -> [a] -> Int -> [[a]]
genSubsetsParallel depth xs k
    | k == 0 = [[]]
    | null xs = []
    | depth <= 0 = genSubsets xs k  -- Fall back to sequential for deep recursion
    | otherwise = case xs of
        [] -> []
        (x:xs') -> 
            let withX = map (x:) (genSubsetsParallel (depth-1) xs' (k-1))
                withoutX = genSubsetsParallel (depth-1) xs' k
            in withX `par` (withoutX `pseq` withX ++ withoutX)
