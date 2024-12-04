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