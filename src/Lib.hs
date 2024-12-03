module Lib
    ( Vertex
    , Edge
    , Graph
    , generateEdges
    , verifyVertexCover
    , vertexCoverIncrementalParallel
    , createGraph
    ) where

import Data.List (nub)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (listToMaybe, catMaybes)
import Control.Parallel.Strategies
import Control.Parallel()

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

-- Incremental vertex cover solver with parallel processing
vertexCoverIncrementalParallel :: Graph -> Maybe [Vertex]
vertexCoverIncrementalParallel graph =
    let vertices = Map.keys graph
        edges = generateEdges graph
        maxSize = length vertices
        
        -- Start with single-vertex sets
        singletons = map (:[]) vertices
        
        -- Extend existing subsets by adding one vertex
        extendSubsets :: [[Vertex]] -> [[Vertex]]
        extendSubsets currentSubsets = 
            nub [subset ++ [newVertex] 
                | subset <- currentSubsets, 
                  newVertex <- vertices,
                  case subset of
                    [] -> True
                    xs -> last xs < newVertex]
        
        -- Parallel verification of subset candidates
        verifySubsets :: [[Vertex]] -> [Maybe [Vertex]]
        verifySubsets subsets = 
            map (\subset -> 
                if verifyVertexCover subset edges 
                then Just subset 
                else Nothing) subsets `using` parList rdeepseq
        
        -- Incremental search that builds upon previous results
        searchWithMemory :: [[Vertex]] -> Int -> Maybe [Vertex]
        searchWithMemory prevSubsets targetSize
            | null prevSubsets = Nothing
            | otherwise = 
                let newSubsets = extendSubsets prevSubsets
                    results = verifySubsets newSubsets
                    solution = listToMaybe $ catMaybes results
                in case solution of
                    Just s -> Just s
                    Nothing -> if length (head newSubsets) < targetSize
                              then searchWithMemory newSubsets targetSize
                              else Nothing
    
    in searchWithMemory singletons maxSize

-- Create a graph from a list of edges
createGraph :: [(Vertex, Vertex)] -> Graph
createGraph edges =
    let addEdge m (v1, v2) = 
            let m1 = Map.insertWith (++) v1 [v2] m
                m2 = Map.insertWith (++) v2 [v1] m1
            in m2
    in foldl addEdge Map.empty edges