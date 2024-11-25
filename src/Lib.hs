module Lib
    ( Vertex
    , Edge
    , Graph
    , generateEdges
    , verifyVertexCover
    , vertexCoverBrute
    -- , vertexCoverParallel
    -- , vertexCoverParallelChunked
    , createGraph
    ) where
import Data.List()
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.List()
import Data.Ord()
-- import Control.Parallel.Strategies
-- import Control.Parallel
-- import Control.DeepSeq()

type Vertex = String
type Edge = (Vertex, Vertex)
type Graph = Map Vertex [Vertex]

-- Generate all edges from the graph
generateEdges :: Graph -> [Edge]
generateEdges graph = 
    [(v1, v2) | (v1, neighbors) <- Map.toList graph,
                v2 <- neighbors,
                v1 < v2]  -- Only include each edge once

-- Verify if a set of vertices forms a valid vertex cover
verifyVertexCover :: [Vertex] -> [Edge] -> Bool
verifyVertexCover cover edges =
    all (\(v1, v2) -> v1 `elem` cover || v2 `elem` cover) edges

-- Generate all subsets of size k
genSubsets :: [a] -> Int -> [[a]]
genSubsets _ 0 = [[]]
genSubsets [] _ = []
genSubsets (x:xs) k = 
    let withX = map (x:) (genSubsets xs (k-1))
        withoutX = genSubsets xs k
    in withX ++ withoutX

-- -- Helper function to chunk a list into n approximately equal parts
-- chunk :: Int -> [a] -> [[a]]
-- chunk n xs = 
--     let len = length xs
--         chunkSize = (len + n - 1) `div` n
--     in take n $ map (take chunkSize) $ iterate (drop chunkSize) xs

-- -- Parallel subset generation with depth control
-- genSubsetsParallel :: Int -> [a] -> Int -> [[a]]
-- genSubsetsParallel depth xs k
--     | k == 0 = [[]]
--     | null xs = []
--     | depth <= 0 = genSubsets xs k  -- Fall back to sequential for deep recursion
--     | otherwise = case xs of
--         [] -> []
--         (x:xs') -> 
--             let withX = map (x:) (genSubsetsParallel (depth-1) xs' (k-1))
--                 withoutX = genSubsetsParallel (depth-1) xs' k
--             in withX `par` (withoutX `pseq` withX ++ withoutX)

-- -- Parallel verification of a chunk of subsets
-- verifyChunk :: [Edge] -> [[Vertex]] -> Maybe [Vertex]
-- verifyChunk edges subsets = 
--     listToMaybe $ filter (\s -> verifyVertexCover s edges) subsets

-- Original sequential vertex cover
vertexCoverBrute :: Graph -> Maybe [Vertex]
vertexCoverBrute graph =
    let vertices = Map.keys graph
        k = length vertices
        edges = generateEdges graph
        trySize i =
            let subsets = genSubsets vertices i
            in find (\s -> verifyVertexCover s edges) subsets
        solutions = [trySize i | i <- [1..k]]
    in listToMaybe $ concat $ filter (not . null) solutions
  where
    find _ [] = []
    find p (x:xs) = if p x then [x] else find p xs

-- -- Version 1: Parallel subset generation with parallel verification
-- vertexCoverParallel :: Graph -> Maybe [Vertex]
-- vertexCoverParallel graph =
--     let vertices = Map.keys graph
--         edges = generateEdges graph
--         maxSize = length vertices
--         -- Generate subsets for each size in parallel
--         candidatesBySize = [1..maxSize] `using` parList rdeepseq
--         -- For each size, generate and verify subsets in parallel
--         solutions = map (\size -> 
--             let subsets = genSubsetsParallel 3 vertices size  -- Control recursion depth
--                 verificationResults = map (\subset -> 
--                     verifyVertexCover subset edges `par` 
--                     if verifyVertexCover subset edges 
--                         then Just subset 
--                         else Nothing) subsets
--             in verificationResults `using` parBuffer 100 rdeepseq) candidatesBySize
--     in listToMaybe $ catMaybes $ concat solutions

-- -- Version 2: Chunked parallel verification with work stealing
-- vertexCoverParallelChunked :: Int -> Graph -> Maybe [Vertex]
-- vertexCoverParallelChunked numChunks graph =
--     let vertices = Map.keys graph
--         edges = generateEdges graph
--         maxSize = length vertices
--         -- Process each size sequentially, but parallelize within each size
--         processSize size = 
--             let subsets = genSubsets vertices size
--                 -- Divide subsets into chunks for parallel processing
--                 subsetsChunks = chunk numChunks subsets
--                 -- Process each chunk in parallel
--                 chunkResults = map (verifyChunk edges) subsetsChunks 
--                     `using` parBuffer numChunks rdeepseq
--             in catMaybes chunkResults
--         -- Try each size until we find a solution
--         solutions = [processSize size | size <- [1..maxSize]]
--     in listToMaybe $ concat solutions

-- Helper function to create a graph from list of edges
createGraph :: [(Vertex, Vertex)] -> Graph
createGraph edges =
    let addEdge m (v1, v2) = 
            let m1 = Map.insertWith (++) v1 [v2] m
                m2 = Map.insertWith (++) v2 [v1] m1
            in m2
    in foldl addEdge Map.empty edges
