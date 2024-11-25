module Lib
    ( Vertex
    , Edge
    , Graph
    , generateEdges
    , verifyVertexCover
    , vertexCoverBrute
    , createGraph
    , graph351
    , graphConnected
    , graphBipartite
    , graphBig
    , runTestCase
    , runTests
    ) where

import Data.List()
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (listToMaybe)

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

-- Find vertex cover using brute force approach
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

-- Helper function to create a graph from list of edges
createGraph :: [(Vertex, Vertex)] -> Graph
createGraph edges =
    let addEdge m (v1, v2) = 
            let m1 = Map.insertWith (++) v1 [v2] m
                m2 = Map.insertWith (++) v2 [v1] m1
            in m2
    in foldl addEdge Map.empty edges

-- Test graphs
graph351 :: [(Vertex, Vertex)]
graph351 = [
    ("A", "B"), 
    ("B", "C"), 
    ("C", "D"), ("C", "E"),
    ("D", "E"), ("D", "F"), ("D", "G"),
    ("E", "F")
    ]

graphConnected :: [(Vertex, Vertex)]
graphConnected = [
    ("A", "B"), ("A", "C"), ("A", "D"), ("A", "E"), ("A", "F"), ("A", "G"),
    ("B", "C"), ("B", "D"), ("B", "E"), ("B", "F"), ("B", "G"),
    ("C", "D"), ("C", "E"), ("C", "F"), ("C", "G"),
    ("D", "E"), ("D", "F"), ("D", "G"),
    ("E", "F"), ("E", "G"),
    ("F", "G")
    ]

graphBipartite :: [(Vertex, Vertex)]
graphBipartite = [
    ("A", "F"), ("A", "G"),
    ("B", "F"),
    ("C", "H"), ("C", "G"),
    ("D", "H"), ("D", "J"),
    ("E", "I"), ("E", "J")
    ]

graphBig :: [(Vertex, Vertex)]
graphBig = [
    ("A", "B"), ("A", "E"), ("A", "D"),
    ("B", "E"), ("B", "F"), ("B", "C"),
    ("C", "F"),
    ("D", "E"), ("D", "H"), ("D", "G"),
    ("G", "H"), ("G", "K"), ("G", "J"),
    ("J", "K"), ("J", "N"), ("J", "M"),
    ("M", "N"), ("M", "Q"), ("M", "P"),
    ("P", "Q"), ("P", "T"), ("P", "S"),
    ("F", "E"), ("F", "I"),
    ("I", "H"), ("I", "E"), ("I", "L"),
    ("L", "K"), ("L", "H"), ("L", "O"),
    ("O", "N"), ("O", "K"), ("O", "R"),
    ("R", "Q"), ("R", "N"), ("R", "U"),
    ("E", "H"),
    ("H", "K"),
    ("K", "N"),
    ("N", "Q"),
    ("Q", "T"),
    ("T", "U"),
    ("S", "T")
    ]

-- Function to run a single test case
runTestCase :: String -> [(Vertex, Vertex)] -> IO ()
runTestCase name edges = do
    putStrLn $ "Running Test Case: " ++ name
    let graph = createGraph edges
    putStrLn $ "Minimum Vertex Cover: " ++ show (vertexCoverBrute graph)
    putStrLn $ "Number of vertices: " ++ show (length $ Map.keys graph)
    putStrLn $ "Number of edges: " ++ show (length $ generateEdges graph)
    putStrLn ""

-- Run all test cases
runTests :: IO ()
runTests = do
    runTestCase "Graph 351" graph351
    runTestCase "Complete Graph" graphConnected
    runTestCase "Bipartite Graph" graphBipartite
    runTestCase "Big Graph" graphBig
