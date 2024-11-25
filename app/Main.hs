module Main
    ( main
    ) where

import Lib (vertexCoverBrute, createGraph)
import System.Environment (getArgs)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    -- Get command-line arguments
    args <- getArgs
    case args of
        [filePath] -> do
            -- Read the graph data from the file
            graphData <- readGraphData filePath
            
            -- Create the graph from the parsed data
            let graph = createGraph graphData
            
            -- Run the vertex cover algorithm
            case vertexCoverBrute graph of
                Nothing -> putStrLn "No vertex cover found."
                Just cover -> do
                    putStrLn "Minimum Vertex Cover:"
                    print cover
        _ -> putStrLn "Usage: minimum-vertex-cover <path_to_graph_data.txt>"

-- Function to read graph data from a text file
readGraphData :: FilePath -> IO [(String, String)]
readGraphData filePath = do
    contents <- readFile filePath
    let edges = map parseEdge (lines contents)
    return $ catMaybes edges  -- Filter out invalid edges

-- Function to parse a line into an edge
parseEdge :: String -> Maybe (String, String)
parseEdge line =
    let wordsList = words line
    in case wordsList of
        [v1, v2] -> Just (v1, v2)  -- Valid case with exactly two vertices
        _         -> Nothing  -- Invalid case