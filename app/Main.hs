module Main
 ( main
 ) where

import Lib 
 ( vertexCoverIncrementalParallel
 , createGraph
 , generateEdges
 , verifyVertexCover
 )
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

-- Main function with comprehensive argument handling
main :: IO ()
main = do
    -- Get command-line arguments
    args <- getArgs
    case args of
        [filePath] -> processGraphFile filePath
        ("-h":_) -> printUsage
        ("--help":_) -> printUsage
        [] -> processStandardInput
        _ -> do 
            hPutStrLn stderr "Invalid arguments."
            printUsage
            fail "Incorrect usage"

-- Rest of the code remains the same as in the previous implementation
processGraphFile :: FilePath -> IO ()
processGraphFile filePath = do
    -- Read and process graph data
    graphData <- readGraphData filePath
    processGraph graphData

-- Process graph data from standard input
processStandardInput :: IO ()
processStandardInput = do
    putStrLn "Enter graph edges (one per line, format: vertex1 vertex2, Ctrl-D to finish):"
    contents <- getContents
    let graphData = catMaybes $ map parseEdge (lines contents)
    processGraph graphData

-- Common graph processing logic
processGraph :: [(String, String)] -> IO ()
processGraph graphData 
    | null graphData = do
        hPutStrLn stderr "Error: No valid graph edges provided."
        fail "Empty graph"
    | otherwise = do
        let graph = createGraph graphData
        
        -- Print input graph details
        putStrLn "Input Graph Details:"
        putStrLn $ "Vertices: " ++ show (Map.keys graph)
        putStrLn $ "Edges: " ++ show (generateEdges graph)
        
        -- Solve vertex cover
        case vertexCoverIncrementalParallel graph of
            Nothing -> putStrLn "No vertex cover found."
            Just cover -> do
                putStrLn "\nMinimum Vertex Cover:"
                print cover
                
                -- Verify the solution
                let edges = generateEdges graph
                putStrLn $ "\nVerification: " ++ 
                    show (verifyVertexCover cover edges)
                
                -- Additional statistics
                putStrLn $ "Cover Size: " ++ show (length cover)
                putStrLn $ "Total Graph Vertices: " ++ show (Map.size graph)

-- Function to read graph data from a text file
readGraphData :: FilePath -> IO [(String, String)]
readGraphData filePath = do
    contents <- readFile filePath
    let edges = map parseEdge (lines contents)
    case catMaybes edges of
        [] -> do
            hPutStrLn stderr $ "Error: No valid edges found in " ++ filePath
            fail "Invalid graph file"
        validEdges -> return validEdges

-- Function to parse a line into an edge
parseEdge :: String -> Maybe (String, String)
parseEdge line =
    case words line of
        [v1, v2] | v1 /= v2 -> Just (v1, v2) -- Prevent self-loops
        _ -> Nothing -- Invalid case

-- Print usage instructions
printUsage :: IO ()
printUsage = do
    putStrLn "Vertex Cover Solver"
    putStrLn "Usage:"
    putStrLn "  vertex-cover <graph_file.txt>"
    putStrLn "  vertex-cover  (for interactive input)"
    putStrLn "Options:"
    putStrLn "  -h, --help    Show this help message"
    putStrLn "\nInput Format:"
    putStrLn "  One edge per line: vertex1 vertex2"
    putStrLn "  Example:"
    putStrLn "    A B"
    putStrLn "    B C"
    putStrLn "    C D"