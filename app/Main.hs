module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

import Common
import Sequential
import Parallel
import Incremental
import IncrementalV2
import Incrementalv3

-- Method enum
data Method = Sequential | Parallel | Incremental | IncrementalV2
    deriving (Read, Show, Eq)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:methodStrs) -> 
            case parseMethodList methodStrs of
                Just methods -> processGraphFile filePath methods
                Nothing -> showUsage
        _ -> showUsage

showUsage :: IO ()
showUsage = do
    hPutStrLn stderr "Usage: minimum-vertex-cover-exe <graph_file.txt> [methods...]"
    hPutStrLn stderr "Methods available:"
    hPutStrLn stderr "  Sequential    - Run sequential algorithm"
    hPutStrLn stderr "  Parallel      - Run parallel algorithm"
    hPutStrLn stderr "  Incremental   - Run incremental parallel algorithm"
    hPutStrLn stderr "  IncrementalV2   - Run incremental V2 parallel algorithm"
    hPutStrLn stderr "If no methods specified, runs all methods"
    fail "Incorrect usage"

parseMethodList :: [String] -> Maybe [Method]
parseMethodList [] = Just [Sequential, Parallel, Incremental]  -- Default: all methods
parseMethodList strs = mapM readMaybe strs

processGraphFile :: FilePath -> [Method] -> IO ()
processGraphFile filePath methods = do
    putStrLn $ "Processing graph from: " ++ filePath
    graphData <- readGraphFile filePath
    let graph = createGraph graphData
    
    -- Print input graph details
    putStrLn "\nInput Graph:"
    putStrLn $ "Vertices: " ++ show (Map.keys graph)
    putStrLn $ "Edges: " ++ show (generateEdges graph)
    
    -- Run selected methods and print results
    putStrLn "\nResults:"
    mapM_ (runMethod graph) methods

runMethod :: Graph -> Method -> IO ()
runMethod graph method = case method of
    Sequential -> do
        putStr "Sequential: "
        printResult $ bruteForce graph
    Parallel -> do
        putStr "Parallel: "
        printResult $ vertexCoverParallel graph
    Incremental -> do
        putStr "Incremental: "
        printResult $ incrementalParallel graph
    IncrementalV2 -> do
        putStr "Incremental: "
        printResult $ incrementalParallelV2 graph

printResult :: Maybe [Vertex] -> IO ()
printResult Nothing = putStrLn "No vertex cover found"
printResult (Just cover) = putStrLn $ "Found vertex cover: " ++ show cover

-- Read and parse graph file
readGraphFile :: FilePath -> IO [(String, String)]
readGraphFile filePath = do
    contents <- readFile filePath
    let edges = map parseEdge (lines contents)
    case catMaybes edges of
        [] -> fail $ "No valid edges found in " ++ filePath
        validEdges -> return validEdges

-- Parse a single edge from a line of text
parseEdge :: String -> Maybe (String, String)
parseEdge line = case words line of
    [v1, v2] | v1 /= v2 -> Just (v1, v2)  -- Prevent self-loops
    _ -> Nothing
