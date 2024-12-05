module Incremental 
    ( incrementalParallel
    ) where

import Common
import qualified Data.Map as Map
import Control.Parallel.Strategies
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (listToMaybe, catMaybes)
import Control.DeepSeq (deepseq, force)

incrementalParallel :: Graph -> Maybe [Vertex]
incrementalParallel graph =
    let vertices = Map.keys graph
        edges = generateEdges graph
        k = length vertices
        chunkSize = 200 -- Size of chunks for parallel processing

        -- Process a chunk of subsets and find valid covers
        processChunk :: [[Vertex]] -> [[Vertex]]
        processChunk chunk = 
            filter (\subset -> verifyVertexCover subset edges) chunk

        -- Check subsets of a given size using chunks
        checkSubsetsSize :: Int -> [[Vertex]]
        checkSubsetsSize size =
            let allSubsets = genSubsetsMemoized vertices size
                chunks = chunksOf chunkSize allSubsets
                -- Process chunks in parallel
                processedChunks = map processChunk chunks `using` parBuffer 1000 rdeepseq
            in concat processedChunks

        -- Try each size sequentially until we find a solution
        findSolution :: Int -> Maybe [Vertex]
        findSolution size
            | size > k = Nothing
            | otherwise =
                let validCovers = checkSubsetsSize size
                in if null validCovers
                   then findSolution (size + 1)
                   else Just $ minimumBy (comparing length) validCovers

    in findSolution 1
