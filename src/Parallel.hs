module Parallel 
    ( vertexCoverParallel 
    ) where

import Common
import Data.List (nub, minimumBy)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, catMaybes)
import Control.Parallel.Strategies
import Control.DeepSeq (force)
import Data.Ord (comparing)

vertexCoverParallel :: Graph -> Maybe [Vertex]
vertexCoverParallel graph =
    let vertices = Map.keys graph
        edges = generateEdges graph
        maxSize = length vertices
        chunkSize = 1000  -- Size of chunks for parallel processing

        -- Process a chunk of subsets and find valid covers
        processChunk :: [[Vertex]] -> [[Vertex]]
        processChunk chunk = 
            filter (\subset -> verifyVertexCover subset edges) chunk

        -- Check subsets of a given size using chunks
        checkSubsetsSize :: Int -> [[Vertex]]
        checkSubsetsSize size =
            let allSubsets = genSubsetsParallel 3 vertices size
                chunks = chunksOf chunkSize allSubsets
                -- Process chunks in parallel
                processedChunks = map processChunk chunks `using` parList rdeepseq
            in concat processedChunks

        -- Generate subsets for each size in parallel
        candidatesBySize = [1..maxSize] `using` parList rdeepseq
        -- For each size, generate and verify subsets in parallel
        solutions = map (\size -> 
            let validCovers = checkSubsetsSize size
            in if null validCovers
               then Nothing
               else Just $ minimumBy (comparing length) validCovers) candidatesBySize

    in listToMaybe $ catMaybes solutions
