module Parallel 
    ( vertexCoverParallel 
    ) where

import Common
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, catMaybes)
import Control.Parallel.Strategies
import Control.DeepSeq (force)

vertexCoverParallel :: Graph -> Maybe [Vertex]
vertexCoverParallel graph =
    let vertices = Map.keys graph
        edges = generateEdges graph
        maxSize = length vertices
        -- Generate subsets for each size in parallel
        candidatesBySize = [1..maxSize] `using` parList rdeepseq
        -- For each size, generate and verify subsets in parallel
        solutions = map (\size -> 
            let subsets = genSubsetsParallel 3 vertices size  -- Control recursion depth
                verificationResults = map (\subset -> 
                    let isCover = verifyVertexCover subset edges 
                    in if isCover
                        then Just subset 
                        else Nothing) subsets `using` parList rdeepseq
            in verificationResults `using` parBuffer 100 rdeepseq) candidatesBySize
    in listToMaybe $ catMaybes $ concat solutions
