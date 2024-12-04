module Incremental 
    ( incrementalParallel
    ) where

import Common
import qualified Data.Map as Map
import Control.Parallel.Strategies
import Data.List (nub, sortBy)
import Data.Maybe (fromMaybe)

-- Parallel Incremental Vertex Cover Solver
incrementalParallel :: Graph -> Maybe [Vertex]
incrementalParallel graph =
    let vertices = Map.keys graph
        edges = generateEdges graph
        k = length vertices

        -- Generate all possible vertex covers
        allPossibleCovers = 
            concat [genSubsets vertices size | size <- [1 .. k]]

        -- Parallel verification of vertex covers
        verifyCovers :: [[Vertex]] -> Maybe [Vertex]
        verifyCovers covers = 
            let validCovers = parMap rdeepseq 
                    (\cover -> if verifyVertexCover cover edges then Just cover else Nothing) 
                    covers
                validResults = [c | Just c <- validCovers]
            in if null validResults 
               then Nothing 
               else Just (minimumCover validResults)

        -- Find the smallest vertex cover
        minimumCover :: [[Vertex]] -> [Vertex]
        minimumCover covers = 
            head $ sortBy (\a b -> compare (length a) (length b)) covers

    in verifyCovers allPossibleCovers