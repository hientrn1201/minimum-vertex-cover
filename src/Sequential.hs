module Sequential 
    ( bruteForce 
    ) where

import Common
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)

-- Sequential (Brute Force) Algorithm
bruteForce :: Graph -> Maybe [Vertex]
bruteForce graph =
    let vertices = Map.keys graph
        k = length vertices
        edges = generateEdges graph
        trySize i =
            let subsets = genSubsets vertices i
            in find (\s -> verifyVertexCover s edges) subsets
        solutions = [trySize i | i <- [1..k]]
    in listToMaybe $ concat $ filter (not . null) solutions
  where
    -- Find first subset satisfying predicate
    find :: (a -> Bool) -> [a] -> [a]
    find _ [] = []
    find p (x:xs) = if p x then [x] else find p xs