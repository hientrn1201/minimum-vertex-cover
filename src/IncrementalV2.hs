module IncrementalV2
 ( incrementalParallelV2
 ) where
import Common
import qualified Data.Map as Map
import Control.Parallel.Strategies
import Data.List (minimumBy, nub)
import Data.Ord (comparing)
import Data.Maybe (listToMaybe)

incrementalParallelV2 :: Graph -> Maybe [Vertex]
incrementalParallelV2 graph =
    let vertices = Map.keys graph
        edges = generateEdges graph
        k = length vertices
        chunkSize = max 1 (length vertices `div` 4)  -- More adaptive chunk size

        -- Get all vertices not in the current cover
        getRemainingVertices :: [Vertex] -> [Vertex]
        getRemainingVertices cover = 
            [v | v <- vertices, v `notElem` cover]

        -- Extend a single cover with new vertices
        extendCover :: [Vertex] -> [[Vertex]]
        extendCover cover =
            let remainingVertices = getRemainingVertices cover
            in [v:cover | v <- remainingVertices]

        -- Process a chunk of covers in parallel
        processChunk :: [[Vertex]] -> [[Vertex]]
        processChunk chunk =
            filter (\subset -> verifyVertexCover subset edges) chunk

        -- Generate and check covers of next size from current valid covers
        generateNextSize :: [[Vertex]] -> [[Vertex]]
        generateNextSize currentCovers =
            let extensions = concatMap extendCover currentCovers
                chunks = chunksOf chunkSize extensions
                processedChunks = map processChunk chunks `using` parList rdeepseq
            in nub $ concat processedChunks

        -- Start with singleton sets and build up
        findSolution :: Int -> [[Vertex]] -> Maybe [Vertex]
        findSolution size currentCovers
            | size > k = Nothing
            | size == 1 =
                -- Initial case: check singletons
                let singletons = [[v] | v <- vertices]
                    chunks = chunksOf chunkSize singletons
                    validSingletons = concat (map processChunk chunks `using` parList rdeepseq)
                in if null validSingletons
                   then findSolution (size + 1) []
                   else Just $ minimumBy (comparing length) validSingletons
            | otherwise =
                let nextSizeCovers = generateNextSize currentCovers
                in if null nextSizeCovers
                   then findSolution (size + 1) []
                   else Just $ minimumBy (comparing length) nextSizeCovers

    in findSolution 1 []