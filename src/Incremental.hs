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
import Data.Bits

{-
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

      -- Strictly force evaluation of the chunk
      processChunkLazy :: [[Vertex]] -> [[Vertex]]
      processChunkLazy chunk = chunk `deepseq` -- Force evaluation of chunk
                               filter (\subset -> verifyVertexCover subset edges) chunk

      -- Generate subsets lazily, immediately processing each subset
      genSubsetsLazy :: [a] -> Int -> [[a]]
      genSubsetsLazy xs k = go xs k [[]]
        where
          go [] _ acc = acc
          go _ 0 acc = acc
          go (x:xs) k acc = go xs k (acc ++ map (x:) acc)

      -- Check subsets of a given size using chunks
      checkSubsetsSize :: Int -> Maybe [Vertex]
      checkSubsetsSize size =
        let allSubsets = genSubsetsLazy vertices size
            -- Using strict chunking and parallelism
            chunks = chunksOf chunkSize allSubsets
            processedChunks = map processChunkLazy chunks `using` parBuffer 1000 rdeepseq
        in -- Find the first valid vertex cover
           listToMaybe $ concat processedChunks

      -- Try each size sequentially until we find a solution
      findSolution :: Int -> Maybe [Vertex]
      findSolution size
        | size > k = Nothing
        | otherwise =
            case checkSubsetsSize size of
              Nothing -> findSolution (size + 1)
              Just validCover -> Just validCover

  in findSolution 1

incrementalParallel :: Graph -> Maybe [Vertex]
incrementalParallel graph =
  let vertices = Map.keys graph
      edges = generateEdges graph
      k = length vertices
      chunkSize = 200 -- Size of chunks for parallel processing

      -- Process a chunk of subsets and find valid covers
      processChunkLazy :: [Vertex] -> Maybe [Vertex]
      processChunkLazy subset =
        if verifyVertexCover subset edges then Just subset else Nothing

      -- Generate subsets lazily, checking each one immediately
      genSubsetsLazy :: [a] -> Int -> [a] -> [Maybe [a]]
      genSubsetsLazy [] _ _ = []  -- No more subsets
      genSubsetsLazy _ 0 acc = [Just acc]  -- Found a valid subset of size k
      genSubsetsLazy (x:xs) k acc =
        -- Check both including and excluding the current element
        genSubsetsLazy xs (k-1) (x:acc) ++ genSubsetsLazy xs k acc

      -- Check subsets of a given size using chunks
      checkSubsetsSize :: Int -> Maybe [Vertex]
      checkSubsetsSize size =
        let chunks = chunksOf chunkSize vertices
            -- Process each chunk lazily and extract valid subsets
            processedChunks = concatMap (\chunk -> genSubsetsLazy chunk size []) chunks
            -- Use catMaybes directly to filter out Nothing values and get valid subsets
            validSubsets = catMaybes processedChunks
        in listToMaybe validSubsets

      -- Try each size sequentially until we find a solution
      findSolution :: Int -> Maybe [Vertex]
      findSolution size
        | size > k = Nothing
        | otherwise =
            case checkSubsetsSize size of
              Nothing -> findSolution (size + 1)
              Just validCover -> Just validCover

  in findSolution 1

incrementalParallel :: Graph -> Maybe [Vertex]
incrementalParallel graph =
  let vertices = Map.keys graph
      edges = generateEdges graph
      k = length vertices
      chunkSize = 200 -- Size of chunks for parallel processing

      -- Process a chunk of subsets and verify if they are valid vertex covers
      processChunkLazy :: [Vertex] -> Maybe [Vertex]
      processChunkLazy subset =
        if verifyVertexCover subset edges then Just subset else Nothing

      -- Generate subsets lazily, checking each one immediately
      genSubsetsLazy :: [a] -> Int -> [a] -> [[a]]
      genSubsetsLazy [] _ _ = []  -- No more subsets
      genSubsetsLazy _ 0 acc = [acc]  -- Found a valid subset of size k
      genSubsetsLazy (x:xs) k acc
        | k > 0     = genSubsetsLazy xs (k-1) (x:acc) ++ genSubsetsLazy xs k acc
        | otherwise = []

      -- Check subsets of a given size using chunks
      checkSubsetsSize :: Int -> Maybe [Vertex]
      checkSubsetsSize size = 
        let chunks = chunksOf chunkSize vertices
            -- Generate subsets lazily, process them immediately, and apply verification
            processedChunks = concatMap (\chunk -> map processChunkLazy (genSubsetsLazy chunk size [])) chunks
            -- Filter out Nothing values using catMaybes
            validSubsets = catMaybes (processedChunks `using` parBuffer 1000 rdeepseq)
        in if null validSubsets then Nothing else Just (minimumBy (comparing length) validSubsets)

      -- Try each size sequentially until we find a solution
      findSolution :: Int -> Maybe [Vertex]
      findSolution size
        | size > k = Nothing
        | otherwise =
            case checkSubsetsSize size of
              Nothing -> findSolution (size + 1)
              Just validCover -> Just validCover

  in findSolution 1

incrementalParallel :: Graph -> Maybe [Vertex]
incrementalParallel graph =
  let vertices = Map.keys graph
      edges = generateEdges graph
      k = length vertices
      chunkSize = 500 -- Size of chunks for parallel processing

      -- Process a subset and verify if it's a valid vertex cover
      processChunkLazy :: [Vertex] -> Maybe [Vertex]
      processChunkLazy subset =
        if verifyVertexCover subset edges then Just subset else Nothing

      -- Generate subsets lazily, checking each one immediately
      genSubsetsLazy :: [a] -> Int -> [a] -> [[a]]
      genSubsetsLazy [] _ _ = []  -- No more subsets
      genSubsetsLazy _ 0 acc = [acc]  -- Found a valid subset of size k
      genSubsetsLazy (x:xs) k acc
        | k > 0     = genSubsetsLazy xs (k-1) (x:acc) ++ genSubsetsLazy xs k acc
        | otherwise = []

      -- Check subsets of a given size using chunks, parallelizing subset generation and verification
      checkSubsetsSize :: Int -> Maybe [Vertex]
      checkSubsetsSize size =
        let chunks = chunksOf chunkSize vertices
            -- Process each chunk lazily: generate subsets and verify them in parallel
            processedChunks = concatMap (\chunk ->
              -- Generate subsets for each chunk and immediately verify them using processChunkLazy
              let subsets = genSubsetsLazy chunk size []
              in map processChunkLazy subsets `using` parBuffer 1000 rdeepseq) chunks
            -- Filter out Nothing values and return the first valid subset (if any)
            validSubsets = catMaybes processedChunks
        in if null validSubsets then Nothing else Just (minimumBy (comparing length) validSubsets)

      -- Try each size sequentially until we find a solution
      findSolution :: Int -> Maybe [Vertex]
      findSolution size
        | size > k = Nothing
        | otherwise =
            case checkSubsetsSize size of
              Nothing -> findSolution (size + 1)
              Just validCover -> Just validCover

  in findSolution 1
-}

subsetsOfSizeK :: Int -> [a] -> [[a]]
subsetsOfSizeK k xs = [ [xs !! i | i <- [0..n-1], testBit mask i] | mask <- masks ]
  where
    n = length xs
    -- Make sure the mask is of type Int, which supports bitwise operations
    masks = [ mask | mask <- [0..(2^n - 1)] :: [Int], popCount mask == k]
    testBit :: Int -> Int -> Bool  -- Explicit type for testBit
    testBit m i = (m .&. (1 `shiftL` i)) /= 0

incrementalParallel :: Graph -> Maybe [Vertex]
incrementalParallel graph =
  let vertices = Map.keys graph
      edges = generateEdges graph
      k = length vertices
      chunkSize = 500 -- Size of chunks for parallel processing

      -- Process a subset and verify if it's a valid vertex cover
      processChunkLazy :: [Vertex] -> Maybe [Vertex]
      processChunkLazy subset =
        if verifyVertexCover subset edges then Just subset else Nothing

      genSubsetsLazy :: (NFData a) => [a] -> Int -> [[a]]
      genSubsetsLazy chunk size = subsetsOfSizeK size chunk

      -- Check subsets of a given size using chunks, parallelizing subset generation and verification
      checkSubsetsSize :: Int -> Maybe [Vertex]
      checkSubsetsSize size =
        let chunks = chunksOf chunkSize vertices
            -- Process each chunk lazily: generate subsets and verify them in parallel
            processedChunks = concatMap (\chunk ->
              -- Generate subsets for each chunk and immediately verify them using processChunkLazy
              let subsets = genSubsetsLazy chunk size
              in map processChunkLazy subsets `using` parBuffer 1000 rdeepseq) chunks
            -- Filter out Nothing values and return the first valid subset (if any)
            validSubsets = catMaybes processedChunks
        in if null validSubsets then Nothing else Just (minimumBy (comparing length) validSubsets)

      -- Try each size sequentially until we find a solution
      findSolution :: Int -> Maybe [Vertex]
      findSolution size
        | size > k = Nothing
        | otherwise =
            case checkSubsetsSize size of
              Nothing -> findSolution (size + 1)
              Just validCover -> Just validCover

  in findSolution 1
