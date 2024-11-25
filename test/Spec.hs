module Main (main) where

import Test.Hspec
import qualified Data.Map as Map
import Lib

-- Test graphs
graph351 :: [(Vertex, Vertex)]
graph351 = [
    ("A", "B"), 
    ("B", "C"), 
    ("C", "D"), ("C", "E"),
    ("D", "E"), ("D", "F"), ("D", "G"),
    ("E", "F")
    ]

graphConnected :: [(Vertex, Vertex)]
graphConnected = [
    ("A", "B"), ("A", "C"), ("A", "D"), ("A", "E"), ("A", "F"), ("A", "G"),
    ("B", "C"), ("B", "D"), ("B", "E"), ("B", "F"), ("B", "G"),
    ("C", "D"), ("C", "E"), ("C", "F"), ("C", "G"),
    ("D", "E"), ("D", "F"), ("D", "G"),
    ("E", "F"), ("E", "G"),
    ("F", "G")
    ]

graphBipartite :: [(Vertex, Vertex)]
graphBipartite = [
    ("A", "F"), ("A", "G"),
    ("B", "F"),
    ("C", "H"), ("C", "G"),
    ("D", "H"), ("D", "J"),
    ("E", "I"), ("E", "J")
    ]

graphBig :: [(Vertex, Vertex)]
graphBig = [
    ("A", "B"), ("A", "E"), ("A", "D"),
    ("B", "E"), ("B", "F"), ("B", "C"),
    ("C", "F"),
    ("D", "E"), ("D", "H"), ("D", "G"),
    ("G", "H"), ("G", "K"), ("G", "J"),
    ("J", "K"), ("J", "N"), ("J", "M"),
    ("M", "N"), ("M", "Q"), ("M", "P"),
    ("P", "Q"), ("P", "T"), ("P", "S"),
    ("F", "E"), ("F", "I"),
    ("I", "H"), ("I", "E"), ("I", "L"),
    ("L", "K"), ("L", "H"), ("L", "O"),
    ("O", "N"), ("O", "K"), ("O", "R"),
    ("R", "Q"), ("R", "N"), ("R", "U"),
    ("E", "H"),
    ("H", "K"),
    ("K", "N"),
    ("N", "Q"),
    ("Q", "T"),
    ("T", "U"),
    ("S", "T")
    ]

runVertexCoverTest :: String -> [(Vertex, Vertex)] -> Spec
runVertexCoverTest name edges = 
    describe ("Vertex Cover for " ++ name) $ do
        let graph = createGraph edges
        
        it "sequential and parallel versions should give same result" $ do
            let sequential = vertexCoverBrute graph
                parallel1 = vertexCoverParallel graph
                -- parallel2 = vertexCoverParallelChunked 4 graph
            parallel1 `shouldBe` sequential
            -- parallel2 `shouldBe` sequential

        it "should be a valid vertex cover" $ do
            let result = vertexCoverBrute graph
            case result of
                Nothing -> expectationFailure "No vertex cover found"
                Just cover -> verifyVertexCover cover (generateEdges graph) `shouldBe` True

main :: IO ()
main = hspec $ do
    runVertexCoverTest "Graph 351" graph351
    runVertexCoverTest "Complete Graph" graphConnected
    runVertexCoverTest "Bipartite Graph" graphBipartite
    runVertexCoverTest "Big Graph" graphBig
