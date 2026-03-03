module Siren.TypesSpec (tests) where

import qualified Data.Map.Strict as Map
import Siren.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (Property, testProperty, (===))

tests :: TestTree
tests =
    testGroup
        "Siren.Types"
        [ testCase "insertNode replaces existing node with same NodeId" unit_insertNodeReplaces
        , testCase "insertEdge prepends edge to list" unit_insertEdgePrepends
        , testProperty "NodeId roundtrips through unNodeId" prop_nodeIdRoundtrip
        ]

unit_insertNodeReplaces :: IO ()
unit_insertNodeReplaces = do
    let firstNode = Node (NodeId "a") "Old" Rectangle
        secondNode = Node (NodeId "a") "New" Diamond
        graph = insertNode secondNode (insertNode firstNode emptyGraph)
    assertEqual "Graph should contain exactly one node" 1 (Map.size (graphNodes graph))
    assertEqual "Node data should be replaced by second insert" (Just secondNode) (Map.lookup (NodeId "a") (graphNodes graph))

unit_insertEdgePrepends :: IO ()
unit_insertEdgePrepends = do
    let edgeAB = Edge (NodeId "a") (NodeId "b") Nothing
        edgeBC = Edge (NodeId "b") (NodeId "c") (Just "label")
        graph = insertEdge edgeBC (insertEdge edgeAB emptyGraph)
    assertEqual "Most recently inserted edge should be first" [edgeBC, edgeAB] (graphEdges graph)

prop_nodeIdRoundtrip :: String -> Property
prop_nodeIdRoundtrip value = unNodeId (NodeId value) === value
