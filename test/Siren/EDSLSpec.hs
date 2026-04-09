module Siren.EDSLSpec (tests) where

import Siren
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (Property, testProperty, (===))

tests :: TestTree
tests =
    testGroup
        "Siren.EDSL"
        [ testCase "buildGraph fails for missing target node" unit_missingTargetNodeFails
        , testCase "buildGraph succeeds for complete graph" unit_buildGraphSucceeds
        , testCase "bulk node and edge helpers build valid graphs" unit_bulkHelpersBuildGraph
        , testCase "chain helper creates path edges" unit_chainCreatesPath
        , testProperty "edge == edgeWithLabel Nothing" prop_unlabeledEdgeEquivalent
        , testProperty "edgeL == edgeWithLabel Just" prop_labeledEdgeEquivalent
        ]

unit_missingTargetNodeFails :: IO ()
unit_missingTargetNodeFails =
    case buildGraph (node "a" "A" <> edge "a" "b") of
        Left err -> assertBool "Error should mention unknown target node" ("unknown target node" `contains` err)
        Right _ -> assertFailure "Expected missing-node validation to fail"

unit_buildGraphSucceeds :: IO ()
unit_buildGraphSucceeds =
    case buildGraph (node "a" "A" <> node "b" "B" <> edge "a" "b") of
        Left err -> assertFailure ("Expected valid graph but got error: " <> err)
        Right graph -> do
            assertEqual "Should contain one edge" 1 (length (graphEdges graph))
            assertEqual "Should contain two nodes" 2 (length (graphNodes graph))

prop_unlabeledEdgeEquivalent :: String -> String -> String -> String -> Property
prop_unlabeledEdgeEquivalent aId bId aLabel bLabel =
    buildGraph builderUsingEdge === buildGraph builderUsingEdgeWithLabel
  where
    builderUsingEdge =
            node aId aLabel <>
            node bId bLabel <>
            edge aId bId

    builderUsingEdgeWithLabel =
            node aId aLabel <>
            node bId bLabel <>
            edgeWithLabel aId bId Nothing

unit_bulkHelpersBuildGraph :: IO ()
unit_bulkHelpersBuildGraph =
    case buildGraph builder of
        Left err -> assertFailure ("Expected valid graph but got error: " <> err)
        Right graph -> do
            assertEqual "Should contain three edges" 3 (length (graphEdges graph))
            assertEqual "Should contain four nodes" 4 (length (graphNodes graph))
  where
    builder =
            nodes [("a", "A"), ("b", "B")] <>
            roundedNodes [("c", "C")] <>
            diamondNodes [("d", "D")] <>
            edges [("a", "b"), ("b", "c")] <>
            labeledEdges [("c", "d", "finish")]

unit_chainCreatesPath :: IO ()
unit_chainCreatesPath =
    case buildGraph (nodes [("a", "A"), ("b", "B"), ("c", "C"), ("d", "D")] <> chain ["a", "b", "c", "d"]) of
        Left err -> assertFailure ("Expected valid graph but got error: " <> err)
        Right graph -> assertEqual "chain with four nodes should create three edges" 3 (length (graphEdges graph))

prop_labeledEdgeEquivalent :: String -> String -> String -> String -> String -> Property
prop_labeledEdgeEquivalent aId bId aLabel bLabel edgeText =
    buildGraph builderUsingEdgeL === buildGraph builderUsingEdgeWithLabel
  where
    builderUsingEdgeL =
            node aId aLabel <>
            node bId bLabel <>
            edgeL aId bId edgeText

    builderUsingEdgeWithLabel =
            node aId aLabel <>
            node bId bLabel <>
            edgeWithLabel aId bId (Just edgeText)

contains :: Eq a => [a] -> [a] -> Bool
contains needle haystack = any (needle `prefixOf`) (tails haystack)

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x : xs) (y : ys) = x == y && prefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails value@(_ : xs) = value : tails xs
