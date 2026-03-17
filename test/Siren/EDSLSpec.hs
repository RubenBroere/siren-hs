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
        , testProperty "edge == edgeWithLabel Nothing" prop_unlabeledEdgeEquivalent
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

contains :: Eq a => [a] -> [a] -> Bool
contains needle haystack = any (needle `prefixOf`) (tails haystack)

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x : xs) (y : ys) = x == y && prefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails value@(_ : xs) = value : tails xs
