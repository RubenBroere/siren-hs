module Siren.LayoutSpec (tests) where

import Data.List (find)
import Siren
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (Positive (..), testProperty)

tests :: TestTree
tests =
    testGroup
        "Siren.Layout"
        [ testCase "simple chain has strictly increasing ranks" unit_simpleRanking
        , testProperty "chain layout respects edge direction in ranks" prop_chainRanksIncrease
        , testCase "LeftRight coordinates are transformed TopDown coordinates" unit_leftRightIsCoordinateTransform
        ]

unit_simpleRanking :: IO ()
unit_simpleRanking =
    case buildGraph (node "a" "A" <> node "b" "B" <> node "c" "C" <> edge "a" "b" <> edge "b" "c") of
        Left err -> assertFailure ("Failed to build test graph: " <> err)
        Right graph -> do
            let laidOut = layoutSugiyamaLike TopDown graph
                rankOf nodeName =
                    positionedRank
                        <$> find
                            (\n -> nodeId (positionedNode n) == NodeId nodeName)
                            (layoutNodes laidOut)
            case (rankOf "a", rankOf "b", rankOf "c") of
                (Just rankA, Just rankB, Just rankC) ->
                    assertBool "Expected strict increasing ranks for a -> b -> c" (rankA < rankB && rankB < rankC)
                _ -> assertFailure "Could not find all nodes in layout"

prop_chainRanksIncrease :: Positive Int -> Bool
prop_chainRanksIncrease (Positive chainLengthRaw) =
    case mkChainGraph chainLength of
        Left _ -> False
        Right graph -> all edgeRespectsRank (graphEdges graph)
  where
    chainLength = max 2 (min 25 chainLengthRaw)

    edgeRespectsRank currentEdge =
        case (rankFor (edgeFrom currentEdge), rankFor (edgeTo currentEdge)) of
            (Just fromRank, Just toRank) -> toRank >= fromRank + 1
            _ -> False

    laidOut = either (const (LaidOutGraph [] [])) (layoutSugiyamaLike TopDown) (mkChainGraph chainLength)

    rankFor currentNodeId =
        positionedRank
            <$> find
                (\n -> nodeId (positionedNode n) == currentNodeId)
                (layoutNodes laidOut)

unit_leftRightIsCoordinateTransform :: IO ()
unit_leftRightIsCoordinateTransform =
    case mkChainGraph 4 of
        Left err -> assertFailure ("Failed to build graph: " <> err)
        Right graph -> do
            let topDownNodes = layoutNodes (layoutSugiyamaLike TopDown graph)
                leftRightNodes = layoutNodes (layoutSugiyamaLike LeftRight graph)
                toPair n = (nodeId (positionedNode n), (positionedX n, positionedY n))
                topDownMap = map toPair topDownNodes
                leftRightMap = map toPair leftRightNodes
            mapM_ (assertNodeTransform leftRightMap) topDownMap
  where
    assertNodeTransform leftRightMap (currentNodeId, (xPos, yPos)) =
        case lookup currentNodeId leftRightMap of
            Nothing -> assertFailure "Node missing in left-right layout"
            Just (xTransformed, yTransformed) -> do
                assertEqual "LeftRight x should be -TopDown y" (negate yPos) xTransformed
                assertEqual "LeftRight y should be TopDown x" xPos yTransformed

mkChainGraph :: Int -> Either String Graph
mkChainGraph n =
    buildGraph (mconcat (nodes ++ edges))
  where
    nodeIds = map show [1 .. n]
    nodes = [node nodeIdValue ("N" <> nodeIdValue) | nodeIdValue <- nodeIds]
    edges =
        [ edge fromNode toNode
        | (fromNode, toNode) <- zip nodeIds (drop 1 nodeIds)
        ]
