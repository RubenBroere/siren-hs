module Siren.LayoutSpec (tests) where

import Data.List (find)
import Data.Maybe (isJust)
import Siren
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (Positive(..), testProperty)

tests :: TestTree
tests =
  testGroup
    "Siren.Layout"
    [ testCase "simple chain has strictly increasing ranks" unit_simpleRanking
    , testCase "dagre simple chain has strictly increasing ranks" unit_dagreSimpleRanking
    , testProperty "chain layout respects edge direction in ranks" prop_chainRanksIncrease
    , testProperty "dagre chain layout respects edge direction in ranks" prop_dagreChainRanksIncrease
    , testCase "LeftRight coordinates are transformed TopDown coordinates" unit_leftRightIsCoordinateTransform
    , testCase "dagre produces finite positioned nodes" unit_dagreHasFiniteCoordinates
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
                (\nodeValue -> nodeId (positionedNode nodeValue) == NodeId nodeName)
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
    laidOut = either (const (layoutSugiyamaLike TopDown emptyGraph)) (layoutSugiyamaLike TopDown) (mkChainGraph chainLength)

    edgeRespectsRank currentEdge =
      case (rankFor (edgeFrom currentEdge), rankFor (edgeTo currentEdge)) of
        (Just fromRank, Just toRank) -> toRank >= fromRank + 1
        _ -> False

    rankFor currentNodeId =
      positionedRank
        <$> find
          (\nodeValue -> nodeId (positionedNode nodeValue) == currentNodeId)
          (layoutNodes laidOut)

unit_dagreSimpleRanking :: IO ()
unit_dagreSimpleRanking =
  case buildGraph (node "a" "A" <> node "b" "B" <> node "c" "C" <> edge "a" "b" <> edge "b" "c") of
    Left err -> assertFailure ("Failed to build test graph: " <> err)
    Right graph -> do
      let laidOut = layoutDagre TopDown graph
          rankOf nodeName =
            positionedRank
              <$> find
                (\nodeValue -> nodeId (positionedNode nodeValue) == NodeId nodeName)
                (layoutNodes laidOut)
      case (rankOf "a", rankOf "b", rankOf "c") of
        (Just rankA, Just rankB, Just rankC) ->
          assertBool "Expected strict increasing ranks for a -> b -> c" (rankA < rankB && rankB < rankC)
        _ -> assertFailure "Could not find all nodes in layout"

prop_dagreChainRanksIncrease :: Positive Int -> Bool
prop_dagreChainRanksIncrease (Positive chainLengthRaw) =
  case mkChainGraph chainLength of
    Left _ -> False
    Right graph -> all edgeRespectsRank (graphEdges graph)
  where
    chainLength = max 2 (min 25 chainLengthRaw)
    laidOut = either (const (layoutDagre TopDown emptyGraph)) (layoutDagre TopDown) (mkChainGraph chainLength)

    edgeRespectsRank currentEdge =
      case (rankFor (edgeFrom currentEdge), rankFor (edgeTo currentEdge)) of
        (Just fromRank, Just toRank) -> toRank >= fromRank + 1
        _ -> False

    rankFor currentNodeId =
      positionedRank
        <$> find
          (\nodeValue -> nodeId (positionedNode nodeValue) == currentNodeId)
          (layoutNodes laidOut)

unit_leftRightIsCoordinateTransform :: IO ()
unit_leftRightIsCoordinateTransform =
  case mkChainGraph 4 of
    Left err -> assertFailure ("Failed to build graph: " <> err)
    Right graph -> do
      let topDownNodes = layoutNodes (layoutSugiyamaLike TopDown graph)
          leftRightNodes = layoutNodes (layoutSugiyamaLike LeftRight graph)
          toPair nodeValue = (nodeId (positionedNode nodeValue), (positionedX nodeValue, positionedY nodeValue))
      mapM_ (assertNodeTransform (map toPair leftRightNodes)) (map toPair topDownNodes)
  where
    assertNodeTransform leftRightMap (currentNodeId, (xPos, yPos)) =
      case lookup currentNodeId leftRightMap of
        Nothing -> assertFailure "Node missing in left-right layout"
        Just (xTransformed, yTransformed) -> do
          assertEqual "LeftRight x should be -TopDown y" (negate yPos) xTransformed
          assertEqual "LeftRight y should be TopDown x" xPos yTransformed

unit_dagreHasFiniteCoordinates :: IO ()
unit_dagreHasFiniteCoordinates =
  case mkChainGraph 5 of
    Left err -> assertFailure ("Failed to build graph: " <> err)
    Right graph -> do
      let laidOut = layoutDagre TopDown graph
          nodes = layoutNodes laidOut
          finite nodeValue =
            all isFinite
              [ positionedX nodeValue
              , positionedY nodeValue
              , positionedWidth nodeValue
              , positionedHeight nodeValue
              ]
      assertBool "Expected all dagre coordinates and dimensions to be finite" (all finite nodes)
      assertBool "Expected at least one positioned node" (isJust (find (const True) nodes))
  where
    isFinite x = x == x && x /= (1 / 0) && x /= (-1 / 0)

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