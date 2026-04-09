{-
Gansner, E. R., Koutsofios, E., North, S. C., & Vo, K.-P. (1993).
"A Technique for Drawing Directed Graphs."
/IEEE Transactions on Software Engineering/, 19(3), 214-230.

Brandes, U., & Kopf, B. (2002).
"Fast and Simple Horizontal Coordinate Assignment."
In /Graph Drawing/ (GD 2001), LNCS 2265.

dagrejs/dagre (MIT License):
<https://github.com/dagrejs/dagre>
-}

module Siren.Layout.Dagre
  ( DagreLayout(..)
  , defaultDagreLayout
  , layoutDagre
  , layoutDagreWith
  )
where

import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Siren.Engine (LayoutEngine(..))
import Siren.Layout.Common
  ( layoutNodeDimensions
  , layoutUniformWidthFromNodes
  , transform
  , withComputedMetrics
  )
import Siren.Layout.Types
import Siren.Types

-- | Dagre-style layered layout with configurable spacing.
data DagreLayout = DagreLayout
  { dagreDirection :: Direction  -- ^ Graph flow direction: top-down or left-right.
  , dagreNodeSep :: Double       -- ^ Horizontal spacing between adjacent nodes in a rank.
  , dagreRankSep :: Double       -- ^ Vertical spacing between ranks/layers.
  }
  deriving (Eq, Show)

-- | Smart default configuration for the Dagre-style engine.
--
-- The defaults are tuned for readability with Siren's current SVG renderer
-- and node sizing model.
defaultDagreLayout :: Direction -> DagreLayout
defaultDagreLayout direction =
  DagreLayout
    { dagreDirection = direction
    , dagreNodeSep = 1.4
    , dagreRankSep = 3.4
    }

instance LayoutEngine DagreLayout where
  runLayout = layoutDagreWith

{-|
Layout entry point using default Dagre-style configuration.

This is the simplest API for callers that only need to choose direction.
Internally it delegates to 'layoutDagreWith'.
-}
layoutDagre :: Direction -> Graph -> LaidOutGraph
layoutDagre direction = layoutDagreWith (defaultDagreLayout direction)

{-|
Performs Dagre-style layered layout with explicit configuration.

__Pipeline overview:__

1. __Cycle Handling__:
  The input is transformed with 'makeAcyclic' by reversing back-edges
  discovered by DFS.

2. __Rank Assignment__:
  Ranks are computed by iterative longest-path style relaxation
  (see 'computeRanks').

3. __In-rank Ordering__:
  Nodes within each rank are re-ordered by predecessor barycenter
  (see 'orderByBarycenter').

4. __Coordinate Assignment__:
  Ranks are spaced by 'dagreRankSep', nodes by 'dagreNodeSep', and then
  transformed by 'dagreDirection'.

__Parameters:__

[@config@] Dagre-style spacing and direction settings.

[@graph@] Graph to layout.

__Returns:__

A fully populated 'LaidOutGraph' compatible with Siren renderers.
-}
layoutDagreWith :: DagreLayout -> Graph -> LaidOutGraph
layoutDagreWith config graph =
  withComputedMetrics originalEdges positionedNodes
  where
    nodeList = sortOn (unNodeId . nodeId) (Map.elems (graphNodes graph))
    originalEdges = graphEdges graph
    acyclicEdges = makeAcyclic nodeList originalEdges
    rankMap = computeRanks nodeList acyclicEdges
    orderedRanks = orderByBarycenter nodeList acyclicEdges rankMap
    uniformWidth = layoutUniformWidthFromNodes nodeList
    dimensions = Map.fromList [ (nodeId currentNode, layoutNodeDimensions uniformWidth currentNode) | currentNode <- nodeList ]
    positionMap = assignCoordinates config rankMap orderedRanks dimensions
    positionedNodes = fmap (mkPositionedNode rankMap positionMap dimensions) nodeList

mkPositionedNode :: Map NodeId Int -> Map NodeId (Double, Double) -> Map NodeId (Double, Double) -> Node -> PositionedNode
mkPositionedNode rankMap positionMap dimensions currentNode =
  PositionedNode
    { positionedNode = currentNode
    , positionedX = xPos
    , positionedY = yPos
    , positionedRank = nodeRank
    , positionedWidth = nodeWidth
    , positionedHeight = nodeHeight
    }
  where
    (xPos, yPos) = Map.findWithDefault (0.0, 0.0) (nodeId currentNode) positionMap
    nodeRank = Map.findWithDefault 0 (nodeId currentNode) rankMap
    (nodeWidth, nodeHeight) = Map.findWithDefault (3.2, 1.8) (nodeId currentNode) dimensions

-- | DFS-based cycle breaking: reverse edges that point to an active ancestor.
makeAcyclic :: [Node] -> [Edge] -> [Edge]
makeAcyclic nodes edges = reverse finalEdges
  where
    orderedStarts = fmap nodeId nodes
    edgeMap = Map.fromListWith (++) [ (edgeFrom edgeValue, [edgeValue]) | edgeValue <- stableEdges ]
    stableEdges = sortOn edgeKey edges
    (_, _, finalEdges) = foldl' visitStart (Set.empty, Set.empty, []) orderedStarts

    edgeKey edgeValue =
      ( unNodeId (edgeFrom edgeValue)
      , unNodeId (edgeTo edgeValue)
      , fromMaybe "" (edgeLabel edgeValue)
      )

    visitStart (visited, active, acc) currentId
      | Set.member currentId visited = (visited, active, acc)
      | otherwise = dfs currentId visited active acc

    dfs currentId visited active acc =
      let visitedWithCurrent = Set.insert currentId visited
          activeWithCurrent = Set.insert currentId active
          outgoing = Map.findWithDefault [] currentId edgeMap
          (visitedAfter, activeAfter, accAfter) = foldl' visitEdge (visitedWithCurrent, activeWithCurrent, acc) outgoing
       in (visitedAfter, Set.delete currentId activeAfter, accAfter)

    visitEdge (visitedAcc, activeAcc, edgesAcc) edgeValue
      | Set.member (edgeTo edgeValue) activeAcc =
          (visitedAcc, activeAcc, reverseEdge edgeValue : edgesAcc)
      | Set.member (edgeTo edgeValue) visitedAcc =
          (visitedAcc, activeAcc, edgeValue : edgesAcc)
      | otherwise =
          dfs (edgeTo edgeValue) visitedAcc activeAcc (edgeValue : edgesAcc)

reverseEdge :: Edge -> Edge
reverseEdge edgeValue =
  edgeValue
    { edgeFrom = edgeTo edgeValue
    , edgeTo = edgeFrom edgeValue
    }

{-|
Computes layer/rank assignments for each node.

This implementation uses iterative edge relaxation equivalent to longest-path
layering on acyclic input.

For each edge @(u -> v)@, it enforces:

\[ rank(v) \ge rank(u) + 1 \]

until a fixed point or iteration limit is reached.
-}
computeRanks :: [Node] -> [Edge] -> Map NodeId Int
computeRanks nodes edges = iterateRanks 0 initialRanks
  where
    initialRanks = Map.fromList [ (nodeId currentNode, 0) | currentNode <- nodes ]
    maxIterations = max 1 (length nodes * 2)

    iterateRanks iteration ranks
      | iteration >= maxIterations = ranks
      | updated == ranks = ranks
      | otherwise = iterateRanks (iteration + 1) updated
      where
        updated = foldl' updateRank ranks edges

-- | Applies one edge-relaxation step for rank constraints.
updateRank :: Map NodeId Int -> Edge -> Map NodeId Int
updateRank ranks edgeValue =
  case (Map.lookup (edgeFrom edgeValue) ranks, Map.lookup (edgeTo edgeValue) ranks) of
    (Just fromRank, Just toRank)
      | toRank < fromRank + 1 -> Map.insert (edgeTo edgeValue) (fromRank + 1) ranks
      | otherwise -> ranks
    _ -> ranks

{-|
Orders nodes inside each rank using predecessor barycenters.

The ordering pass processes ranks top-down. For a node in rank @r@,
its key is the average index of predecessors in rank @r-1@.
When no predecessors are found in the previous rank, a neutral key is used,
and node id breaks ties for deterministic output.
-}
orderByBarycenter :: [Node] -> [Edge] -> Map NodeId Int -> Map Int [NodeId]
orderByBarycenter nodes edges rankMap = foldl' orderRank initialOrder sortedRanks
  where
    initialOrder =
      Map.fromListWith (++)
        [ (Map.findWithDefault 0 (nodeId currentNode) rankMap, [nodeId currentNode])
        | currentNode <- sortOn (unNodeId . nodeId) nodes
        ]

    sortedRanks = sortOn id (nub (Map.elems rankMap))

    predecessors =
      Map.fromListWith (++)
        [ (edgeTo edgeValue, [edgeFrom edgeValue])
        | edgeValue <- edges
        ]

    orderRank currentOrder currentRank
      | currentRank <= 0 = currentOrder
      | otherwise =
          let previousRankOrder = Map.findWithDefault [] (currentRank - 1) currentOrder
              previousIndex = Map.fromList (zip previousRankOrder [0 :: Int ..])
              nodesInRank = Map.findWithDefault [] currentRank currentOrder
              rankedNodes = sortOn (nodeSortKey previousIndex) nodesInRank
           in Map.insert currentRank rankedNodes currentOrder
      where
        nodeSortKey previousIndex currentNodeId =
          ( barycenter previousIndex (Map.findWithDefault [] currentNodeId predecessors)
          , unNodeId currentNodeId
          )

-- | Barycenter utility used for in-rank ordering.
barycenter :: Map NodeId Int -> [NodeId] -> Double
barycenter _ [] = 0.0
barycenter indexMap nodeIds =
  case foundIndexes of
    [] -> 0.0
    _ -> fromIntegral (sum foundIndexes) / fromIntegral (length foundIndexes)
  where
    foundIndexes =
      [ index
      | nodeIdValue <- nodeIds
      , Just index <- [Map.lookup nodeIdValue indexMap]
      ]

{-|
Assigns concrete node coordinates from ranks and rank-local ordering.

Coordinates are first assigned in a canonical top-down frame, then transformed
using 'dagreDirection' so callers can request left-right flow.
-}
assignCoordinates :: DagreLayout -> Map NodeId Int -> Map Int [NodeId] -> Map NodeId (Double, Double) -> Map NodeId (Double, Double)
assignCoordinates config rankMap orderedRanks dimensions = foldl' assignRank Map.empty sortedRanks
  where
    sortedRanks = sortOn id (nub (Map.elems rankMap))

    assignRank positionMap currentRank =
      let nodesInRank = Map.findWithDefault [] currentRank orderedRanks
          placements = placeNodesInRank config currentRank dimensions nodesInRank
       in foldl' (insertPlacement (dagreDirection config)) positionMap placements

-- | Places all nodes in a single rank with width-aware spacing and centering.
placeNodesInRank :: DagreLayout -> Int -> Map NodeId (Double, Double) -> [NodeId] -> [(NodeId, Double, Double)]
placeNodesInRank config currentRank dimensions nodeIds = go startX nodeIds
  where
    yPos = negate (fromIntegral currentRank * dagreRankSep config)
    widths = [ fst (Map.findWithDefault (3.2, 1.8) nodeIdValue dimensions) | nodeIdValue <- nodeIds ]
    totalWidth = sum widths + max 0 (fromIntegral (length nodeIds - 1) * dagreNodeSep config)
    startX = negate (totalWidth / 2.0)

    go _ [] = []
    go cursor (currentNodeId : rest) =
      let (nodeWidth, _) = Map.findWithDefault (3.2, 1.8) currentNodeId dimensions
          xPos = cursor + nodeWidth / 2.0
          nextCursor = xPos + nodeWidth / 2.0 + dagreNodeSep config
       in (currentNodeId, xPos, yPos) : go nextCursor rest

-- | Inserts one positioned node after applying direction transform.
insertPlacement :: Direction -> Map NodeId (Double, Double) -> (NodeId, Double, Double) -> Map NodeId (Double, Double)
insertPlacement direction positionMap (currentNodeId, xPos, yPos) =
  Map.insert currentNodeId (transform direction xPos yPos) positionMap
