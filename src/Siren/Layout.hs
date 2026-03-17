{-
Sugiyama, K., Tagawa, S., & Toda, M. (1981). "Methods for Visual Understanding
of Hierarchical System Structures." /IEEE Transactions on Systems, Man, and Cybernetics/,
SMC-11(2), 109-125. <https://doi.org/10.1109/TSMC.1981.4308636>

"Layered graph drawing" on Wikipedia:
<https://en.wikipedia.org/wiki/Layered_graph_drawing>

Battista, G. D., Eades, P., Tamassia, R., & Tollis, I. G. (1998).
/Graph Drawing: Algorithms for the Visualization of Graphs/.
Prentice Hall. Chapter 9: Layered Drawings of Digraphs.
-}

module Siren.Layout
  ( Direction(..)
  , PositionedNode(..)
  , LaidOutGraph(..)
  , layoutSugiyamaLike
  , layoutUniformBoxWidth
  , layoutNodeDimensions
  , layoutBounds
  )
where

import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import Siren.Types

-- | The direction in which the graph should flow.
data Direction
  = TopDown    -- ^ Nodes flow from top to bottom (root at top)
  | LeftRight  -- ^ Nodes flow from left to right (root at left)
  deriving (Eq, Show)

-- | A node that has been assigned a 2D position and rank.
data PositionedNode = PositionedNode
  { positionedNode :: Node   -- ^ The original node
  , positionedX :: Double    -- ^ X-coordinate in the layout
  , positionedY :: Double    -- ^ Y-coordinate in the layout
  , positionedRank :: Int    -- ^ Hierarchical layer/rank (0 is the root layer)
  }
  deriving (Eq, Show)

-- | A graph with all nodes positioned in 2D space.
data LaidOutGraph = LaidOutGraph
  { layoutNodes :: [PositionedNode]  -- ^ Positioned nodes
  , layoutEdges :: [Edge]            -- ^ Original edges from the graph
  }
  deriving (Eq, Show)

{-|
Performs a simplified Sugiyama-style layered layout on a directed graph.

The Sugiyama framework is a widely-used method for drawing directed graphs
in a hierarchical manner. The classic algorithm consists of four phases:

1. __Cycle Removal__: Make the graph acyclic (not implemented - assumes DAG)
2. __Layer Assignment__: Assign nodes to horizontal layers (see 'computeRanks')
3. __Crossing Reduction__: Minimize edge crossings (simplified - uses node ID ordering)
4. __Coordinate Assignment__: Assign actual X,Y positions (see 'assignCoordinates')

This implementation is a simplified version suitable for most directed acyclic graphs.
It assigns ranks based on the longest path from source nodes and positions nodes
to minimize horizontal spread within each layer.

__Algorithm Steps:__

[@Step 1: Rank Assignment@]
  Each node is assigned a /rank/ (layer) based on its dependencies.
  A node's rank is defined as the maximum rank of its predecessors plus one.
  Root nodes (those with no incoming edges) are assigned rank 0.
  This is computed iteratively until convergence (see 'computeRanks').

[@Step 2: Node Sorting@]
  Nodes are sorted first by rank, then by node ID to ensure deterministic
  positioning and reduce edge crossings.

[@Step 3: Coordinate Assignment@]
  For each rank, nodes are positioned horizontally with equal spacing,
  centered around the origin. Vertical spacing separates the ranks.
  The 'Direction' parameter determines whether the layout flows top-to-bottom
  or left-to-right (see 'assignCoordinates').

__Parameters:__

[@direction@] Whether the graph should flow top-down or left-right

[@graph@] The input graph to layout

__Returns:__

A 'LaidOutGraph' with all nodes positioned in 2D space.

__Examples:__

@
layout = layoutSugiyamaLike TopDown myGraph
sortedNodes = sortOn positionedRank (layoutNodes layout)
@
-}
layoutSugiyamaLike :: Direction -> Graph -> LaidOutGraph
layoutSugiyamaLike direction graph =
  LaidOutGraph
    { layoutNodes = positionedNodes
    , layoutEdges = graphEdges graph
    }
  where
    nodeList = Map.elems (graphNodes graph)
    rankMap = computeRanks nodeList (graphEdges graph)
    rankedNodes = sortOn (nodeSortKey rankMap) nodeList
    positionMap = assignCoordinates direction rankMap rankedNodes
    positionedNodes = fmap (mkPositionedNode rankMap positionMap) rankedNodes

nodeSortKey :: Map NodeId Int -> Node -> (Int, String)
nodeSortKey rankMap currentNode =
  ( Map.findWithDefault 0 (nodeId currentNode) rankMap
  , unNodeId (nodeId currentNode)
  )

mkPositionedNode :: Map NodeId Int -> Map NodeId (Double, Double) -> Node -> PositionedNode
mkPositionedNode rankMap positionMap currentNode =
  PositionedNode
    { positionedNode = currentNode
    , positionedX = xPos
    , positionedY = yPos
    , positionedRank = nodeRank
    }
  where
    (xPos, yPos) = Map.findWithDefault (0.0, 0.0) (nodeId currentNode) positionMap
    nodeRank = Map.findWithDefault 0 (nodeId currentNode) rankMap

{-|
Computes the rank (layer) for each node in the graph using an iterative approach.

__Algorithm__: This implements a /balanced layer assignment/ method that:

1. First computes minimum feasible ranks (longest path from source)
2. Then applies balancing to reduce overall height while respecting constraints

This creates more compact layouts where nodes at similar depths can occupy the same rank,
while still maintaining a clear hierarchical structure.

Formally, for each node v:

\[rank(v) = \max_{(u,v) \in E} (rank(u) + 1)\]

where E is the set of edges, but the algorithm is relaxed to allow some flexibility
in final rank assignment to minimize total height.

__Process__:

1. Initialize all nodes to rank 0
2. For each edge (u -> v), ensure that minimumRank(v) >= minimumRank(u) + 1
3. Repeat until fixed point (minimum rank computation)
4. Apply balancing: try to reduce node ranks where possible without violating constraints
5. The iteration limit (2n) prevents infinite loops in cyclic graphs

__Complexity__: O(n · m) where n is the number of nodes and m is the number of edges.
For a DAG with maximum path length d, typically converges in d iterations.

__Benefits__:

- Produces more compact layouts for graphs with multiple paths
- Nodes that can be at the same level are positioned together
- Better utilization of horizontal space in the layout

__Parameters:__

[@nodes@] All nodes in the graph

[@edges@] All edges in the graph

__Returns:__

A mapping from each node ID to its computed rank (layer number).
-}
computeRanks :: [Node] -> [Edge] -> Map NodeId Int
computeRanks nodes edges =
  balanceRanks minimumRanks edges
  where
    minimumRanks = computeMinimumRanks nodes edges

computeMinimumRanks :: [Node] -> [Edge] -> Map NodeId Int
computeMinimumRanks nodes edges = iterateRanks 0 initialRanks
  where
    initialRanks = Map.fromList (fmap (\currentNode -> (nodeId currentNode, 0)) nodes)
    maxIterations = max 1 (length nodes * 2)

    iterateRanks iteration ranks
      | iteration >= maxIterations = ranks
      | updated == ranks = ranks
      | otherwise = iterateRanks (iteration + 1) updated
      where
        updated = foldl updateRank ranks edges

updateRank :: Map NodeId Int -> Edge -> Map NodeId Int
updateRank ranks edgeValue =
  case (Map.lookup (edgeFrom edgeValue) ranks, Map.lookup (edgeTo edgeValue) ranks) of
    (Just fromRank, Just toRank)
      | toRank < fromRank + 1 -> Map.insert (edgeTo edgeValue) (fromRank + 1) ranks
      | otherwise -> ranks
    _ -> ranks

balanceRanks :: Map NodeId Int -> [Edge] -> Map NodeId Int
balanceRanks minimumRanks edges = foldl (tryMoveNodeUp edges) minimumRanks orderedNodes
  where
    orderedNodes = sortOn (Down . (minimumRanks Map.!)) (Map.keys minimumRanks)

tryMoveNodeUp :: [Edge] -> Map NodeId Int -> NodeId -> Map NodeId Int
tryMoveNodeUp edges ranks nodeIdValue =
  case Map.lookup nodeIdValue ranks of
    Nothing -> ranks
    Just currentRank
      | currentRank <= 0 -> ranks
      | not canMoveUp -> ranks
      | otherwise -> tryMoveNodeUp edges (Map.insert nodeIdValue candidateRank ranks) nodeIdValue
      where
        candidateRank = currentRank - 1
        canMoveUp = all (preservesIncomingConstraint ranks candidateRank nodeIdValue) edges

preservesIncomingConstraint :: Map NodeId Int -> Int -> NodeId -> Edge -> Bool
preservesIncomingConstraint ranks candidateRank nodeIdValue edgeValue
  | edgeTo edgeValue /= nodeIdValue = True
  | otherwise =
      case Map.lookup (edgeFrom edgeValue) ranks of
        Just sourceRank -> candidateRank > sourceRank
        Nothing -> True

{-|
Assigns 2D coordinates to all nodes based on their ranks.

__Algorithm__: For each rank (layer), nodes are positioned:

1. Horizontally: Evenly spaced, centered around x=0
2. Vertically: At y = -(rank × ySpacing), so higher ranks appear lower
3. The 'Direction' parameter transforms coordinates for left-right flow

__Spacing Constants__:

* @xSpacing = 5.5@: Horizontal distance between adjacent nodes in the same rank
* @ySpacing = 4.0@: Vertical distance between ranks

These values are chosen to provide comfortable spacing for typical node sizes.

__Centering__: Nodes in each rank are centered by computing:

\[x_{offset} = -\frac{(n-1) \times spacing}{2}\]

where n is the number of nodes in the rank. Node i is then placed at:

\[x_i = x_{offset} + i \times spacing\]

__Parameters:__

[@direction@] Flow direction (affects final coordinate transformation)

[@rankMap@] Mapping from node IDs to their ranks

[@nodes@] All nodes to position

__Returns:__

A mapping from each node ID to its (x, y) coordinate pair.
-}
assignCoordinates :: Direction -> Map NodeId Int -> [Node] -> Map NodeId (Double, Double)
assignCoordinates direction rankMap nodes = foldl assignRank Map.empty ranks
  where
    xSpacing = 5.5
    ySpacing = 4.0
    ranks = sortOn id . nub $ fmap (\nodeValue -> Map.findWithDefault 0 (nodeId nodeValue) rankMap) nodes
    assignRank positionMap currentRank =
      let nodesInRank = nodesForRank rankMap nodes currentRank
          placements = placeNodesInRank xSpacing ySpacing currentRank nodesInRank
       in foldl (insertPlacement direction) positionMap placements

nodesForRank :: Map NodeId Int -> [Node] -> Int -> [Node]
nodesForRank rankMap nodes currentRank =
  sortOn (unNodeId . nodeId) (filter isInRank nodes)
  where
    isInRank nodeValue = Map.findWithDefault 0 (nodeId nodeValue) rankMap == currentRank

placeNodesInRank :: Double -> Double -> Int -> [Node] -> [(Node, Double, Double)]
placeNodesInRank xSpacing ySpacing currentRank nodesInRank =
  zipWith (toPlacement startOffset) [0 :: Int ..] nodesInRank
  where
    startOffset = negate (fromIntegral (length nodesInRank - 1) * xSpacing / 2.0)
    yPos = negate (fromIntegral currentRank * ySpacing)

    toPlacement offset index currentNode =
      let xPos = offset + fromIntegral index * xSpacing
       in (currentNode, xPos, yPos)

insertPlacement :: Direction -> Map NodeId (Double, Double) -> (Node, Double, Double) -> Map NodeId (Double, Double)
insertPlacement direction positionMap (currentNode, xPos, yPos) =
  Map.insert (nodeId currentNode) (transform direction xPos yPos) positionMap

{-|
Transforms coordinates based on the layout direction.

* 'TopDown': No transformation - x is horizontal, y is vertical (increasing downward)
* 'LeftRight': Rotates 90 degrees clockwise - x becomes vertical (upward), y becomes horizontal (rightward)

__Parameters:__

[@direction@] The desired flow direction

[@xPos@] The horizontal position in the default orientation

[@yPos@] The vertical position in the default orientation

__Returns:__

The transformed (x, y) coordinate pair.
-}
transform :: Direction -> Double -> Double -> (Double, Double)
transform direction xPos yPos =
  case direction of
    TopDown -> (xPos, yPos)
    LeftRight -> (negate yPos, xPos)

layoutNodeMinWidth :: Double
layoutNodeMinWidth = 3.2

layoutNodeWidthPerChar :: Double
layoutNodeWidthPerChar = 0.26

layoutNodeHorizontalPadding :: Double
layoutNodeHorizontalPadding = 1.8

layoutNodeHeight :: Double
layoutNodeHeight = 1.8

layoutNodeWidthForLabel :: String -> Double
layoutNodeWidthForLabel labelText =
  max layoutNodeMinWidth (fromIntegral (length labelText) * layoutNodeWidthPerChar + layoutNodeHorizontalPadding)

layoutUniformBoxWidth :: LaidOutGraph -> Double
layoutUniformBoxWidth graph =
  case nonDiamondWidths of
    [] -> layoutNodeMinWidth
    widths -> maximum widths
  where
    nonDiamondWidths =
      [ layoutNodeWidthForLabel (nodeLabel (positionedNode positionedNodeValue))
      | positionedNodeValue <- layoutNodes graph
      , nodeShape (positionedNode positionedNodeValue) /= Diamond
      ]

layoutNodeDimensions :: Double -> Node -> (Double, Double)
layoutNodeDimensions uniformBoxWidth nodeValue =
  case nodeShape nodeValue of
    Diamond -> (layoutNodeWidthForLabel (nodeLabel nodeValue), layoutNodeHeight)
    Rectangle -> (uniformBoxWidth, layoutNodeHeight)
    RoundedRectangle -> (uniformBoxWidth, layoutNodeHeight)

layoutBounds :: LaidOutGraph -> (Double, Double, Double, Double)
layoutBounds graph =
  case layoutNodes graph of
    [] -> (-1.0, 1.0, -1.0, 1.0)
    firstNode : restNodes ->
      let firstBounds = nodeBounds uniformBoxWidth firstNode
       in foldl (updateBounds uniformBoxWidth) firstBounds restNodes
  where
    uniformBoxWidth = layoutUniformBoxWidth graph

nodeBounds :: Double -> PositionedNode -> (Double, Double, Double, Double)
nodeBounds uniformBoxWidth positionedNodeValue =
  ( centerX - halfWidth
  , centerX + halfWidth
  , centerY - halfHeight
  , centerY + halfHeight
  )
  where
    currentNode = positionedNode positionedNodeValue
    (nodeWidth, nodeHeightValue) = layoutNodeDimensions uniformBoxWidth currentNode
    halfWidth = nodeWidth / 2.0
    halfHeight = nodeHeightValue / 2.0
    centerX = positionedX positionedNodeValue
    centerY = positionedY positionedNodeValue

updateBounds :: Double -> (Double, Double, Double, Double) -> PositionedNode -> (Double, Double, Double, Double)
updateBounds uniformBoxWidth (minX, maxX, minY, maxY) positionedNodeValue =
  ( min minX leftEdge
  , max maxX rightEdge
  , min minY bottomEdge
  , max maxY topEdge
  )
  where
    (leftEdge, rightEdge, bottomEdge, topEdge) = nodeBounds uniformBoxWidth positionedNodeValue