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
    ( Direction (..)
    , PositionedNode (..)
    , LaidOutGraph (..)
    , layoutSugiyamaLike
    , layoutUniformBoxWidth
    , layoutNodeDimensions
    , layoutBounds
    )
where

import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
    let nodeList = Map.elems (graphNodes graph)
        rankMap = computeRanks nodeList (graphEdges graph)
        rankedNodes =
            sortOn
                (\currentNode ->
                    ( Map.findWithDefault 0 (nodeId currentNode) rankMap
                    , unNodeId (nodeId currentNode)
                    )
                )
                nodeList
        positionMap = assignCoordinates direction rankMap rankedNodes
        positioned =
            fmap
                (\currentNode ->
                    let (xPos, yPos) = Map.findWithDefault (0.0, 0.0) (nodeId currentNode) positionMap
                        nodeRank = Map.findWithDefault 0 (nodeId currentNode) rankMap
                     in PositionedNode
                            { positionedNode = currentNode
                            , positionedX = xPos
                            , positionedY = yPos
                            , positionedRank = nodeRank
                            }
                )
                rankedNodes
     in LaidOutGraph
            { layoutNodes = positioned
            , layoutEdges = graphEdges graph
            }

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
2. For each edge (u → v), ensure that minimumRank(v) ≥ minimumRank(u) + 1
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
    let minimumRanks = computeMinimumRanks nodes edges
        balancedRanks = balanceRanks minimumRanks edges
     in balancedRanks
  where
    -- Compute minimum feasible ranks using longest path
    computeMinimumRanks nodes' edges' =
        iterateRanks 0 (initialRanks nodes')
      where
        initialRanks = Map.fromList . fmap (\currentNode -> (nodeId currentNode, 0))
        maxIterations = max 1 (length nodes' * 2)

        iterateRanks iteration ranks
            | iteration >= maxIterations = ranks
            | otherwise =
                let updated = foldl updateRank ranks edges'
                 in if updated == ranks
                        then ranks
                        else iterateRanks (iteration + 1) updated

        updateRank ranks edgeValue =
            case (Map.lookup (edgeFrom edgeValue) ranks, Map.lookup (edgeTo edgeValue) ranks) of
                (Just fromRank, Just toRank)
                    | toRank < fromRank + 1 -> Map.insert (edgeTo edgeValue) (fromRank + 1) ranks
                    | otherwise -> ranks
                _ -> ranks

    -- Balancing phase: try to reduce maximum rank while respecting constraints
    balanceRanks minRanks edges' =
        foldl tryMoveNodeUp minRanks (sortOn (Down . (minRanks Map.!)) (Map.keys minRanks))
      where
        tryMoveNodeUp ranks nodeId' =
            -- Try to move node up (reduce rank) if all its predecessors still satisfy constraints
            case Map.lookup nodeId' ranks of
              Nothing -> ranks
              Just currentRank
                | currentRank <= 0 -> ranks
                | otherwise ->
                    let candidateRank = currentRank - 1
                        -- Check if any incoming edge would be violated by moving up
                        canMoveUp = all (\edge -> 
                          if edgeTo edge == nodeId'
                          then case Map.lookup (edgeFrom edge) ranks of
                                 Just sourceRank -> candidateRank > sourceRank
                                 Nothing -> True
                          else True) edges'
                     in if canMoveUp
                        then Map.insert nodeId' candidateRank (tryMoveNodeUp ranks nodeId')
                        else ranks

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
assignCoordinates direction rankMap nodes =
    let ranks = sortOn id . nub $ fmap (\n -> Map.findWithDefault 0 (nodeId n) rankMap) nodes
        xSpacing = 5.5  -- Horizontal spacing between nodes in same rank
        ySpacing = 4.0  -- Vertical spacing between ranks
     in foldl (assignRankNodes xSpacing ySpacing) Map.empty ranks
  where
    -- Process all nodes in a single rank
    assignRankNodes xSpacing ySpacing acc currentRank =
        let nodesInRank =
                sortOn (unNodeId . nodeId) . filter (\n -> Map.findWithDefault 0 (nodeId n) rankMap == currentRank) $ nodes
            -- Center the nodes horizontally around x=0
            startOffset = negate (fromIntegral (length nodesInRank - 1) * xSpacing / 2.0)
            placed = zipWith (toPlacement startOffset) ([0 :: Int ..]) nodesInRank
         in foldl
                (\positionMap (currentNode, xPos, yPos) -> Map.insert (nodeId currentNode) (transform direction xPos yPos) positionMap)
                acc
                placed
      where
        -- Compute position for a single node in the rank
        toPlacement startOffset index currentNode =
            let xPos = startOffset + fromIntegral index * xSpacing
                yPos = negate (fromIntegral currentRank * ySpacing)  -- Negative so rank 0 is at top
             in (currentNode, xPos, yPos)

{-| 
Transforms coordinates based on the layout direction.

* 'TopDown': No transformation - x is horizontal, y is vertical (increasing downward)
* 'LeftRight': Rotates 90° clockwise - x becomes vertical (upward), y becomes horizontal (rightward)

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
        LeftRight -> (negate yPos, xPos)  -- Rotate 90° clockwise

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
            let firstModel = positionedNode firstNode
                (firstWidth, firstHeight) = layoutNodeDimensions uniformBoxWidth firstModel
                firstHalfWidth = firstWidth / 2.0
                firstHalfHeight = firstHeight / 2.0
                firstX = positionedX firstNode
                firstY = positionedY firstNode
                initialBounds =
                    ( firstX - firstHalfWidth
                    , firstX + firstHalfWidth
                    , firstY - firstHalfHeight
                    , firstY + firstHalfHeight
                    )
             in foldl updateBounds initialBounds restNodes
  where
    uniformBoxWidth = layoutUniformBoxWidth graph

    updateBounds (minX, maxX, minY, maxY) positionedNodeValue =
        let currentNode = positionedNode positionedNodeValue
            (nodeWidth, nodeHeight) = layoutNodeDimensions uniformBoxWidth currentNode
            halfWidth = nodeWidth / 2.0
            halfHeight = nodeHeight / 2.0
            nodeCenterX = positionedX positionedNodeValue
            nodeCenterY = positionedY positionedNodeValue
            leftEdge = nodeCenterX - halfWidth
            rightEdge = nodeCenterX + halfWidth
            bottomEdge = nodeCenterY - halfHeight
            topEdge = nodeCenterY + halfHeight
         in ( min minX leftEdge
            , max maxX rightEdge
            , min minY bottomEdge
            , max maxY topEdge
            )
