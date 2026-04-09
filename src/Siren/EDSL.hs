module Siren.EDSL
  ( GraphBuilder
  , node
  , nodes
  , roundedNode
  , roundedNodes
  , diamondNode
  , diamondNodes
  , edge
  , edges
  , edgeL
  , edgeWithLabel
  , labeledEdges
  , chain
  , buildGraph
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Siren.Types

-- | A builder type for constructing graphs in a composable way.
-- Internally represents a function that transforms a graph.
--
-- Use '<>' to combine multiple graph builder operations, and 'buildGraph'
-- to validate and finalize the graph construction.
newtype GraphBuilder = GraphBuilder (Graph -> Graph)

-- | Compose two graph builders sequentially.
-- The right builder's modifications are applied after the left builder's.
instance Semigroup GraphBuilder where
  GraphBuilder left <> GraphBuilder right = GraphBuilder (right . left)

-- | The identity graph builder that makes no modifications.
instance Monoid GraphBuilder where
  mempty = GraphBuilder id

-- | Creates a rectangular node.
node :: String -> String -> GraphBuilder
node nodeName labelText = GraphBuilder $ insertNode Node
  { nodeId = NodeId nodeName
  , nodeLabel = labelText
  , nodeShape = Rectangle
  }

-- | Creates many rectangular nodes from (id, label) pairs.
nodes :: [(String, String)] -> GraphBuilder
nodes = mconcat . fmap (uncurry node)

-- | Creates a rounded rectangle node.
roundedNode :: String -> String -> GraphBuilder
roundedNode nodeName labelText = GraphBuilder $ insertNode Node
  { nodeId = NodeId nodeName
  , nodeLabel = labelText
  , nodeShape = RoundedRectangle
  }

-- | Creates many rounded rectangle nodes from (id, label) pairs.
roundedNodes :: [(String, String)] -> GraphBuilder
roundedNodes = mconcat . fmap (uncurry roundedNode)

-- | Creates a diamond-shaped node.
diamondNode :: String -> String -> GraphBuilder
diamondNode nodeName labelText = GraphBuilder $ insertNode Node
  { nodeId = NodeId nodeName
  , nodeLabel = labelText
  , nodeShape = Diamond
  }

-- | Creates many diamond nodes from (id, label) pairs.
diamondNodes :: [(String, String)] -> GraphBuilder
diamondNodes = mconcat . fmap (uncurry diamondNode)

-- | Creates an unlabeled directed edge between two nodes.
edge :: String -> String -> GraphBuilder
edge fromNode toNode = edgeWithLabel fromNode toNode Nothing

-- | Creates many unlabeled directed edges from (from, to) pairs.
edges :: [(String, String)] -> GraphBuilder
edges = mconcat . fmap (uncurry edge)

-- | Creates a directed edge with a required label.
edgeL :: String -> String -> String -> GraphBuilder
edgeL fromNode toNode labelText = edgeWithLabel fromNode toNode (Just labelText)

-- | Creates a directed edge with an optional label.
edgeWithLabel :: String -> String -> Maybe String -> GraphBuilder
edgeWithLabel fromNode toNode maybeLabel = GraphBuilder $ insertEdge Edge
  { edgeFrom = NodeId fromNode
  , edgeTo = NodeId toNode
  , edgeLabel = maybeLabel
  }

-- | Creates many labeled directed edges from (from, to, label) triples.
labeledEdges :: [(String, String, String)] -> GraphBuilder
labeledEdges = mconcat . fmap (\(fromNode, toNode, labelText) -> edgeL fromNode toNode labelText)

-- | Creates an unlabeled path through a sequence of node ids.
--
-- @
-- chain ["a", "b", "c"] == edge "a" "b" <> edge "b" "c"
-- @
chain :: [String] -> GraphBuilder
chain nodeNames = edges (zip nodeNames (drop 1 nodeNames))

-- | Validates and builds the final graph from a 'GraphBuilder'.
buildGraph :: GraphBuilder -> Either String Graph
buildGraph (GraphBuilder build) = validateGraph (build emptyGraph)

validateGraph :: Graph -> Either String Graph
validateGraph graph =
  case collectMissingReferences (graphNodes graph) (graphEdges graph) of
    [] -> Right graph
    firstMissing : _ -> Left firstMissing

-- | Internal function to collect all edges that reference non-existent nodes.
collectMissingReferences :: Map NodeId Node -> [Edge] -> [String]
collectMissingReferences nodeMap = foldl' collectEdgeError []
  where
    collectEdgeError acc currentEdge =
      case missingReferenceMessage nodeMap currentEdge of
        Nothing -> acc
        Just message -> message : acc

missingReferenceMessage :: Map NodeId Node -> Edge -> Maybe String
missingReferenceMessage nodeMap currentEdge =
  case (fromMissing, toMissing) of
    (False, False) -> Nothing
    (True, False) -> Just ("edge references unknown source node: " <> fromName)
    (False, True) -> Just ("edge references unknown target node: " <> toName)
    (True, True) -> Just ("edge references unknown nodes: " <> fromName <> ", " <> toName)
  where
    fromMissing = Map.notMember (edgeFrom currentEdge) nodeMap
    toMissing = Map.notMember (edgeTo currentEdge) nodeMap
    fromName = unNodeId (edgeFrom currentEdge)
    toName = unNodeId (edgeTo currentEdge)