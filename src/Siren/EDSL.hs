module Siren.EDSL
    ( GraphBuilder
    , node
    , roundedNode
    , diamondNode
    , edge
    , edgeWithLabel
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
--
-- __Examples:__
--
-- @
-- node "n1" "My Node"
-- @
node :: String   -- ^ Unique node identifier
     -> String   -- ^ Display label
     -> GraphBuilder
node nodeName labelText = GraphBuilder $ insertNode Node
    { nodeId = NodeId nodeName
    , nodeLabel = labelText
    , nodeShape = Rectangle
    }

-- | Creates a rounded rectangle node.
-- Useful for representing processes or states.
--
-- __Examples:__
--
-- @
-- roundedNode "process1" "Process Data"
-- @
roundedNode :: String   -- ^ Unique node identifier
            -> String   -- ^ Display label
            -> GraphBuilder
roundedNode nodeName labelText = GraphBuilder $ insertNode Node
    { nodeId = NodeId nodeName
    , nodeLabel = labelText
    , nodeShape = RoundedRectangle
    }

-- | Creates a diamond-shaped node.
-- Conventionally used for decision points in flowcharts.
--
-- __Examples:__
--
-- @
-- diamondNode "check" "Is Valid?"
-- @
diamondNode :: String   -- ^ Unique node identifier
            -> String   -- ^ Display label
            -> GraphBuilder
diamondNode nodeName labelText = GraphBuilder $ insertNode Node
    { nodeId = NodeId nodeName
    , nodeLabel = labelText
    , nodeShape = Diamond
    }

-- | Creates an unlabeled directed edge between two nodes.
--
-- __Examples:__
--
-- @
-- edge "node1" "node2"
-- @
edge :: String   -- ^ Source node identifier
     -> String   -- ^ Target node identifier
     -> GraphBuilder
edge fromNode toNode = edgeWithLabel fromNode toNode Nothing

-- | Creates a directed edge with an optional label.
-- If the label is 'Nothing', the edge will be drawn without text.
--
-- __Examples:__
--
-- @
-- edgeWithLabel "decision" "success" (Just "yes")
-- edgeWithLabel "decision" "failure" (Just "no")
-- @
edgeWithLabel :: String         -- ^ Source node identifier
              -> String         -- ^ Target node identifier
              -> Maybe String   -- ^ Optional edge label
              -> GraphBuilder
edgeWithLabel fromNode toNode maybeLabel = GraphBuilder $ insertEdge Edge
    { edgeFrom = NodeId fromNode
    , edgeTo = NodeId toNode
    , edgeLabel = maybeLabel
    }

-- | Validates and builds the final graph from a 'GraphBuilder'.
-- 
-- This function checks that all edges reference existing nodes.
-- If any edge references a non-existent node, it returns a 'Left' value
-- containing an error message. Otherwise, it returns the constructed graph.
--
-- __Examples:__
--
-- @
-- -- Valid graph:
-- buildGraph (node "a" "A" <> node "b" "B" <> edge "a" "b")
-- -- Returns: Right (Graph ...)
--
-- -- Invalid graph (missing node "c"):
-- buildGraph (node "a" "A" <> edge "a" "c")
-- -- Returns: Left "edge references unknown target node: c"
-- @
buildGraph :: GraphBuilder -> Either String Graph
buildGraph (GraphBuilder build) =
    let graph = build emptyGraph
        missingReferences = collectMissingReferences (graphNodes graph) (graphEdges graph)
     in case missingReferences of
            [] -> Right graph
            firstMissing : _ -> Left firstMissing

-- | Internal function to collect all edges that reference non-existent nodes.
-- Returns a list of error messages describing the missing references.
collectMissingReferences :: Map NodeId Node -> [Edge] -> [String]
collectMissingReferences nodes edges =
    foldl' collect [] edges
  where
    collect acc currentEdge =
        let fromMissing = Map.notMember (edgeFrom currentEdge) nodes
            toMissing = Map.notMember (edgeTo currentEdge) nodes
            fromName = unNodeId (edgeFrom currentEdge)
            toName = unNodeId (edgeTo currentEdge)
         in case (fromMissing, toMissing) of
                (False, False) -> acc
                (True, False) -> ("edge references unknown source node: " <> fromName) : acc
                (False, True) -> ("edge references unknown target node: " <> toName) : acc
                (True, True) -> ("edge references unknown nodes: " <> fromName <> ", " <> toName) : acc
