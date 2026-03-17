{-|
Module      : Siren.Types
Description : Core data types for graph representation
Copyright   : (c) Ruben Broere
License     : See LICENSE file
Maintainer  : ruben.broere@example.com
Stability   : experimental

This module defines the core data types used throughout the Siren library
for representing directed graphs. It provides types for nodes, edges, and
the graph structure itself, along with basic operations for constructing graphs.
-}

module Siren.Types
  ( NodeId(..)
  , NodeShape(..)
  , Node(..)
  , Edge(..)
  , Graph(..)
  , emptyGraph
  , insertNode
  , insertEdge
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A unique identifier for a node in the graph.
-- Wraps a 'String' to provide type safety and prevent confusion with other string values.
newtype NodeId = NodeId { unNodeId :: String }
  deriving (Eq, Ord, Show)

-- | The visual shape to use when rendering a node.
data NodeShape
  = Rectangle         -- ^ Standard rectangular node
  | RoundedRectangle  -- ^ Rectangle with rounded corners
  | Diamond           -- ^ Diamond-shaped node, typically used for decision points
  deriving (Eq, Show)

-- | Represents a node in the graph.
data Node = Node
  { nodeId :: NodeId
  , nodeLabel :: String
  , nodeShape :: NodeShape
  }
  deriving (Eq, Show)

-- | Represents a directed edge connecting two nodes.
data Edge = Edge
  { edgeFrom :: NodeId
  , edgeTo :: NodeId
  , edgeLabel :: Maybe String
  }
  deriving (Eq, Show)

-- | A directed graph consisting of nodes and edges.
-- Uses a 'Map' for efficient node lookup by identifier.
data Graph = Graph
  { graphNodes :: Map NodeId Node
  , graphEdges :: [Edge]
  }
  deriving (Eq, Show)

-- | Creates an empty graph with no nodes or edges.
emptyGraph :: Graph
emptyGraph = Graph Map.empty []

-- | Inserts a node into the graph. If a node with the same ID already exists,
-- it will be replaced.
insertNode :: Node -> Graph -> Graph
insertNode nodeValue graph =
  graph { graphNodes = Map.insert (nodeId nodeValue) nodeValue (graphNodes graph) }

-- | Inserts an edge into the graph. The edge is prepended to the edge list,
-- so more recently added edges will appear first when iterating.
insertEdge :: Edge -> Graph -> Graph
insertEdge edgeValue graph =
  graph { graphEdges = edgeValue : graphEdges graph }