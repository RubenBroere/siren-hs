module Siren.Layout.Types
  ( Direction(..)
  , LayoutBounds(..)
  , LayoutViewport(..)
  , PositionedNode(..)
  , LaidOutGraph(..)
  )
where

import Siren.Types

-- | The direction in which the graph should flow.
data Direction
  = TopDown    -- ^ Nodes flow from top to bottom (root at top)
  | LeftRight  -- ^ Nodes flow from left to right (root at left)
  deriving (Eq, Show)

-- | Bounding box for a complete laid-out graph.
data LayoutBounds = LayoutBounds
  { boundsMinX :: Double
  , boundsMaxX :: Double
  , boundsMinY :: Double
  , boundsMaxY :: Double
  , boundsWidth :: Double
  , boundsHeight :: Double
  }
  deriving (Eq, Show)

-- | Output-space metadata for rendering engines.
data LayoutViewport = LayoutViewport
  { viewportPadding :: Double
  , viewportUnitsPerLayoutUnit :: Double
  , viewportWidthPx :: Double
  , viewportHeightPx :: Double
  }
  deriving (Eq, Show)

-- | A node that has been assigned a 2D position and dimensions.
data PositionedNode = PositionedNode
  { positionedNode :: Node       -- ^ The original node
  , positionedX :: Double        -- ^ X-coordinate in the layout
  , positionedY :: Double        -- ^ Y-coordinate in the layout
  , positionedRank :: Int        -- ^ Hierarchical layer/rank (0 is root)
  , positionedWidth :: Double    -- ^ Node width in layout units
  , positionedHeight :: Double   -- ^ Node height in layout units
  }
  deriving (Eq, Show)

-- | A graph with all nodes positioned in 2D space.
data LaidOutGraph = LaidOutGraph
  { layoutNodes :: [PositionedNode]       -- ^ Positioned nodes
  , layoutEdges :: [Edge]                 -- ^ Original edges from the graph
  , layoutUniformWidth :: Double          -- ^ Uniform width for rectangular nodes
  , layoutGraphBounds :: LayoutBounds     -- ^ Precomputed graph bounds
  , layoutViewport :: LayoutViewport      -- ^ Precomputed viewport/canvas metrics
  }
  deriving (Eq, Show)
