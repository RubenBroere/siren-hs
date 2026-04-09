module Siren.Layout
  ( -- * Shared Types
    Direction(..)
  , LayoutBounds(..)
  , LayoutViewport(..)
  , PositionedNode(..)
  , LaidOutGraph(..)
    -- * Sugiyama Layout Engine
  , SugiyamaLikeLayout(..)
  , layoutSugiyamaLike
    -- * Dagre-Style Layout Engine
  , DagreLayout(..)
  , defaultDagreLayout
  , layoutDagre
  , layoutDagreWith
    -- * Layout Utilities
  , layoutUniformBoxWidth
  , layoutNodeDimensions
  , layoutBounds
  )
where

import Siren.Layout.Sugiyama
  ( Direction(..)
  , SugiyamaLikeLayout(..)
  , layoutSugiyamaLike
  , layoutNodeDimensions
  )
import Siren.Layout.Dagre
  ( DagreLayout(..)
  , defaultDagreLayout
  , layoutDagre
  , layoutDagreWith
  )
import Siren.Layout.Types
  ( LayoutBounds(..)
  , LayoutViewport(..)
  , LaidOutGraph(..)
  , PositionedNode(..)
  )

-- | Extract the uniform box width from a laid-out graph.
layoutUniformBoxWidth :: LaidOutGraph -> Double
layoutUniformBoxWidth = layoutUniformWidth

-- | Extract the bounding box of a laid-out graph.
layoutBounds :: LaidOutGraph -> (Double, Double, Double, Double)
layoutBounds graph =
  ( boundsMinX bounds
  , boundsMaxX bounds
  , boundsMinY bounds
  , boundsMaxY bounds
  )
  where
    bounds = layoutGraphBounds graph