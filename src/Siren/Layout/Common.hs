module Siren.Layout.Common
  ( withComputedMetrics
  , transform
  , layoutNodeDimensions
  , layoutUniformWidthFromNodes
  , layoutUniformWidthFromPositionedNodes
  , boundsFromNodes
  , viewportFromBounds
  )
where

import Siren.Layout.Types
import Siren.Types

withComputedMetrics :: [Edge] -> [PositionedNode] -> LaidOutGraph
withComputedMetrics edges positionedNodes =
  LaidOutGraph
    { layoutNodes = positionedNodes
    , layoutEdges = edges
    , layoutUniformWidth = uniformWidth
    , layoutGraphBounds = bounds
    , layoutViewport = viewport
    }
  where
    uniformWidth = layoutUniformWidthFromPositionedNodes positionedNodes
    bounds = boundsFromNodes positionedNodes
    viewport = viewportFromBounds bounds

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

layoutUniformWidthFromNodes :: [Node] -> Double
layoutUniformWidthFromNodes nodes =
  case nonDiamondWidths of
    [] -> layoutNodeMinWidth
    widths -> maximum widths
  where
    nonDiamondWidths =
      [ layoutNodeWidthForLabel (nodeLabel currentNode)
      | currentNode <- nodes
      , nodeShape currentNode /= Diamond
      ]

layoutUniformWidthFromPositionedNodes :: [PositionedNode] -> Double
layoutUniformWidthFromPositionedNodes positionedNodes =
  case nonDiamondWidths of
    [] -> layoutNodeMinWidth
    widths -> maximum widths
  where
    nonDiamondWidths =
      [ positionedWidth positionedNodeValue
      | positionedNodeValue <- positionedNodes
      , nodeShape (positionedNode positionedNodeValue) /= Diamond
      ]

boundsFromNodes :: [PositionedNode] -> LayoutBounds
boundsFromNodes positionedNodes =
  case positionedNodes of
    [] ->
      LayoutBounds
        { boundsMinX = -1.0
        , boundsMaxX = 1.0
        , boundsMinY = -1.0
        , boundsMaxY = 1.0
        , boundsWidth = 2.0
        , boundsHeight = 2.0
        }
    firstNode : restNodes ->
      let firstBounds = nodeBounds firstNode
          (minX, maxX, minY, maxY) = foldl updateBounds firstBounds restNodes
       in mkLayoutBounds minX maxX minY maxY

mkLayoutBounds :: Double -> Double -> Double -> Double -> LayoutBounds
mkLayoutBounds minX maxX minY maxY =
  LayoutBounds
    { boundsMinX = minX
    , boundsMaxX = maxX
    , boundsMinY = minY
    , boundsMaxY = maxY
    , boundsWidth = maxX - minX
    , boundsHeight = maxY - minY
    }

viewportFromBounds :: LayoutBounds -> LayoutViewport
viewportFromBounds bounds =
  LayoutViewport
    { viewportPadding = canvasPadding
    , viewportUnitsPerLayoutUnit = unitsPerLayoutUnit
    , viewportWidthPx = canvasWidthUnits * unitsPerLayoutUnit
    , viewportHeightPx = canvasHeightUnits * unitsPerLayoutUnit
    }
  where
    canvasPadding = 1.4
    unitsPerLayoutUnit = 120.0
    canvasWidthUnits = max 1.0 (boundsWidth bounds + 2 * canvasPadding)
    canvasHeightUnits = max 1.0 (boundsHeight bounds + 2 * canvasPadding)

layoutNodeDimensions :: Double -> Node -> (Double, Double)
layoutNodeDimensions uniformBoxWidth nodeValue =
  case nodeShape nodeValue of
    Diamond -> (layoutNodeWidthForLabel (nodeLabel nodeValue), layoutNodeHeight)
    Rectangle -> (uniformBoxWidth, layoutNodeHeight)
    RoundedRectangle -> (uniformBoxWidth, layoutNodeHeight)

nodeBounds :: PositionedNode -> (Double, Double, Double, Double)
nodeBounds positionedNodeValue =
  ( centerX - halfWidth
  , centerX + halfWidth
  , centerY - halfHeight
  , centerY + halfHeight
  )
  where
    halfWidth = positionedWidth positionedNodeValue / 2.0
    halfHeight = positionedHeight positionedNodeValue / 2.0
    centerX = positionedX positionedNodeValue
    centerY = positionedY positionedNodeValue

updateBounds :: (Double, Double, Double, Double) -> PositionedNode -> (Double, Double, Double, Double)
updateBounds (minX, maxX, minY, maxY) positionedNodeValue =
  ( min minX leftEdge
  , max maxX rightEdge
  , min minY bottomEdge
  , max maxY topEdge
  )
  where
    (leftEdge, rightEdge, bottomEdge, topEdge) = nodeBounds positionedNodeValue
