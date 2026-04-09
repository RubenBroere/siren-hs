{-# LANGUAGE MultiParamTypeClasses #-}

module Siren.Render.SVG
  ( SvgRenderer(..)
  , renderGraphToFile
  )
where

import Control.Monad.Reader (Reader, asks, runReader)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Diagrams.Backend.SVG (B, renderSVG)
import Diagrams.Prelude
import Siren.Engine (RenderEngine(..))
import Siren.Layout.Types
import Siren.Types

data SvgRenderer = SvgRenderer

instance RenderEngine SvgRenderer LaidOutGraph where
  renderLayout _ = renderGraphToFile

data NodeGeometry = NodeGeometry
  { geometryNode :: Node
  , geometryCenter :: P2 Double
  , geometryWidth :: Double
  , geometryHeight :: Double
  }

nodeLineWidth :: Double
nodeLineWidth = 0.05

nodeTextSize :: Double
nodeTextSize = 0.38

diamondSideScale :: Double
diamondSideScale = 1.12

roundedNodeCornerRadius :: Double
roundedNodeCornerRadius = 0.35

arrowHeadLength :: Double
arrowHeadLength = 0.38

arrowShaftWidth :: Double
arrowShaftWidth = 0.04

sourcePadding :: Double
sourcePadding = 0.0

targetPadding :: Double
targetPadding = 0.0

edgeLabelFontSize :: Double
edgeLabelFontSize = 0.32

edgeLabelOffset :: Double
edgeLabelOffset = 0.45

edgeLabelHorizontalPadding :: Double
edgeLabelHorizontalPadding = 0.45

edgeLabelHeight :: Double
edgeLabelHeight = 0.62

edgeLabelCornerRadius :: Double
edgeLabelCornerRadius = 0.12

edgeLabelBorderWidth :: Double
edgeLabelBorderWidth = 0.02

epsilon :: Double
epsilon = 1.0e-9

-- | Renders a laid-out graph to an SVG file.
-- The output SVG dimensions are derived from the graph's layout bounds.
renderGraphToFile :: FilePath -> LaidOutGraph -> IO ()
renderGraphToFile outputPath graph =
  renderSVG outputPath (dims2D (viewportWidthPx viewport) (viewportHeightPx viewport)) (drawGraph graph)
  where
    viewport = layoutViewport graph

data RenderContext = RenderContext
  { contextNodeGeometries :: Map NodeId NodeGeometry
  , contextEdges :: [Edge]
  }

-- | Converts a laid-out graph into a Diagrams diagram.
-- Draws edge shafts first, then nodes, then edge labels on top for readability.
drawGraph :: LaidOutGraph -> QDiagram B V2 Double Any
drawGraph graph = runReader renderLayers context
  where
    nodeGeometries = buildNodeGeometries (layoutNodes graph)
    context = RenderContext nodeGeometries (layoutEdges graph)

    renderLayers :: Reader RenderContext (QDiagram B V2 Double Any)
    renderLayers = do
      nodeGeometryMap <- asks contextNodeGeometries
      edges <- asks contextEdges
      pure (drawEdgeLabels nodeGeometryMap edges <> drawEdges nodeGeometryMap edges <> drawNodes nodeGeometryMap)

buildNodeGeometries :: [PositionedNode] -> Map NodeId NodeGeometry
buildNodeGeometries positionedNodes =
  Map.fromList (fmap toNodeGeometry positionedNodes)

toNodeGeometry :: PositionedNode -> (NodeId, NodeGeometry)
toNodeGeometry positioned =
  ( nodeId currentNode
  , NodeGeometry
      { geometryNode = currentNode
      , geometryCenter = p2 (positionedX positioned, positionedY positioned)
      , geometryWidth = positionedWidth positioned
      , geometryHeight = positionedHeight positioned
      }
  )
  where
    currentNode = positionedNode positioned

-- | Draws all nodes as diagrams and combines them.
drawNodes :: Map NodeId NodeGeometry -> QDiagram B V2 Double Any
drawNodes = mconcat . fmap drawNode . Map.elems

-- | Draws a single node with its label.
drawNode :: NodeGeometry -> QDiagram B V2 Double Any
drawNode geometry = (labelDiagram <> styledShape) # translate (r2 (centerCoordX, centerCoordY))
  where
    currentNode = geometryNode geometry
    nodeBoxWidth = geometryWidth geometry
    nodeBoxHeight = geometryHeight geometry
    (centerCoordX, centerCoordY) = unp2 (geometryCenter geometry)
    styledShape = nodeBaseShape currentNode nodeBoxWidth nodeBoxHeight # fc white # lc black # lwG nodeLineWidth
    labelDiagram = text (nodeLabel currentNode) # fontSizeL nodeTextSize # fc black # centerXY

nodeBaseShape :: Node -> Double -> Double -> QDiagram B V2 Double Any
nodeBaseShape currentNode nodeBoxWidth nodeBoxHeight =
  case nodeShape currentNode of
    Rectangle -> rect nodeBoxWidth nodeBoxHeight
    RoundedRectangle -> roundedRect nodeBoxWidth nodeBoxHeight roundedNodeCornerRadius
    Diamond -> square (nodeBoxHeight * diamondSideScale) # rotateBy (1 / 8)

-- | Draws all edges as directed arrows between node positions.
drawEdges :: Map NodeId NodeGeometry -> [Edge] -> QDiagram B V2 Double Any
drawEdges nodeGeometries = mconcat . fmap (drawEdge nodeGeometries)

-- | Draws labels for all edges as a separate top layer.
drawEdgeLabels :: Map NodeId NodeGeometry -> [Edge] -> QDiagram B V2 Double Any
drawEdgeLabels nodeGeometries = mconcat . fmap (drawEdgeLabel nodeGeometries)

-- | Draws a single directed edge as an arrow, optionally with a label.
-- If either endpoint node is not found, returns an empty diagram.
drawEdge :: Map NodeId NodeGeometry -> Edge -> QDiagram B V2 Double Any
drawEdge nodeGeometries currentEdge =
  case lookupEdgeGeometries nodeGeometries currentEdge of
    Just (sourceGeometry, targetGeometry) ->
      arrowBetween' arrowStyle sourcePoint targetPoint
      where
        (sourcePoint, targetPoint) = clippedEdgeEndpoints sourceGeometry targetGeometry
        arrowStyle = with & headLength .~ global arrowHeadLength & shaftStyle %~ lwG arrowShaftWidth
    Nothing -> mempty

drawEdgeLabel :: Map NodeId NodeGeometry -> Edge -> QDiagram B V2 Double Any
drawEdgeLabel nodeGeometries currentEdge =
  case (lookupEdgeGeometries nodeGeometries currentEdge, edgeLabel currentEdge) of
    (Just (sourceGeometry, targetGeometry), Just labelText) ->
      (labelGlyph <> labelBackground # opacity 0.96) # moveTo labelPoint
      where
        (sourcePoint, targetPoint) = clippedEdgeEndpoints sourceGeometry targetGeometry
        edgeVector = targetPoint .-. sourcePoint
        perpendicularDir = choosePerpendicular edgeVector
        alongEdge = safeNormalized edgeVector
        labelPoint = lerp 0.5 sourcePoint targetPoint .+^ (edgeLabelOffset *^ perpendicularDir) .+^ (0.16 *^ alongEdge)
        labelGlyph = text labelText # fontSizeL edgeLabelFontSize # fc black
        labelBackground =
          roundedRect (edgeLabelWidth labelText) edgeLabelHeight edgeLabelCornerRadius
            # fc white
            # lc black
            # lwG edgeLabelBorderWidth
    _ -> mempty

lookupEdgeGeometries :: Map NodeId NodeGeometry -> Edge -> Maybe (NodeGeometry, NodeGeometry)
lookupEdgeGeometries nodeGeometries currentEdge = do
  sourceGeometry <- Map.lookup (edgeFrom currentEdge) nodeGeometries
  targetGeometry <- Map.lookup (edgeTo currentEdge) nodeGeometries
  pure (sourceGeometry, targetGeometry)

choosePerpendicular :: V2 Double -> V2 Double
choosePerpendicular edgeVector =
  if py < 0
    then negated perpendicular
    else perpendicular
  where
    perpendicular = normalizedPerp edgeVector
    V2 _ py = perpendicular

edgeLabelWidth :: String -> Double
edgeLabelWidth labelText =
  max 1.2 (fromIntegral (length labelText) * 0.26 + edgeLabelHorizontalPadding)

clippedEdgeEndpoints :: NodeGeometry -> NodeGeometry -> (P2 Double, P2 Double)
clippedEdgeEndpoints sourceGeometry targetGeometry = (sourcePoint, targetPoint)
  where
    sourceCenter = geometryCenter sourceGeometry
    targetCenter = geometryCenter targetGeometry
    directionVector = targetCenter .-. sourceCenter
    unitDir = safeNormalized directionVector
    sourceOffset = (boundaryDistance sourceGeometry unitDir + sourcePadding) *^ unitDir
    targetOffset = (boundaryDistance targetGeometry (negated unitDir) + targetPadding) *^ negated unitDir
    sourcePoint = sourceCenter .+^ sourceOffset
    targetPoint = targetCenter .+^ targetOffset

boundaryDistance :: NodeGeometry -> V2 Double -> Double
boundaryDistance geometry unitDirection =
  case nodeShape (geometryNode geometry) of
    Rectangle -> boxDistance
    RoundedRectangle -> boxDistance
    Diamond -> diamondDistance
  where
    V2 dx dy = unitDirection
    halfWidth = geometryWidth geometry / 2.0
    halfHeight = geometryHeight geometry / 2.0
    boxDistance =
      min scaleToVerticalBoundary scaleToHorizontalBoundary
      where
        scaleToVerticalBoundary = if abs dx < epsilon then 1 / epsilon else halfWidth / abs dx
        scaleToHorizontalBoundary = if abs dy < epsilon then 1 / epsilon else halfHeight / abs dy
    diamondDistance =
      let halfDiagonal = (geometryHeight geometry * diamondSideScale) / sqrt 2.0
          denominator = max epsilon (abs dx + abs dy)
       in halfDiagonal / denominator

safeNormalized :: V2 Double -> V2 Double
safeNormalized vector
  | quadrance vector < 1.0e-12 = V2 1 0
  | otherwise = normalize vector

normalizedPerp :: V2 Double -> V2 Double
normalizedPerp vector = safeNormalized (perp vector)