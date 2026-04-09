{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Siren.Engine
  ( LayoutEngine(..)
  , RenderEngine(..)
  , renderGraphWith
  )
where

import Siren.Layout.Types (LaidOutGraph)
import Siren.Types (Graph)

-- | A layout engine turns a graph into a laid-out graph.
class LayoutEngine engine where
  runLayout :: engine -> Graph -> LaidOutGraph

-- | A rendering engine turns a laid-out graph into an output artifact.
class RenderEngine renderer layouted where
  renderLayout :: renderer -> FilePath -> layouted -> IO ()

-- | Compose any layout engine with any compatible rendering engine.
renderGraphWith ::
  ( LayoutEngine engine
  , RenderEngine renderer LaidOutGraph
  ) =>
  engine ->
  renderer ->
  FilePath ->
  Graph ->
  IO ()
renderGraphWith engine renderer outputPath graph =
  renderLayout renderer outputPath (runLayout engine graph)