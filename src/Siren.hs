{-| 
Module      : Siren
Description : Main module for the Siren graph library
Copyright   : (c) Ruben Broere
License     : See LICENSE file
Maintainer  : ruben.broere@example.com
Stability   : experimental

Siren is a Haskell library for creating and rendering directed graphs as SVG diagrams.
It provides a declarative EDSL for graph construction and implements a Sugiyama-style
layered layout algorithm for hierarchical visualization.

== Quick Start

@
import Siren

main :: IO ()
main = do
    let graph = buildGraph $
            node "start" "Start" <>
            node "end" "End" <>
            edge "start" "end"
    
    case graph of
        Left err -> putStrLn $ "Error: " ++ err
        Right g -> writeGraphSvg "output.svg" TopDown g
@

This module re-exports the core types, EDSL functions, and layout options,
providing a convenient single import for most use cases.
-}

module Siren
    ( writeGraphSvg
    , module Siren.Types
    , module Siren.EDSL
    , module Siren.Layout
    )
where

import Siren.EDSL
import Siren.Layout
import Siren.Render.SVG (renderGraphToFile)
import Siren.Types

-- | Convenience function to layout and render a graph to an SVG file.
-- Combines 'layoutSugiyamaLike' and 'renderGraphToFile' in one step.
--
-- __Examples:__
--
-- @
-- writeGraphSvg "output.svg" TopDown myGraph
-- writeGraphSvg "flowchart.svg" LeftRight myGraph
-- @
writeGraphSvg :: FilePath   -- ^ Output SVG file path
              -> Direction  -- ^ Layout direction (TopDown or LeftRight)
              -> Graph      -- ^ The graph to render
              -> IO ()
writeGraphSvg outputPath direction graph =
    renderGraphToFile outputPath (layoutSugiyamaLike direction graph)
