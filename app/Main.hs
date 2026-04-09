module Main (main) where

import Control.Monad (when)
import Data.Either (isLeft)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)

import Samples
    ( buggedGraph
    , chainGraph
    , crossingBipartiteGraph
    , cycleGraph
    , denseLayeredGraph
    , diamondMergeGraph
    , disconnectedGraph
    , fanInGraph
    , ladderGraph
    , longLabelGraph
    , multiSourceSinkGraph
    , sampleGraph
    )
import Siren
  ( DagreLayout(..)
  , Direction(TopDown)
  , Graph
  , SugiyamaLikeLayout(..)
  , renderGraphWith
  )
import Siren.Render.SVG (SvgRenderer(..))

data CliOptions = CliOptions
    { selectedGraphName :: String
    , selectedLayoutName :: String
    }

graphSelectionParser :: Parser CliOptions
graphSelectionParser =
    CliOptions
        <$> strArgument (metavar "GRAPH_NAME")
        <*> strOption
            ( long "layout"
                <> short 'l'
                <> metavar "ENGINE"
                <> value "sugiyama"
                <> help "Layout engine: sugiyama | dagre"
            )

graphSelectionInfo :: ParserInfo CliOptions
graphSelectionInfo =
    info
        (graphSelectionParser <**> helper)
        ( fullDesc
            <> progDesc "Render a named graph as SVG with a chosen layout engine"
            <> header "siren-hs graph renderer"
        )

graphForSelection :: String -> Either String (String, FilePath, Either String Graph)
graphForSelection selection =
    case selection of
        "sampleGraph" -> Right ("sampleGraph", "output/sample.svg", sampleGraph)
        "buggedGraph" -> Right ("buggedGraph", "output/bugged.svg", buggedGraph)
        "chainGraph" -> Right ("chainGraph", "output/chain.svg", chainGraph)
        "diamondMergeGraph" -> Right ("diamondMergeGraph", "output/diamond-merge.svg", diamondMergeGraph)
        "fanInGraph" -> Right ("fanInGraph", "output/fan-in.svg", fanInGraph)
        "crossingBipartiteGraph" -> Right ("crossingBipartiteGraph", "output/crossing-bipartite.svg", crossingBipartiteGraph)
        "longLabelGraph" -> Right ("longLabelGraph", "output/long-label.svg", longLabelGraph)
        "multiSourceSinkGraph" -> Right ("multiSourceSinkGraph", "output/multi-source-sink.svg", multiSourceSinkGraph)
        "cycleGraph" -> Right ("cycleGraph", "output/cycle.svg", cycleGraph)
        "denseLayeredGraph" -> Right ("denseLayeredGraph", "output/dense-layered.svg", denseLayeredGraph)
        "disconnectedGraph" -> Right ("disconnectedGraph", "output/disconnected.svg", disconnectedGraph)
        "ladderGraph" -> Right ("ladderGraph", "output/ladder.svg", ladderGraph)
        _ ->
            Left
                (  "Expected graph name to be one of: "
                <> "sampleGraph, buggedGraph, chainGraph, diamondMergeGraph, fanInGraph, "
                <> "crossingBipartiteGraph, longLabelGraph, multiSourceSinkGraph, cycleGraph, "
                <> "denseLayeredGraph, disconnectedGraph, ladderGraph"
                )

main :: IO ()
main = do
    options <- execParser graphSelectionInfo
    createDirectoryIfMissing True "output"
    result <-
        case graphForSelection (selectedGraphName options) of
            Left err -> pure (Left err)
            Right (selectionName, outputPath, selectedGraph) ->
                case selectedGraph of
                    Left graphErr -> pure (Left graphErr)
                    Right graph -> do
                        renderWithSelection (selectedLayoutName options) outputPath graph
                        pure (Right (selectionName, outputPath))
    case result of
        Left err -> putStrLn ("Failed to render graph: " <> err)
        Right (selectionName, path) -> putStrLn ("Rendered " <> selectionName <> ", Engine: " <> selectedLayoutName options <> ", SVG: " <> path)
    when (isLeft result) (fail "siren-hs render failed")

renderWithSelection :: String -> FilePath -> Graph -> IO ()
renderWithSelection layoutName outputPath graph =
    case layoutName of
        "sugiyama" -> renderGraphWith (SugiyamaLikeLayout TopDown) SvgRenderer outputPath graph
        "dagre" ->
            renderGraphWith
                (DagreLayout {dagreDirection = TopDown, dagreNodeSep = 1.4, dagreRankSep = 3.4})
                SvgRenderer
                outputPath
                graph
        _ -> fail "Expected layout engine: sugiyama or dagre"
