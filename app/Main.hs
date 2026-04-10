module Main (main) where

import Control.Monad (when, forM_)
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

data CLIOptions = CLIOptions
  { selectedGraphName :: String
  , selectedLayoutName :: String
  }

graphSelectionParser :: Parser CLIOptions
graphSelectionParser =
  CLIOptions
    <$> strArgument (metavar "GRAPH_NAME")
    <*> strOption
      (  long "layout"
      <> short 'l'
      <> metavar "ENGINE"
      <> value "sugiyama"
      <> help "Layout engine: sugiyama | dagre"
      )

graphSelectionInfo :: ParserInfo CLIOptions
graphSelectionInfo =
  info
    (graphSelectionParser <**> helper)
    (  fullDesc
    <> progDesc "Render a named graph as SVG with a chosen layout engine"
    <> header "siren-hs graph renderer"
    )

graphForSelection :: String -> Either String (FilePath, Either String Graph)
graphForSelection selection =
  case selection of
    "sample"             -> Right ("sample", sampleGraph)
    "bugged"             -> Right ("bugged", buggedGraph)
    "chain"              -> Right ("chain", chainGraph)
    "diamond-merge"      -> Right ("diamond-merge", diamondMergeGraph)
    "fan-in"             -> Right ("fan-in", fanInGraph)
    "crossing-bipartite" -> Right ("crossing-bipartite", crossingBipartiteGraph)
    "long-label"         -> Right ("long-label", longLabelGraph)
    "multi-source-sink"  -> Right ("multi-source-sink", multiSourceSinkGraph)
    "cycle"              -> Right ("cycle", cycleGraph)
    "dense-layered"      -> Right ("dense-layered", denseLayeredGraph)
    "disconnected"       -> Right ("disconnected", disconnectedGraph)
    "ladder"             -> Right ("ladder", ladderGraph)
    _ ->
      Left
        (  "Expected graph name to be one of:\n"
        ++ "sample, bugged, chain, diamond-merge, fan-in,\n"
        ++ "crossing-bipartite, long-label, multi-source-sink, cycle,\n"
        ++ "dense-layered, disconnected, ladder,\n" 
        ++ "or 'all' to render all samples"
        )

formatPath :: FilePath -> String -> FilePath
formatPath outputPath layoutName = "output/" ++ outputPath ++ "-" ++ layoutName ++ ".svg"

renderWithSelection :: String -> FilePath -> Graph -> IO ()
renderWithSelection layoutName outputPath graph =
  case layoutName of
    "sugiyama" -> renderGraphWith (SugiyamaLikeLayout TopDown) SvgRenderer (formatPath outputPath layoutName) graph
    "dagre" ->
      renderGraphWith
        (DagreLayout {dagreDirection = TopDown, dagreNodeSep = 1.4, dagreRankSep = 3.4})
        SvgRenderer
        (formatPath outputPath layoutName)
        graph
    _ -> fail "Expected layout engine: sugiyama or dagre"

renderSingleSample :: CLIOptions -> IO ()
renderSingleSample (CLIOptions graphName layoutName) = do
  result <-
    case graphForSelection graphName of
      Left err -> pure (Left err)
      Right (outputPath, selectedGraph) ->
        case selectedGraph of
          Left graphErr -> pure (Left graphErr)
          Right graph -> do
            renderWithSelection layoutName outputPath graph
            pure (Right outputPath)
            
  case result of
      Left err -> putStrLn ("Failed to render graph: " ++ err)
      Right path -> putStrLn ("Rendered " ++ path ++ ", Engine: " ++ layoutName ++ ", SVG: " ++ formatPath path layoutName)
  when (isLeft result) (fail "siren-hs render failed")

renderAllSamplesWithBothMethods :: IO ()
renderAllSamplesWithBothMethods = do
  let samples =
        [ ("sample", sampleGraph)
        , ("bugged", buggedGraph)
        , ("chain", chainGraph)
        , ("diamond-merge", diamondMergeGraph)
        , ("fan-in", fanInGraph)
        , ("crossing-bipartite", crossingBipartiteGraph)
        , ("long-label", longLabelGraph)
        , ("multi-source-sink", multiSourceSinkGraph)
        , ("cycle", cycleGraph)
        , ("dense-layered", denseLayeredGraph)
        , ("disconnected", disconnectedGraph)
        , ("ladder", ladderGraph)
        ]
      layouts = ["sugiyama", "dagre"]

  forM_ samples $ \(sampleName, selectedGraph) ->
    case selectedGraph of
      Left err -> putStrLn ("Error building " ++ sampleName ++ ": " ++ err)
      Right graph -> do
        forM_ layouts $ \layoutName -> do
          renderWithSelection layoutName sampleName graph
          putStrLn ("Rendered " ++ sampleName ++ " with " ++ layoutName)
  putStrLn ("Successfully rendered " ++ show (length samples * length layouts) ++ " graphs")

main :: IO ()
main = do
  options <- execParser graphSelectionInfo
  createDirectoryIfMissing True "output"

  if selectedGraphName options == "all"
    then do
      renderAllSamplesWithBothMethods
    else do
      renderSingleSample options
