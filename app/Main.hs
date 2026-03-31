module Main (main) where

import Control.Monad (when)
import Data.Either (isLeft)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)

import Samples (buggedGraph, sampleGraph)
import Siren (Direction (TopDown), Graph, writeGraphSvg)

graphSelectionParser :: Parser String
graphSelectionParser = strArgument (metavar "GRAPH_NAME")

graphSelectionInfo :: ParserInfo String
graphSelectionInfo =
    info
        (graphSelectionParser <**> helper)
        ( fullDesc
            <> progDesc "Render a named graph as SVG"
            <> header "siren-hs graph renderer"
        )

graphForSelection :: String -> Either String (String, FilePath, Either String Graph)
graphForSelection selection =
    case selection of
        "sampleGraph" -> Right ("sampleGraph", "output/sample.svg", sampleGraph)
        "buggedGraph" -> Right ("buggedGraph", "output/bugged.svg", buggedGraph)
        _ -> Left "Expected one of: sampleGraph, buggedGraph"

main :: IO ()
main = do
    selection <- execParser graphSelectionInfo
    createDirectoryIfMissing True "output"
    result <-
        case graphForSelection selection of
            Left err -> pure (Left err)
            Right (selectionName, outputPath, selectedGraph) ->
                case selectedGraph of
                    Left graphErr -> pure (Left graphErr)
                    Right graph -> do
                        writeGraphSvg outputPath TopDown graph
                        pure (Right (selectionName, outputPath))
    case result of
        Left err -> putStrLn ("Failed to render graph: " <> err)
        Right (selectionName, path) -> putStrLn ("Rendered " <> selectionName <> " SVG: " <> path)
    when (isLeft result) (fail "siren-hs render failed")
