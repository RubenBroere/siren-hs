module Siren.Render.SVGSpec (tests) where

import Siren
import Siren.Render.SVG (renderGraphToFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Siren.Render.SVG"
        [ testCase "renderGraphToFile writes an SVG file" unit_renderGraphToFileWritesFile
        ]

unit_renderGraphToFileWritesFile :: IO ()
unit_renderGraphToFileWritesFile = do
    createDirectoryIfMissing True "output"
    let outputPath = "output/test-render-svg-spec.svg"
    removeIfExists outputPath

    case buildGraph (node "start" "Start" <> node "end" "End" <> edge "start" "end") of
        Left err -> assertFailure ("Failed to construct graph for rendering: " <> err)
        Right graph -> renderGraphToFile outputPath (layoutSugiyamaLike TopDown graph)

    created <- doesFileExist outputPath
    assertBool "Expected renderGraphToFile to create the SVG file" created
    removeIfExists outputPath

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
    exists <- doesFileExist path
    if exists then removeFile path else pure ()
