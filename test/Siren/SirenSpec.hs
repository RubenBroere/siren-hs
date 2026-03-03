module Siren.SirenSpec (tests) where

import Siren
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Siren"
        [ testCase "writeGraphSvg writes an SVG file" unit_writeGraphSvgWritesFile
        ]

unit_writeGraphSvgWritesFile :: IO ()
unit_writeGraphSvgWritesFile = do
    createDirectoryIfMissing True "output"
    let outputPath = "output/test-siren-write-svg-spec.svg"
    removeIfExists outputPath

    case buildGraph (node "a" "A" <> node "b" "B" <> edgeWithLabel "a" "b" (Just "go")) of
        Left err -> assertFailure ("Failed to construct graph for writeGraphSvg: " <> err)
        Right graph -> writeGraphSvg outputPath TopDown graph

    created <- doesFileExist outputPath
    assertBool "Expected writeGraphSvg to create the SVG file" created
    removeIfExists outputPath

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
    exists <- doesFileExist path
    if exists then removeFile path else pure ()
