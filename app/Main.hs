module Main (main) where

import Control.Monad (when)
import Data.Either (isLeft)
import System.Directory (createDirectoryIfMissing)

import Samples (writeSampleSvg)

main :: IO ()
main = do
    createDirectoryIfMissing True "output"
    result <- writeSampleSvg "output/sample.svg"
    case result of
        Left err -> putStrLn ("Failed to render sample: " <> err)
        Right path -> putStrLn ("Rendered sample SVG: " <> path)
    when (isLeft result) (fail "siren-hs sample render failed")
