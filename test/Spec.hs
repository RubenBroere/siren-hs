module Main (main) where

import qualified Siren.EDSLSpec as EDSLSpec
import qualified Siren.LayoutSpec as LayoutSpec
import qualified Siren.Render.SVGSpec as RenderSVGSpec
import qualified Siren.SirenSpec as SirenSpec
import qualified Siren.TypesSpec as TypesSpec
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "siren-hs"
        [ TypesSpec.tests
        , EDSLSpec.tests
        , LayoutSpec.tests
        , RenderSVGSpec.tests
        , SirenSpec.tests
        ]
