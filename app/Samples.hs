module Samples
    ( sampleGraph
    , writeSampleSvg
    )
where

import Siren

sampleGraph :: Either String Graph
sampleGraph =
  buildGraph
    ( roundedNode "start" "Start"
        <> node "read" "Read Input"
        <> diamondNode "valid" "Valid?"
        <> node "process" "Process"
        <> roundedNode "done" "Done"
        <> edge "start" "read"
        <> edge "read" "valid"
        <> edgeWithLabel "valid" "process" (Just "yes")
        <> edgeWithLabel "valid" "done" (Just "no")
        <> edge "process" "done"
    )

writeSampleSvg :: FilePath -> IO (Either String FilePath)
writeSampleSvg outputPath =
    case sampleGraph of
        Left err -> pure (Left err)
        Right graph -> do
            writeGraphSvg outputPath TopDown graph
            pure (Right outputPath)
