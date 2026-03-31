module Samples
    ( sampleGraph
    , buggedGraph
    )
where

import Siren

sampleGraph :: Either String Graph
sampleGraph =
  buildGraph
    (  diamondNode "A" "Start"
    <> node "B" "Left"
    <> node "C" "Right"
    <> roundedNode "D" "Down"
    <> roundedNode "E" "End"
    <> edgeWithLabel "A" "B" (Just "go left")
    <> edgeWithLabel "A" "C" (Just "go right")
    <> edge "B" "D"
    <> edge "B" "E"
    )

buggedGraph :: Either String Graph
buggedGraph =
  buildGraph
    (  roundedNode "start" "Start"
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
