module Samples
  ( sampleGraph
  , buggedGraph
  , chainGraph
  , diamondMergeGraph
  , fanInGraph
  , crossingBipartiteGraph
  , longLabelGraph
  , multiSourceSinkGraph
  , cycleGraph
  , denseLayeredGraph
  , disconnectedGraph
  , ladderGraph
  )
where

import Siren

sampleGraph :: Either String Graph
sampleGraph =
  buildGraph
    (  diamondNode "A" "Start"
    <> nodes [("B", "Left"), ("C", "Right")]
    <> roundedNodes [("D", "Down"), ("E", "End")]
    <> labeledEdges
      [ ("A", "B", "go left")
      , ("A", "C", "go right")
      ]
    <> edges [("B", "D"), ("B", "E")]
    )

buggedGraph :: Either String Graph
buggedGraph =
  buildGraph
    (  roundedNodes [("start", "Start"), ("done", "Done")]
    <> nodes [("read", "Read Input"), ("process", "Process")]
    <> diamondNode "valid" "Valid?"
    <> edges [("start", "read"), ("read", "valid"), ("process", "done")]
    <> labeledEdges
      [ ("valid", "process", "yes")
      , ("valid", "done", "no")
      ]
    )

chainGraph :: Either String Graph
chainGraph =
  buildGraph
    (  nodes [("a", "A"), ("b", "B"), ("c", "C"), ("d", "D")]
    <> labeledEdges
      [ ("a", "b", "step 1")
      , ("b", "c", "step 2")
      , ("c", "d", "step 3")
      ]
    )

-- This example often differs across engines because sibling order is heuristic.
diamondMergeGraph :: Either String Graph
diamondMergeGraph =
  buildGraph
    (  nodes [("a", "A"), ("b", "B"), ("c", "C"), ("d", "D")]
    <> labeledEdges
      [ ("a", "b", "left")
      , ("a", "c", "right")
      , ("b", "d", "merge")
      , ("c", "d", "merge")
      ]
    )

-- This example often differs across engines because fan-in alignment is heuristic.
fanInGraph :: Either String Graph
fanInGraph =
  buildGraph
    (  nodes [("a", "A"), ("b", "B"), ("c", "C"), ("d", "D"), ("e", "E")]
    <> labeledEdges
      [ ("a", "d", "input 1")
      , ("b", "d", "input 2")
      , ("c", "e", "side")
      , ("d", "e", "to sink")
      ]
    )

  -- This example often differs across engines because crossing reduction matters.
crossingBipartiteGraph :: Either String Graph
crossingBipartiteGraph =
  buildGraph
    (  nodes
      [ ("a", "A")
      , ("b", "B")
      , ("c", "C")
      , ("d", "D")
      , ("e", "E")
      , ("f", "F")
      ]
    <> labeledEdges
      [ ("a", "e", "route 1")
      , ("b", "d", "route 2")
      , ("c", "f", "route 3")
      ]
    )

  -- This example often differs across engines because node widths influence spacing.
longLabelGraph :: Either String Graph
longLabelGraph =
  buildGraph
    (  roundedNodes [("a", "VeryLongLabelNode"), ("d", "MediumLabel")]
    <> nodes [("b", "B"), ("c", "C")]
    <> labeledEdges
      [ ("a", "c", "wide")
      , ("b", "c", "short")
      , ("c", "d", "continue")
      ]
    )

  -- This example often differs across engines because barycenter ordering affects middle ranks.
multiSourceSinkGraph :: Either String Graph
multiSourceSinkGraph =
  buildGraph
    (  nodes
      [ ("s1", "S1")
      , ("s2", "S2")
      , ("s3", "S3")
      , ("m1", "M1")
      , ("m2", "M2")
      ]
    <> roundedNode "t" "T"
    <> labeledEdges
      [ ("s1", "m1", "feed a")
      , ("s2", "m1", "feed b")
      , ("s3", "m2", "feed c")
      , ("m1", "t", "join 1")
      , ("m2", "t", "join 2")
      ]
    )

  -- This example often differs across engines because Dagre performs an acyclic pass.
cycleGraph :: Either String Graph
cycleGraph =
  buildGraph
    (  nodes [("a", "A"), ("b", "B"), ("c", "C")]
    <> labeledEdges [("a", "b", "ab"), ("b", "c", "bc"), ("c", "a", "ca")]
    )

  -- This example often differs across engines because there are many crossing choices.
denseLayeredGraph :: Either String Graph
denseLayeredGraph =
  buildGraph
    (  nodes
      [ ("a", "A")
      , ("b", "B")
      , ("c", "C")
      , ("d", "D")
      , ("e", "E")
      , ("f", "F")
      , ("g", "G")
      , ("h", "H")
      ]
    <> labeledEdges
      [ ("a", "e", "ae")
      , ("a", "f", "af")
      , ("b", "e", "be")
      , ("b", "g", "bg")
      , ("c", "f", "cf")
      , ("c", "h", "ch")
      , ("d", "g", "dg")
      , ("d", "h", "dh")
      ]
    )

  -- This example can differ across engines in how disconnected components are packed.
disconnectedGraph :: Either String Graph
disconnectedGraph =
  buildGraph
    (  nodes [("a", "A"), ("b", "B"), ("c", "C"), ("x", "X"), ("y", "Y")]
    <> labeledEdges
      [ ("a", "b", "comp1")
      , ("b", "c", "comp1")
      , ("x", "y", "comp2")
      ]
    )

  -- This example often differs across engines because it is crossing-heavy and near-symmetric.
ladderGraph :: Either String Graph
ladderGraph =
  buildGraph
    (  nodes
      [ ("a1", "A1")
      , ("a2", "A2")
      , ("a3", "A3")
      , ("b1", "B1")
      , ("b2", "B2")
      , ("b3", "B3")
      ]
    <> labeledEdges
      [ ("a1", "a2", "rail a")
      , ("a2", "a3", "rail a")
      , ("b1", "b2", "rail b")
      , ("b2", "b3", "rail b")
      , ("a1", "b2", "cross")
      , ("b1", "a2", "cross")
      , ("a2", "b3", "cross")
      , ("b2", "a3", "cross")
      ]
    )
