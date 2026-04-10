# siren-hs

`siren-hs` is a Haskell library and CLI for building directed flowchart-style graphs and rendering them to SVG.

The project includes:

- A typed graph model in `src/Siren/Types.hs`
- A composable EDSL for graph construction in `src/Siren/EDSL.hs`
- Two layered layout engines:
	- Sugiyama-like (`src/Siren/Layout/Sugiyama.hs`)
	- Dagre-style (`src/Siren/Layout/Dagre.hs`)
- An SVG renderer based on `diagrams-svg` in `src/Siren/Render/SVG.hs`
- A CLI application with sample graphs in `app/Main.hs` and `app/Samples.hs`

## Features

- Node shapes: rectangle, rounded rectangle, diamond
- Directed edges with optional labels
- Two layout strategies for hierarchical diagrams
- Layout direction support (`TopDown`, `LeftRight`) in the library APIs
- Batch rendering of all built-in sample graphs

## Installation and build

This project uses Stack.

```bash
stack build
```

Run tests:

```bash
stack test
```

## CLI usage

The executable expects a graph name and optionally a layout engine.

```bash
stack run -- GRAPH_NAME [--layout ENGINE]
```

Short option for layout:

```bash
stack run -- GRAPH_NAME -l ENGINE
```

Where:

- `ENGINE` is `sugiyama` (default) or `dagre`
- Output is written to `output/<graph>-<engine>.svg`

### Available graph names

- `sample`
- `bugged`
- `chain`
- `diamond-merge`
- `fan-in`
- `crossing-bipartite`
- `long-label`
- `multi-source-sink`
- `cycle`
- `dense-layered`
- `disconnected`
- `ladder`

### Render one graph

```bash
stack run -- sample
stack run -- long-label --layout dagre
```

### Render all built-in samples with both engines

```bash
stack run -- all
```

This generates 24 SVG files (12 samples x 2 engines) in `output/`.

## Library usage

Import the top-level `Siren` module:

```haskell
import Siren
```

Common convenience functions:

- `writeGraphSvg :: FilePath -> Direction -> Graph -> IO ()`
- `writeGraphSvgDagre :: FilePath -> Direction -> Graph -> IO ()`
- `writeGraphWith :: (LayoutEngine engine, RenderEngine renderer LaidOutGraph) => engine -> renderer -> FilePath -> Graph -> IO ()`

Minimal example:

```haskell
import Siren

main :: IO ()
main =
	case buildGraph (node "start" "Start" <> node "end" "End" <> edge "start" "end") of
		Left err -> putStrLn err
		Right g -> writeGraphSvg "output/example.svg" TopDown g
```

## Project structure

- `src/Siren.hs`: top-level public API
- `src/Siren/Types.hs`: core graph types
- `src/Siren/EDSL.hs`: graph construction DSL
- `src/Siren/Layout/*.hs`: layout engines and shared layout types/utilities
- `src/Siren/Render/SVG.hs`: SVG rendering backend
- `app/Main.hs`: CLI entry point
- `app/Samples.hs`: sample graphs used by the CLI
- `test/`: test suite (`tasty`, `tasty-hunit`, `tasty-quickcheck`)

## Development notes

This repository uses `package.yaml` (hpack) as the source of truth.

- Edit package metadata and dependencies in `package.yaml`
- Let Stack/hpack regenerate `siren-hs.cabal`
- Avoid manual edits to generated Cabal fields
