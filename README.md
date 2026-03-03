# siren-hs

`siren-hs` is a Haskell-first graph visualization library and executable for flowchart-style diagrams.
The current implementation provides a foundational pipeline from a typed EDSL to SVG output using `diagrams-svg`.

## Current status (Week 2 foundation)

The project now contains a working end-to-end baseline:

- A typed graph model (`Node`, `Edge`, `Graph`) in `src/Siren/Types.hs`
- A small EDSL for programmatic graph construction in `src/Siren/EDSL.hs`
- A Sugiyama-like layered layout pass in `src/Siren/Layout.hs`
- SVG rendering via `diagrams` / `diagrams-svg` in `src/Siren/Render/SVG.hs`
- Public library API in `src/Siren.hs`
- App-local sample graph module in `app/Samples.hs`
- Executable entry point that generates an SVG in `app/Main.hs`
- Baseline tests in `test/Spec.hs`

## What it can do now

- Build flowchart graphs with node shapes: rectangle, rounded rectangle, diamond
- Add directed edges with optional labels
- Compute rank-based node layout (`TopDown` and `LeftRight`)
- Render valid SVG output through the `diagrams-svg` backend

## Quick start

### Build and test

```bash
stack test
```

### Generate the sample SVG

```bash
stack run
```

This writes the example diagram to:

- `output/sample.svg`

## Library usage

The top-level API is exported from `Siren`.

Key exported values/functions:

- `writeGraphSvg :: FilePath -> Direction -> Graph -> IO ()`

Sample/demo helpers (`sampleGraph`, `writeSampleSvg`) are intentionally kept in `app/Samples.hs` so they do not leak into the public library API.

## Project workflow (`stack` + `hpack`)

This repository uses `package.yaml` as the source of truth.

- Edit dependencies/components in `package.yaml`
- Let Stack generate `siren-hs.cabal` automatically (`stack build` / `stack test`)
- Do not hand-edit generated `siren-hs.cabal`

## Next steps

Planned next milestones include:

- Improving layout quality (crossing reduction and coordinate refinement)
- Expanding examples and test coverage
- Adding parser/CLI as stretch goals after core library stabilization
