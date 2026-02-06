# sprite-tool — Haskell port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- GHC
- Cabal

## Build

```sh
cabal build
```

## Run

```sh
cabal run sprite-tool -- launch my-sprite plan.md
```

## Project structure

```
sprite-tool.cabal
app/
  Main.hs
src/
  Config.hs
  Sprite.hs
  Launch.hs
  Push.hs
  Pull.hs
  Watch.hs
```
