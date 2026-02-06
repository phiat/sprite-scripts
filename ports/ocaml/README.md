# sprite-tool — OCaml port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- OCaml >= 5.0
- Dune >= 3.0
- opam

## Build

```sh
dune build
```

## Run

```sh
dune exec sprite-tool -- launch my-sprite plan.md
```

## Project structure

```
dune-project
sprite-tool.opam
bin/
  main.ml
  dune
lib/
  config.ml
  sprite.ml
  launch.ml
  push.ml
  pull.ml
  watch.ml
  dune
```
