# sprite-tool — Common Lisp port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- SBCL (Steel Bank Common Lisp)

## Build

N/A (interpreted). Run via helper script.

## Run

```sh
./run.sh launch my-sprite plan.md
```

## Project structure

```
sprite-tool.asd
run.sh
src/
  packages.lisp
  config.lisp
  sprite.lisp
  launch.lisp
  push.lisp
  pull.lisp
  watch.lisp
  main.lisp
```
