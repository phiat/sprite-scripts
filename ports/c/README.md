# sprite-tool — C port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- GCC or Clang
- Make

## Build

```sh
make
```

## Run

```sh
./sprite-tool launch my-sprite plan.md
```

## Project structure

```
Makefile
main.c
config.c
config.h
sprite.c
sprite.h
launch.c
launch.h
push.c
push.h
pull.c
pull.h
watch.c
watch.h
```
