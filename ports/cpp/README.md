# sprite-tool — C++ port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- C++17 compiler
- CMake >= 3.10

## Build

```sh
cmake -B build && cmake --build build
```

## Run

```sh
./build/sprite-tool launch my-sprite plan.md
```

## Project structure

```
CMakeLists.txt
src/
  main.cpp
  config.cpp
  config.h
  sprite.cpp
  sprite.h
  launch.cpp
  launch.h
  push.cpp
  push.h
  pull.cpp
  pull.h
  watch.cpp
  watch.h
```
