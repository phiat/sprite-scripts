# sprite-tool — Ada port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- GNAT (Ada compiler)
- GPRBuild

## Build

```sh
gprbuild -P sprite_tool.gpr -p
```

## Run

```sh
./sprite_tool launch my-sprite plan.md
```

## Project structure

```
sprite_tool.gpr
src/
  sprite_tool.adb
  config.ads
  config.adb
  sprite_exec.ads
  sprite_exec.adb
  launch_cmd.ads
  launch_cmd.adb
  push_cmd.ads
  push_cmd.adb
  pull_cmd.ads
  pull_cmd.adb
  watch_cmd.ads
  watch_cmd.adb
```
