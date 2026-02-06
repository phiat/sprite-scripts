# sprite-tool — F# port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- .NET SDK >= 8.0

## Build

```sh
dotnet build
```

## Run

```sh
dotnet run -- launch my-sprite plan.md
```

## Project structure

```
SpriteTool.fsproj
Program.fs
Config.fs
Sprite.fs
Launch.fs
Push.fs
Pull.fs
Watch.fs
```
