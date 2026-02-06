# sprite-tool — C# port

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
SpriteTool.csproj
Program.cs
Config.cs
SpriteExec.cs
Commands/
  LaunchCommand.cs
  PushCommand.cs
  PullCommand.cs
  WatchCommand.cs
```
