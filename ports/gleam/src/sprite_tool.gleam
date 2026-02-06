/// sprite-tool: Gleam port of sprite-scripts.
/// Manage sprites with coding agents, git, and beads.

import argv
import gleam/io
import launch
import pull
import push
import watch

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

fn usage() -> Nil {
  io.println(
    "sprite-tool: Manage sprites with coding agents, git, and beads.

Usage: sprite_tool <command> [args...]

Commands:
  launch    Create and configure a sprite with coding agent
  push      Push local file or directory to a sprite
  pull      Pull file or directory from a sprite
  watch     Poll a sprite's beads task for progress

Run 'sprite_tool <command> --help' for command-specific help.",
  )
  halt(1)
}

pub fn main() {
  let args = argv.load().arguments

  case args {
    ["launch", ..rest] -> launch.run(rest)
    ["push", ..rest] -> push.run(rest)
    ["pull", ..rest] -> pull.run(rest)
    ["watch", ..rest] -> watch.run(rest)
    ["--help", ..] -> usage()
    ["-h", ..] -> usage()
    [] -> usage()
    [cmd, ..] -> {
      io.println("Unknown command: " <> cmd)
      usage()
    }
  }
}
