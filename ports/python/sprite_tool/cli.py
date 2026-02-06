"""CLI entry point: argparse with subparsers for launch, push, pull, watch."""

import sys

from . import launch, pull, push, watch


def main(argv=None):
    """Main CLI dispatcher.

    We use lightweight manual subcommand parsing instead of argparse subparsers
    because each subcommand does its own flag parsing internally (launch has
    repeatable --upload flags, watch has positional-only args, etc.). This keeps
    the individual modules self-contained and matches the bash originals.
    """
    if argv is None:
        argv = sys.argv[1:]

    if not argv or argv[0] in ("-h", "--help", "help"):
        _print_help()
        sys.exit(0)

    if argv[0] == "--version":
        from . import __version__

        print(f"sprite-tool {__version__}")
        sys.exit(0)

    subcommand = argv[0]
    sub_args = argv[1:]

    commands = {
        "launch": launch.run,
        "push": push.run,
        "pull": pull.run,
        "watch": watch.run,
    }

    if subcommand not in commands:
        print(f"Error: unknown subcommand '{subcommand}'")
        print()
        _print_help()
        sys.exit(1)

    commands[subcommand](sub_args)


def _print_help():
    print("""\
sprite-tool: manage sprites with coding agents, git, and beads

Usage: sprite-tool <command> [options] [args...]

Commands:
  launch    Create and configure a sprite with coding agent, git, beads
  push      Push local file or directory to a sprite
  pull      Pull file or directory from a sprite
  watch     Poll beads task for progress

Run 'sprite-tool <command> --help' for command-specific help.

Global options:
  --help, -h    Show this help
  --version     Show version""")
