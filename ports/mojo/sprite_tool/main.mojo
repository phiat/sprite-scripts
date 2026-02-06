"""CLI entry point: dispatch subcommands for launch, push, pull, watch."""

from python import Python, PythonObject

from . import launch, pull, push, watch


def _print_help():
    print(
        "sprite-tool: manage sprites with coding agents, git, and beads\n"
        "\n"
        "Usage: sprite-tool <command> [options] [args...]\n"
        "\n"
        "Commands:\n"
        "  launch    Create and configure a sprite with coding agent, git, beads\n"
        "  push      Push local file or directory to a sprite\n"
        "  pull      Pull file or directory from a sprite\n"
        "  watch     Poll beads task for progress\n"
        "\n"
        "Run 'sprite-tool <command> --help' for command-specific help.\n"
        "\n"
        "Global options:\n"
        "  --help, -h    Show this help\n"
        "  --version     Show version"
    )


fn main() raises:
    """Main CLI dispatcher."""
    var sys_mod = Python.import_module("sys")

    # Get argv from Python sys module
    var argv = sys_mod.argv
    var num_args = int(len(argv))

    # Skip argv[0] (program name), collect the rest
    var args = Python.list()
    for i in range(1, num_args):
        args.append(argv[i])

    var arg_count = int(len(args))

    if arg_count == 0:
        _print_help()
        _ = sys_mod.exit(0)

    var first_arg = str(args[0])

    if first_arg == "-h" or first_arg == "--help" or first_arg == "help":
        _print_help()
        _ = sys_mod.exit(0)

    if first_arg == "--version":
        print("sprite-tool 0.1.0")
        _ = sys_mod.exit(0)

    var subcommand = first_arg

    # Collect sub_args (everything after the subcommand)
    var sub_args = Python.list()
    for i in range(1, arg_count):
        sub_args.append(args[i])

    if subcommand == "launch":
        launch.run(sub_args)
    elif subcommand == "push":
        push.run(sub_args)
    elif subcommand == "pull":
        pull.run(sub_args)
    elif subcommand == "watch":
        watch.run(sub_args)
    else:
        print("Error: unknown subcommand '" + subcommand + "'")
        print("")
        _print_help()
        _ = sys_mod.exit(1)
