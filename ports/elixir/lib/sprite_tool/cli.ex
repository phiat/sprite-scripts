defmodule SpriteTool.CLI do
  @moduledoc """
  OptionParser-based CLI dispatch for sprite-tool.

  This module is the escript entry point. It dispatches to the appropriate
  subcommand module based on the first argument.
  """

  @doc """
  Entry point for the escript. Receives argv as a list of strings.
  """
  @spec main([String.t()]) :: no_return()
  def main([]) do
    usage()
    System.halt(1)
  end

  def main(["--help" | _]) do
    usage()
    System.halt(0)
  end

  def main(["-h" | _]) do
    usage()
    System.halt(0)
  end

  def main(["--version" | _]) do
    IO.puts("sprite-tool #{SpriteTool.version()}")
    System.halt(0)
  end

  def main([subcommand | args]) do
    case subcommand do
      "launch" ->
        SpriteTool.Launch.run(args)

      "push" ->
        SpriteTool.Push.run(args)

      "pull" ->
        SpriteTool.Pull.run(args)

      "watch" ->
        SpriteTool.Watch.run(args)

      other ->
        IO.puts(:stderr, "Unknown command: #{other}")
        usage()
        System.halt(1)
    end
  end

  defp usage do
    IO.puts("""
    Usage: sprite-tool <command> [options] [args]

    Commands:
      launch    Create and configure a sprite with coding agent, git, beads
      push      Push local file or directory to a sprite
      pull      Pull file or directory from a sprite
      watch     Poll a sprite's beads tracker task for progress

    Options:
      --help       Show this help message
      --version    Show version

    Run 'sprite-tool <command> --help' for command-specific help.\
    """)
  end
end
