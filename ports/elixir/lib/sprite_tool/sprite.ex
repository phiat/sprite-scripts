defmodule SpriteTool.Sprite do
  @moduledoc """
  Subprocess wrapper for the sprite CLI.

  Provides helpers to execute commands on sprites, push files and directories,
  and query sprite status. Uses System.cmd/3 for simple commands and Port for
  piped operations (tar streaming).
  """

  @doc """
  Find the sprite CLI binary on PATH.
  Returns the path string or nil if not found.
  """
  @spec find_sprite_cli() :: String.t() | nil
  def find_sprite_cli do
    System.find_executable("sprite")
  end

  @doc """
  Run a command inside a sprite via bash.

  Equivalent to: sprite exec -s SPRITE bash -c "CMD"

  Returns stdout as a trimmed string, or nil in dry-run mode.
  """
  @spec sx(String.t(), String.t(), boolean()) :: String.t() | nil
  def sx(sprite_name, cmd, dry_run \\ false) do
    if dry_run do
      IO.puts("  [dry-run] sprite exec -s #{sprite_name} bash -c \"#{cmd}\"")
      nil
    else
      case System.cmd("sprite", ["exec", "-s", sprite_name, "bash", "-c", cmd],
             stderr_to_stdout: true
           ) do
        {output, 0} -> String.trim(output)
        {output, _} -> String.trim(output)
      end
    end
  end

  @doc """
  Run a command inside a sprite, passing stdout/stderr through to the terminal.

  Returns the exit code.
  """
  @spec sx_passthrough(String.t(), String.t(), boolean()) :: non_neg_integer()
  def sx_passthrough(sprite_name, cmd, dry_run \\ false) do
    if dry_run do
      IO.puts("  [dry-run] sprite exec -s #{sprite_name} bash -c \"#{cmd}\"")
      0
    else
      port =
        Port.open(
          {:spawn_executable, System.find_executable("sprite")},
          [
            :binary,
            :exit_status,
            :use_stdio,
            :stderr_to_stdout,
            args: ["exec", "-s", sprite_name, "bash", "-c", cmd]
          ]
        )

      stream_port_output(port)
    end
  end

  @doc """
  Push a local file to a sprite.

  Equivalent to:
    sprite exec -s SPRITE bash -c "mkdir -p $(dirname DEST)"
    sprite exec -s SPRITE bash -c "cat > DEST" < src
  """
  @spec push_file(String.t(), String.t(), String.t(), boolean()) :: :ok
  def push_file(sprite_name, src, dest, dry_run \\ false) do
    if dry_run do
      IO.puts("  [dry-run] push #{src} -> sprite:#{dest}")
      :ok
    else
      dest_dir = Path.dirname(dest)

      System.cmd("sprite", [
        "exec",
        "-s",
        sprite_name,
        "bash",
        "-c",
        "mkdir -p '#{dest_dir}'"
      ])

      # Pipe file content to sprite exec stdin
      file_data = File.read!(src)

      port =
        Port.open(
          {:spawn_executable, System.find_executable("sprite")},
          [
            :binary,
            :exit_status,
            args: ["exec", "-s", sprite_name, "bash", "-c", "cat > '#{dest}'"]
          ]
        )

      Port.command(port, file_data)
      Port.close(port)

      # Wait for the port to finish
      receive do
        {^port, {:exit_status, _status}} -> :ok
      after
        30_000 -> :ok
      end
    end
  end

  @doc """
  Push a local directory to a sprite via tar.

  Equivalent to:
    sprite exec -s SPRITE bash -c "mkdir -p DEST"
    tar czf - -C parent base | sprite exec -s SPRITE bash -c "tar xzf - -C parent_of_dest"
  """
  @spec push_dir(String.t(), String.t(), String.t(), boolean()) :: :ok
  def push_dir(sprite_name, src, dest, dry_run \\ false) do
    if dry_run do
      IO.puts("  [dry-run] push dir #{src} -> sprite:#{dest}")
      :ok
    else
      parent = Path.dirname(src)
      base = Path.basename(src)
      dest_parent = Path.dirname(dest)

      System.cmd("sprite", [
        "exec",
        "-s",
        sprite_name,
        "bash",
        "-c",
        "mkdir -p '#{dest}'"
      ])

      # tar czf - -C parent base | sprite exec -s SPRITE bash -c "tar xzf - -C dest_parent"
      # Use a shell pipeline to connect the two commands
      {_output, exit_code} =
        System.cmd("bash", [
          "-c",
          "tar czf - -C '#{parent}' '#{base}' | sprite exec -s '#{sprite_name}' bash -c \"tar xzf - -C '#{dest_parent}'\""
        ])

      if exit_code != 0 do
        IO.puts(:stderr, "Warning: push_dir failed with exit code #{exit_code}")
      end

      :ok
    end
  end

  @doc """
  Run 'sprite ls' and return stdout.
  """
  @spec sprite_list() :: String.t()
  def sprite_list do
    case System.cmd("sprite", ["ls"], stderr_to_stdout: true) do
      {output, _} -> output
    end
  end

  @doc """
  Check if a sprite with the given name already exists.
  """
  @spec sprite_exists?(String.t()) :: boolean()
  def sprite_exists?(sprite_name) do
    output = sprite_list()

    output
    |> String.split("\n")
    |> Enum.any?(fn line ->
      line
      |> String.split()
      |> Enum.member?(sprite_name)
    end)
  end

  # Stream output from a port to stdout and return the exit status.
  defp stream_port_output(port) do
    receive do
      {^port, {:data, data}} ->
        IO.write(data)
        stream_port_output(port)

      {^port, {:exit_status, status}} ->
        status
    end
  end
end
