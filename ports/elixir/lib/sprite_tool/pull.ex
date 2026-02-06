defmodule SpriteTool.Pull do
  @moduledoc """
  sprite-tool pull: Pull file or directory from a sprite.
  """

  @doc """
  Execute the pull subcommand.
  """
  @spec run([String.t()]) :: :ok
  def run(args) do
    case args do
      ["--help" | _] ->
        usage()
        System.halt(0)

      ["-h" | _] ->
        usage()
        System.halt(0)

      [remote_path, local_path | rest] ->
        sprite_name = List.first(rest)
        do_pull(remote_path, local_path, sprite_name)

      _ ->
        usage()
        System.halt(1)
    end
  end

  defp do_pull(remote_path, local_path, sprite_name) do
    sprite_args = build_sprite_args(sprite_name)

    # Check if remote path is a directory
    is_dir = check_remote_dir(remote_path, sprite_args)

    if is_dir do
      IO.puts("Pulling directory: #{remote_path} -> #{local_path}")
      pull_directory(remote_path, local_path, sprite_args)
    else
      IO.puts("Pulling file: #{remote_path} -> #{local_path}")
      pull_single_file(remote_path, local_path, sprite_args)
    end

    IO.puts("Done.")
    :ok
  end

  defp check_remote_dir(remote_path, sprite_args) do
    {output, _exit_code} =
      System.cmd("bash", [
        "-c",
        "sprite exec #{sprite_args} bash -c \"[ -d '#{remote_path}' ] && echo 'dir' || echo 'file'\""
      ])

    String.trim(output) == "dir"
  end

  defp pull_directory(remote_path, local_path, sprite_args) do
    File.mkdir_p!(local_path)

    # sprite exec ARGS tar czf - -C remote_path . | tar xzf - -C local_path
    {_output, exit_code} =
      System.cmd("bash", [
        "-c",
        "sprite exec #{sprite_args} tar czf - -C '#{remote_path}' . | tar xzf - -C '#{local_path}'"
      ])

    if exit_code != 0 do
      IO.puts(:stderr, "Warning: directory pull failed with exit code #{exit_code}")
    end
  end

  defp pull_single_file(remote_path, local_path, sprite_args) do
    local_dir = Path.dirname(local_path)
    File.mkdir_p!(local_dir)

    # sprite exec ARGS cat remote_path > local_path
    {_output, exit_code} =
      System.cmd("bash", [
        "-c",
        "sprite exec #{sprite_args} cat '#{remote_path}' > '#{local_path}'"
      ])

    if exit_code != 0 do
      IO.puts(:stderr, "Warning: file pull failed with exit code #{exit_code}")
    end
  end

  defp build_sprite_args(nil), do: ""
  defp build_sprite_args(""), do: ""
  defp build_sprite_args(sprite_name), do: "-s '#{sprite_name}'"

  defp usage do
    IO.puts("""
    Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

    Examples:
      sprite-tool pull /home/sprite/file.txt ./file.txt
      sprite-tool pull /home/sprite/mydir ./mydir
      sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk\
    """)
  end
end
