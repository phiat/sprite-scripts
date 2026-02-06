defmodule SpriteTool.Push do
  @moduledoc """
  sprite-tool push: Push local file or directory to a sprite.
  """

  @doc """
  Execute the push subcommand.
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

      [local_path, remote_path | rest] ->
        sprite_name = List.first(rest)
        do_push(local_path, remote_path, sprite_name)

      _ ->
        usage()
        System.halt(1)
    end
  end

  defp do_push(local_path, remote_path, sprite_name) do
    unless File.exists?(local_path) do
      IO.puts(:stderr, "Error: #{local_path} does not exist")
      System.halt(1)
    end

    sprite_args = build_sprite_args(sprite_name)

    if File.dir?(local_path) do
      IO.puts("Pushing directory: #{local_path} -> #{remote_path}")
      push_directory(local_path, remote_path, sprite_args)
    else
      IO.puts("Pushing file: #{local_path} -> #{remote_path}")
      push_single_file(local_path, remote_path, sprite_args)
    end

    IO.puts("Done.")
    :ok
  end

  defp push_directory(local_path, remote_path, sprite_args) do
    parent = Path.dirname(local_path)
    base = Path.basename(local_path)

    # tar czf - -C parent base | sprite exec ARGS bash -c "mkdir -p DEST && tar xzf - -C DEST --strip-components=1"
    tar_cmd = "tar czf - -C '#{parent}' '#{base}'"

    sprite_cmd =
      "sprite exec #{sprite_args} bash -c \"mkdir -p '#{remote_path}' && tar xzf - -C '#{remote_path}' --strip-components=1\""

    {_output, exit_code} = System.cmd("bash", ["-c", "#{tar_cmd} | #{sprite_cmd}"])

    if exit_code != 0 do
      IO.puts(:stderr, "Warning: directory push failed with exit code #{exit_code}")
    end
  end

  defp push_single_file(local_path, remote_path, sprite_args) do
    remote_dir = Path.dirname(remote_path)

    # mkdir -p remote_dir && cat > remote_path
    {_output, _exit_code} =
      System.cmd("bash", [
        "-c",
        "sprite exec #{sprite_args} bash -c \"mkdir -p '#{remote_dir}' && cat > '#{remote_path}'\" < '#{local_path}'"
      ])
  end

  defp build_sprite_args(nil), do: ""
  defp build_sprite_args(""), do: ""
  defp build_sprite_args(sprite_name), do: "-s '#{sprite_name}'"

  defp usage do
    IO.puts("""
    Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

    Examples:
      sprite-tool push ./file.txt /home/sprite/file.txt
      sprite-tool push ./mydir /home/sprite/mydir
      sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk\
    """)
  end
end
