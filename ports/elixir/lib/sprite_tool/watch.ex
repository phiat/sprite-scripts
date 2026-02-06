defmodule SpriteTool.Watch do
  @moduledoc """
  sprite-tool watch: Poll a sprite's beads tracker task for progress.
  """

  alias SpriteTool.Sprite

  @doc """
  Execute the watch subcommand.
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

      [] ->
        usage()
        System.halt(1)

      [sprite_name | rest] ->
        {task_id, poll_interval} = parse_watch_args(rest)
        do_watch(sprite_name, task_id, poll_interval)
    end
  end

  defp parse_watch_args([]) do
    {"", 30}
  end

  defp parse_watch_args([task_id]) do
    {task_id, 30}
  end

  defp parse_watch_args([task_id, interval_str | _]) do
    interval =
      case Integer.parse(interval_str) do
        {n, ""} -> n
        _ -> 30
      end

    {task_id, interval}
  end

  defp do_watch(sprite_name, task_id, poll_interval) do
    # Auto-detect tracker task if not specified
    task_id = resolve_task_id(sprite_name, task_id)

    IO.puts("Watching sprite '#{sprite_name}' task '#{task_id}' (every #{poll_interval}s)")
    IO.puts("Press Ctrl+C to stop")
    IO.puts("")

    watch_loop(sprite_name, task_id, poll_interval)
  end

  defp resolve_task_id(sprite_name, "") do
    IO.puts("Detecting tracker task...")

    # Try critical tasks first
    task_id =
      Sprite.sx(
        sprite_name,
        "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'"
      )
      |> normalize_task_id()

    task_id =
      if task_id == "" do
        IO.puts("No critical task found. Falling back to first open task...")

        Sprite.sx(
          sprite_name,
          "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'"
        )
        |> normalize_task_id()
      else
        task_id
      end

    if task_id == "" do
      IO.puts("ERROR: No beads tasks found on sprite '#{sprite_name}'")
      IO.puts("Specify a task ID manually: sprite-tool watch #{sprite_name} <task-id>")
      System.halt(1)
    end

    IO.puts("Tracking task: #{task_id}")
    task_id
  end

  defp resolve_task_id(_sprite_name, task_id), do: task_id

  defp normalize_task_id(nil), do: ""
  defp normalize_task_id(str), do: String.trim(str)

  defp watch_loop(sprite_name, task_id, poll_interval) do
    # Clear screen and move cursor to top-left
    IO.write("\e[2J\e[H")

    now = Calendar.strftime(DateTime.utc_now(), "%H:%M:%S")
    IO.puts("=== sprite-watch: #{sprite_name} / #{task_id} === #{now} ===")
    IO.puts("")

    # Show task status
    task_output =
      Sprite.sx(sprite_name, "cd /home/sprite && bd show #{task_id} 2>/dev/null")

    case task_output do
      nil -> IO.puts("(could not read task)")
      "" -> IO.puts("(could not read task)")
      output -> IO.puts(output)
    end

    IO.puts("")

    # Show recent comments
    IO.puts("--- Recent updates ---")

    comments_output =
      Sprite.sx(sprite_name, "cd /home/sprite && bd comments #{task_id} 2>/dev/null | tail -8")

    case comments_output do
      nil -> IO.puts("(no comments)")
      "" -> IO.puts("(no comments)")
      output -> IO.puts(output)
    end

    IO.puts("")

    # Check if done
    status =
      Sprite.sx(
        sprite_name,
        "cd /home/sprite && bd show #{task_id} 2>/dev/null | grep -i status"
      ) || ""

    if status =~ ~r/closed|done|completed/i do
      IO.puts("==========================================")
      IO.puts("PROJECT COMPLETE")
      IO.puts("==========================================")
      :ok
    else
      Process.sleep(poll_interval * 1000)
      watch_loop(sprite_name, task_id, poll_interval)
    end
  end

  defp usage do
    IO.puts("""
    Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

    Arguments:
      sprite-name     Name of the sprite to watch
      task-id         Beads task ID to track (default: auto-detect first open critical task)
      poll-interval   Seconds between polls (default: 30)

    Examples:
      sprite-tool watch ember-red-hawk
      sprite-tool watch ember-red-hawk CRM-1
      sprite-tool watch ember-red-hawk CRM-1 60\
    """)
  end
end
