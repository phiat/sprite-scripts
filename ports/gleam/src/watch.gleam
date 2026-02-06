/// sprite-tool watch: Poll a sprite's beads tracker task for progress.

import gleam/int
import gleam/io
import gleam/string
import sprite

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

@external(erlang, "timer", "sleep")
fn sleep_ms(milliseconds: Int) -> Dynamic

type Dynamic

/// Print usage information and halt.
fn usage() -> Nil {
  io.println(
    "Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

Arguments:
  sprite-name     Name of the sprite to watch
  task-id         Beads task ID to track (default: auto-detect first open critical task)
  poll-interval   Seconds between polls (default: 30)

Examples:
  sprite-tool watch ember-red-hawk
  sprite-tool watch ember-red-hawk CRM-1
  sprite-tool watch ember-red-hawk CRM-1 60",
  )
  halt(1)
}

/// Run a command inside a sprite, capturing output.
fn sx(sprite_name: String, cmd: String) -> String {
  sprite.sx(sprite_name, cmd, False)
  |> string.trim()
}

/// Run a command inside a sprite and print output directly.
fn sx_print(sprite_name: String, cmd: String) -> String {
  let output = sx(sprite_name, cmd)
  case output {
    "" -> ""
    _ -> {
      io.println(output)
      output
    }
  }
}

/// Auto-detect the tracker task ID.
fn detect_task(sprite_name: String) -> String {
  io.println("Detecting tracker task...")

  // Try critical tasks first
  let task_id =
    sx(
      sprite_name,
      "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'",
    )

  case task_id {
    "" -> {
      io.println("No critical task found. Falling back to first open task...")
      let task_id2 =
        sx(
          sprite_name,
          "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'",
        )
      case task_id2 {
        "" -> {
          io.println(
            "ERROR: No beads tasks found on sprite '" <> sprite_name <> "'",
          )
          io.println(
            "Specify a task ID manually: sprite-tool watch "
            <> sprite_name
            <> " <task-id>",
          )
          halt(1)
          // unreachable but needed for type checker
          ""
        }
        tid -> {
          io.println("Tracking task: " <> tid)
          tid
        }
      }
    }
    tid -> {
      io.println("Tracking task: " <> tid)
      tid
    }
  }
}

/// The main watch loop - polls repeatedly until done.
fn watch_loop(
  sprite_name: String,
  task_id: String,
  poll_interval: Int,
) -> Nil {
  sprite.clear_screen()
  let now = sprite.time_now()
  io.println(
    "=== sprite-watch: "
    <> sprite_name
    <> " / "
    <> task_id
    <> " === "
    <> now
    <> " ===",
  )
  io.println("")

  // Show task status
  let output =
    sx_print(
      sprite_name,
      "cd /home/sprite && bd show " <> task_id <> " 2>/dev/null",
    )
  case output {
    "" -> io.println("(could not read task)")
    _ -> Nil
  }
  io.println("")

  // Show recent comments
  io.println("--- Recent updates ---")
  let comments =
    sx_print(
      sprite_name,
      "cd /home/sprite && bd comments " <> task_id <> " 2>/dev/null | tail -8",
    )
  case comments {
    "" -> io.println("(no comments)")
    _ -> Nil
  }
  io.println("")

  // Check if done
  let status =
    sx(
      sprite_name,
      "cd /home/sprite && bd show "
      <> task_id
      <> " 2>/dev/null | grep -i status",
    )

  let status_lower = string.lowercase(status)
  case
    string.contains(status_lower, "closed")
    || string.contains(status_lower, "done")
    || string.contains(status_lower, "completed")
  {
    True -> {
      io.println("==========================================")
      io.println("PROJECT COMPLETE")
      io.println("==========================================")
      Nil
    }
    False -> {
      let _ = sleep_ms(poll_interval * 1000)
      watch_loop(sprite_name, task_id, poll_interval)
    }
  }
}

/// Execute the watch subcommand.
pub fn run(args: List(String)) -> Nil {
  case args {
    [] -> usage()
    [sprite_name, ..rest] -> {
      let #(task_id, poll_interval) = case rest {
        [] -> #("", 30)
        [tid] -> #(tid, 30)
        [tid, interval_str, ..] ->
          case int.parse(interval_str) {
            Ok(n) -> #(tid, n)
            Error(_) -> {
              io.println(
                "Error: invalid poll-interval '"
                <> interval_str
                <> "' (must be integer)",
              )
              halt(1)
              // unreachable
              #("", 30)
            }
          }
      }

      // Auto-detect task if not specified
      let task_id = case task_id {
        "" -> detect_task(sprite_name)
        tid -> tid
      }

      io.println(
        "Watching sprite '"
        <> sprite_name
        <> "' task '"
        <> task_id
        <> "' (every "
        <> int.to_string(poll_interval)
        <> "s)",
      )
      io.println("Press Ctrl+C to stop")
      io.println("")

      watch_loop(sprite_name, task_id, poll_interval)
    }
  }
}
