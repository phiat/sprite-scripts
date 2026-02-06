/// sprite-tool pull: Pull file or directory from a sprite.

import gleam/io
import gleam/string
import sprite

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

/// Print usage information and halt.
fn usage() -> Nil {
  io.println(
    "Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

Examples:
  sprite-tool pull /home/sprite/file.txt ./file.txt
  sprite-tool pull /home/sprite/mydir ./mydir
  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk",
  )
  halt(1)
}

/// Shell-escape a string for embedding in a command.
fn shell_escape(s: String) -> String {
  "'" <> string.replace(s, "'", "'\\''") <> "'"
}

/// Execute the pull subcommand.
pub fn run(args: List(String)) -> Nil {
  case args {
    [remote_path, local_path, ..rest] -> {
      let sprite_name_arg = case rest {
        [name, ..] -> " -s " <> shell_escape(name)
        [] -> ""
      }

      // Check if remote is directory or file
      let check_cmd =
        "sprite exec"
        <> sprite_name_arg
        <> " bash -c "
        <> shell_escape(
          "[ -d '" <> remote_path <> "' ] && echo dir || echo file",
        )
      let result = string.trim(sprite.exec(check_cmd))
      let is_dir = result == "dir"

      case is_dir {
        True -> {
          io.println(
            "Pulling directory: " <> remote_path <> " -> " <> local_path,
          )
          let _ = sprite.mkdir_p(local_path)

          // sprite exec [-s name] tar czf - -C REMOTE . | tar xzf - -C LOCAL
          let command =
            "sprite exec"
            <> sprite_name_arg
            <> " tar czf - -C "
            <> shell_escape(remote_path)
            <> " . | tar xzf - -C "
            <> shell_escape(local_path)
          let result = sprite.exec_pipe(command)
          case result {
            0 -> Nil
            _ -> {
              io.println("Error: pull failed")
              halt(1)
            }
          }
        }
        False -> {
          io.println(
            "Pulling file: " <> remote_path <> " -> " <> local_path,
          )
          let local_dir = sprite.dirname(local_path)
          let _ = sprite.mkdir_p(local_dir)

          // sprite exec [-s name] cat REMOTE > local
          let command =
            "sprite exec"
            <> sprite_name_arg
            <> " cat "
            <> shell_escape(remote_path)
            <> " > "
            <> shell_escape(local_path)
          let result = sprite.exec_pipe(command)
          case result {
            0 -> Nil
            _ -> {
              io.println("Error: pull failed")
              halt(1)
            }
          }
        }
      }

      io.println("Done.")
    }
    _ -> usage()
  }
}
