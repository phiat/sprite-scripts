/// sprite-tool push: Push local file or directory to a sprite.

import gleam/io
import gleam/string
import sprite

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

/// Print usage information and halt.
fn usage() -> Nil {
  io.println(
    "Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

Examples:
  sprite-tool push ./file.txt /home/sprite/file.txt
  sprite-tool push ./mydir /home/sprite/mydir
  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk",
  )
  halt(1)
}

/// Shell-escape a string for embedding in a command.
fn shell_escape(s: String) -> String {
  "'" <> string.replace(s, "'", "'\\''") <> "'"
}

/// Execute the push subcommand.
pub fn run(args: List(String)) -> Nil {
  case args {
    [local_path, remote_path, ..rest] -> {
      let sprite_name_arg = case rest {
        [name, ..] -> " -s " <> shell_escape(name)
        [] -> ""
      }

      // Check local path exists
      case sprite.file_exists(local_path) || sprite.is_directory(local_path) {
        False -> {
          io.println("Error: " <> local_path <> " does not exist")
          halt(1)
        }
        True -> Nil
      }

      case sprite.is_directory(local_path) {
        True -> {
          io.println(
            "Pushing directory: " <> local_path <> " -> " <> remote_path,
          )
          let parent = sprite.dirname(local_path)
          let base = sprite.basename(local_path)

          // tar czf - -C parent base | sprite exec [-s name] bash -c "mkdir -p DEST && tar xzf - -C DEST --strip-components=1"
          let command =
            "tar czf - -C "
            <> shell_escape(parent)
            <> " "
            <> shell_escape(base)
            <> " | sprite exec"
            <> sprite_name_arg
            <> " bash -c "
            <> shell_escape(
              "mkdir -p '"
              <> remote_path
              <> "' && tar xzf - -C '"
              <> remote_path
              <> "' --strip-components=1",
            )
          let result = sprite.exec_pipe(command)
          case result {
            0 -> Nil
            _ -> {
              io.println("Error: push failed")
              halt(1)
            }
          }
        }
        False -> {
          io.println(
            "Pushing file: " <> local_path <> " -> " <> remote_path,
          )
          let remote_dir = sprite.dirname(remote_path)

          // sprite exec [-s name] bash -c "mkdir -p DIR && cat > DEST" < local
          let command =
            "sprite exec"
            <> sprite_name_arg
            <> " bash -c "
            <> shell_escape(
              "mkdir -p '"
              <> remote_dir
              <> "' && cat > '"
              <> remote_path
              <> "'",
            )
            <> " < "
            <> shell_escape(local_path)
          let result = sprite.exec_pipe(command)
          case result {
            0 -> Nil
            _ -> {
              io.println("Error: push failed")
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
