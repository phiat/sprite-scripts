/// Subprocess wrapper for the sprite CLI using Erlang FFI.

import gleam/io
import gleam/string

// --- Erlang FFI ---

/// Execute a shell command via Erlang os:cmd/1.
/// os:cmd takes a charlist and returns a charlist.
@external(erlang, "sprite_ffi", "exec_cmd")
fn ffi_exec(command: String) -> String

/// Execute a shell command, returning the exit code.
@external(erlang, "sprite_ffi", "exec_cmd_status")
fn ffi_exec_status(command: String) -> Int

/// Execute a piped command (cmd1 | cmd2) for push/pull operations.
@external(erlang, "sprite_ffi", "exec_pipe")
fn ffi_exec_pipe(command: String) -> Int

/// Check if a command exists on PATH.
@external(erlang, "sprite_ffi", "command_exists")
pub fn command_exists(name: String) -> Bool

/// Get the user's home directory.
@external(erlang, "sprite_ffi", "home_dir")
pub fn home_dir() -> String

/// Check if a local path is a directory.
@external(erlang, "sprite_ffi", "is_directory")
pub fn is_directory(path: String) -> Bool

/// Check if a local path exists.
@external(erlang, "sprite_ffi", "file_exists")
pub fn file_exists(path: String) -> Bool

/// Get the parent directory of a path.
@external(erlang, "sprite_ffi", "dirname")
pub fn dirname(path: String) -> String

/// Get the base name of a path.
@external(erlang, "sprite_ffi", "basename")
pub fn basename(path: String) -> String

/// Create a directory and parents.
@external(erlang, "sprite_ffi", "mkdir_p")
pub fn mkdir_p(path: String) -> Bool

/// Clear the terminal screen.
@external(erlang, "sprite_ffi", "clear_screen")
pub fn clear_screen() -> Nil

/// Get current time as HH:MM:SS string.
@external(erlang, "sprite_ffi", "time_now")
pub fn time_now() -> String

// --- Public API ---

/// Run a shell command and return stdout as a string.
pub fn exec(command: String) -> String {
  ffi_exec(command)
}

/// Run a shell command and return the exit code.
pub fn exec_status(command: String) -> Int {
  ffi_exec_status(command)
}

/// Run a piped shell command and return the exit code.
pub fn exec_pipe(command: String) -> Int {
  ffi_exec_pipe(command)
}

/// Shell-escape a string for embedding in a bash -c command.
fn shell_escape(s: String) -> String {
  "'" <> string.replace(s, "'", "'\\''") <> "'"
}

/// Run a command inside a sprite via bash.
/// Equivalent to: sprite exec -s SPRITE bash -c "CMD"
pub fn sx(sprite_name: String, cmd: String, dry_run: Bool) -> String {
  case dry_run {
    True -> {
      io.println(
        "  [dry-run] sprite exec -s " <> sprite_name <> " bash -c \"" <> cmd
        <> "\"",
      )
      ""
    }
    False -> {
      let command =
        "sprite exec -s "
        <> shell_escape(sprite_name)
        <> " bash -c "
        <> shell_escape(cmd)
      exec(command)
    }
  }
}

/// Run a command inside a sprite, printing output directly to terminal.
/// Returns the exit code.
pub fn sx_passthrough(
  sprite_name: String,
  cmd: String,
  dry_run: Bool,
) -> Int {
  case dry_run {
    True -> {
      io.println(
        "  [dry-run] sprite exec -s " <> sprite_name <> " bash -c \"" <> cmd
        <> "\"",
      )
      0
    }
    False -> {
      let command =
        "sprite exec -s "
        <> shell_escape(sprite_name)
        <> " bash -c "
        <> shell_escape(cmd)
      exec_status(command)
    }
  }
}

/// Push a local file to a sprite.
pub fn push_file(
  sprite_name: String,
  src: String,
  dest: String,
  dry_run: Bool,
) -> Nil {
  case dry_run {
    True -> {
      io.println("  [dry-run] push " <> src <> " -> sprite:" <> dest)
      Nil
    }
    False -> {
      let dest_dir = dirname(dest)
      let _ =
        sx(sprite_name, "mkdir -p '" <> dest_dir <> "'", False)
      let command =
        "sprite exec -s "
        <> shell_escape(sprite_name)
        <> " bash -c "
        <> shell_escape("cat > '" <> dest <> "'")
        <> " < "
        <> shell_escape(src)
      let _ = exec_status(command)
      Nil
    }
  }
}

/// Push a local directory to a sprite via tar.
pub fn push_dir(
  sprite_name: String,
  src: String,
  dest: String,
  dry_run: Bool,
) -> Nil {
  case dry_run {
    True -> {
      io.println("  [dry-run] push dir " <> src <> " -> sprite:" <> dest)
      Nil
    }
    False -> {
      let _ =
        sx(sprite_name, "mkdir -p '" <> dest <> "'", False)
      let parent = dirname(src)
      let base = basename(src)
      let dest_parent = dirname(dest)
      let command =
        "tar czf - -C "
        <> shell_escape(parent)
        <> " "
        <> shell_escape(base)
        <> " | sprite exec -s "
        <> shell_escape(sprite_name)
        <> " bash -c "
        <> shell_escape("tar xzf - -C '" <> dest_parent <> "'")
      let _ = exec_pipe(command)
      Nil
    }
  }
}

/// Check if a sprite with the given name already exists.
pub fn sprite_exists(sprite_name: String) -> Bool {
  let output = exec("sprite ls 2>/dev/null")
  output
  |> string.split("\n")
  |> find_sprite_in_listing(sprite_name)
}

fn find_sprite_in_listing(lines: List(String), name: String) -> Bool {
  case lines {
    [] -> False
    [line, ..rest] -> {
      let words =
        line
        |> string.split(" ")
        |> filter_non_empty([])
      case list_contains(words, name) {
        True -> True
        False -> find_sprite_in_listing(rest, name)
      }
    }
  }
}

fn filter_non_empty(words: List(String), acc: List(String)) -> List(String) {
  case words {
    [] -> acc
    [word, ..rest] ->
      case word {
        "" -> filter_non_empty(rest, acc)
        _ -> filter_non_empty(rest, [word, ..acc])
      }
  }
}

fn list_contains(items: List(String), target: String) -> Bool {
  case items {
    [] -> False
    [item, ..rest] ->
      case item == target {
        True -> True
        False -> list_contains(rest, target)
      }
  }
}
