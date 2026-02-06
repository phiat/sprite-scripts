(* main.ml — Entry point and subcommand dispatch for sprite-tool *)

open Sprite_tool

let usage () =
  Printf.printf
{|sprite-tool: A CLI tool for managing sprites with coding agents

Usage: sprite-tool <command> [args...]

Commands:
  launch    Create and configure a sprite with a coding agent
  push      Push a local file or directory to a sprite
  pull      Pull a file or directory from a sprite
  watch     Poll a beads task on a sprite for progress

Run 'sprite-tool <command> --help' for more information on a command.
|};
  exit 1

let () =
  let argv = Array.to_list Sys.argv in
  (* argv.(0) is the program name; the rest are arguments *)
  let args = match argv with _ :: rest -> rest | [] -> [] in
  match args with
  | [] -> usage ()
  | "launch" :: rest -> Launch.run rest
  | "push" :: rest -> Push.run rest
  | "pull" :: rest -> Pull.run rest
  | "watch" :: rest -> Watch.run rest
  | "--help" :: _ | "-h" :: _ -> usage ()
  | cmd :: _ ->
    Printf.eprintf "Unknown command: %s\n\n%!" cmd;
    usage ()
