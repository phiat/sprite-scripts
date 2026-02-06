(* pull.ml — sprite-tool pull: pull a file or directory from a sprite *)

let usage () =
  Printf.printf
{|Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

Examples:
  sprite-tool pull /home/sprite/file.txt ./file.txt
  sprite-tool pull /home/sprite/mydir ./mydir
  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk
|};
  exit 1

let resolve_sprite_name args =
  match args with
  | _ :: _ :: name :: _ -> name
  | _ ->
    (match Sys.getenv_opt "SPRITE_NAME" with
     | Some name when String.length (String.trim name) > 0 -> String.trim name
     | _ ->
       Printf.eprintf "Error: sprite name not provided and SPRITE_NAME not set\n%!";
       exit 1)

let run args =
  match args with
  | [] | [_] -> usage ()
  | _ ->
    let remote_path = List.nth args 0 in
    let local_path = List.nth args 1 in
    let sprite_name = resolve_sprite_name args in
    let dry_run = false in

    (* Check if remote path is a directory *)
    let is_dir = Sprite.is_remote_dir ~dry_run ~sprite_name remote_path in

    if is_dir then begin
      Printf.printf "Pulling directory: %s -> %s\n%!" remote_path local_path;
      Sprite.pull_dir ~dry_run ~sprite_name remote_path local_path
    end else begin
      Printf.printf "Pulling file: %s -> %s\n%!" remote_path local_path;
      Sprite.pull_file ~dry_run ~sprite_name remote_path local_path
    end;

    Printf.printf "Done.\n%!"
