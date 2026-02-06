(* push.ml — sprite-tool push: push a local file or directory to a sprite *)

let usage () =
  Printf.printf
{|Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

Examples:
  sprite-tool push ./file.txt /home/sprite/file.txt
  sprite-tool push ./mydir /home/sprite/mydir
  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk
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
    let local_path = List.nth args 0 in
    let remote_path = List.nth args 1 in
    let sprite_name = resolve_sprite_name args in
    let dry_run = false in

    if not (Sys.file_exists local_path) then begin
      Printf.eprintf "Error: %s does not exist\n%!" local_path;
      exit 1
    end;

    if Sys.is_directory local_path then begin
      Printf.printf "Pushing directory: %s -> %s\n%!" local_path remote_path;
      (* For directory push: tar up and extract with strip-components *)
      let parent_dir = Filename.dirname local_path in
      let base_name = Filename.basename local_path in
      let cmd =
        Printf.sprintf "tar czf - -C %s %s | sprite exec -s %s bash -c %s"
          (Filename.quote parent_dir)
          (Filename.quote base_name)
          (Filename.quote sprite_name)
          (Filename.quote (Printf.sprintf "mkdir -p '%s' && tar xzf - -C '%s' --strip-components=1"
            remote_path remote_path))
      in
      let code = Sprite.system_checked cmd in
      if code <> 0 then begin
        Printf.eprintf "Error: push directory failed (exit %d)\n%!" code;
        exit 1
      end
    end else begin
      Printf.printf "Pushing file: %s -> %s\n%!" local_path remote_path;
      let remote_dir = Filename.dirname remote_path in
      let _ = Sprite.sx ~dry_run ~sprite_name
        (Printf.sprintf "mkdir -p '%s'" remote_dir) in
      let cmd =
        Printf.sprintf "sprite exec -s %s bash -c %s < %s"
          (Filename.quote sprite_name)
          (Filename.quote (Printf.sprintf "cat > '%s'" remote_path))
          (Filename.quote local_path)
      in
      let code = Sprite.system_checked cmd in
      if code <> 0 then begin
        Printf.eprintf "Error: push file failed (exit %d)\n%!" code;
        exit 1
      end
    end;

    Printf.printf "Done.\n%!"
