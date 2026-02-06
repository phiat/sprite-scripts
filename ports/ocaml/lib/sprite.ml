(* sprite.ml — Wrapper around the sprite CLI for exec, file transfer, etc. *)

(* Run a shell command and return its exit status code.
   Returns 0 on success, non-zero on failure. *)
let system_checked cmd =
  match Unix.system cmd with
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED _ -> 128
  | Unix.WSTOPPED _ -> 128

(* Run a shell command and capture its stdout as a string.
   Returns (exit_code, stdout_string). *)
let capture_output cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 256 in
  (try
     while true do
       let line = input_line ic in
       if Buffer.length buf > 0 then Buffer.add_char buf '\n';
       Buffer.add_string buf line
     done
   with End_of_file -> ());
  let status = Unix.close_process_in ic in
  let code =
    match status with
    | Unix.WEXITED c -> c
    | Unix.WSIGNALED _ -> 128
    | Unix.WSTOPPED _ -> 128
  in
  (code, String.trim (Buffer.contents buf))

(* Execute a bash command on the sprite via `sprite exec -s <name> bash -c "<cmd>"`.
   In dry-run mode, prints the command and returns (0, "").
   Otherwise returns (exit_code, stdout). *)
let sx ~dry_run ~sprite_name cmd =
  if dry_run then begin
    Printf.printf "  [dry-run] sprite exec -s %s bash -c %S\n%!" sprite_name cmd;
    (0, "")
  end else
    let full_cmd =
      Printf.sprintf "sprite exec -s %s bash -c %s"
        (Filename.quote sprite_name)
        (Filename.quote cmd)
    in
    capture_output full_cmd

(* Run a plain `sprite <args>` command, returning (exit_code, stdout). *)
let run_sprite ~dry_run args =
  if dry_run then begin
    Printf.printf "  [dry-run] sprite %s\n%!" (String.concat " " args);
    (0, "")
  end else
    let cmd =
      Printf.sprintf "sprite %s"
        (String.concat " " (List.map Filename.quote args))
    in
    capture_output cmd

(* Run a plain `sprite <args>` command interactively (inherits stdin/stdout/stderr).
   Returns exit code. *)
let run_sprite_interactive ~dry_run args =
  if dry_run then begin
    Printf.printf "  [dry-run] sprite %s\n%!" (String.concat " " args);
    0
  end else
    let cmd =
      Printf.sprintf "sprite %s"
        (String.concat " " (List.map Filename.quote args))
    in
    system_checked cmd

(* Check if sprite CLI is installed; install if not. *)
let rec ensure_cli ~dry_run =
  let (code, _) = capture_output "command -v sprite" in
  if code = 0 then
    ()
  else begin
    if dry_run then
      Printf.printf "  [dry-run] Would install sprite CLI\n%!"
    else begin
      Printf.printf "Installing sprite CLI...\n%!";
      let rc = system_checked "curl -fsSL https://sprites.dev/install.sh | sh" in
      if rc <> 0 then begin
        Printf.eprintf "Error: Failed to install sprite CLI (exit %d)\n%!" rc;
        exit 1
      end;
      (* Add to path *)
      let home = getenv_default "HOME" "." in
      let new_path = Printf.sprintf "%s/.local/bin:%s" home (getenv_default "PATH" "") in
      Unix.putenv "PATH" new_path
    end
  end

and getenv_default key default_val =
  match Sys.getenv_opt key with
  | Some v when String.length (String.trim v) > 0 -> v
  | _ -> default_val

(* Authenticate with sprite service. *)
let auth ~dry_run token =
  if String.length token > 0 then begin
    Printf.printf "Authenticating sprite with token...\n%!";
    if not dry_run then begin
      let (code, _) = run_sprite ~dry_run:false ["auth"; "setup"; "--token"; token] in
      if code <> 0 then begin
        Printf.eprintf "Error: sprite auth failed (exit %d)\n%!" code;
        exit 1
      end
    end
  end else begin
    Printf.printf "No SPRITE_TOKEN set. Running interactive login...\n%!";
    if not dry_run then begin
      let code = run_sprite_interactive ~dry_run:false ["login"] in
      if code <> 0 then begin
        Printf.eprintf "Error: sprite login failed (exit %d)\n%!" code;
        exit 1
      end
    end
  end

(* Check if a sprite already exists by looking at `sprite ls` output. *)
let exists ~dry_run sprite_name =
  if dry_run then begin
    Printf.printf "  [dry-run] sprite ls (checking for %s)\n%!" sprite_name;
    false
  end else
    let (code, output) = capture_output "sprite ls 2>/dev/null" in
    if code <> 0 then false
    else
      let lines = String.split_on_char '\n' output in
      List.exists (fun line ->
        let words = String.split_on_char ' ' line in
        List.exists (fun w -> String.trim w = sprite_name) words
      ) lines

(* Create a new sprite. *)
let create ~dry_run sprite_name =
  if dry_run then
    Printf.printf "  [dry-run] sprite create -skip-console %s\n%!" sprite_name
  else begin
    Printf.printf "Creating sprite: %s\n%!" sprite_name;
    let code = run_sprite_interactive ~dry_run:false ["create"; "-skip-console"; sprite_name] in
    if code <> 0 then begin
      Printf.eprintf "Error: sprite create failed (exit %d)\n%!" code;
      exit 1
    end
  end

(* Push a local file to a remote path on the sprite. *)
let push_file ~dry_run ~sprite_name local_path remote_path =
  if dry_run then
    Printf.printf "  [dry-run] push %s -> sprite:%s\n%!" local_path remote_path
  else begin
    let remote_dir = Filename.dirname remote_path in
    let _ = sx ~dry_run:false ~sprite_name
      (Printf.sprintf "mkdir -p '%s'" remote_dir) in
    let cmd =
      Printf.sprintf "sprite exec -s %s bash -c %s < %s"
        (Filename.quote sprite_name)
        (Filename.quote (Printf.sprintf "cat > '%s'" remote_path))
        (Filename.quote local_path)
    in
    let code = system_checked cmd in
    if code <> 0 then
      Printf.eprintf "Warning: push_file to %s failed (exit %d)\n%!" remote_path code
  end

(* Push a local directory to a remote path on the sprite using tar. *)
let push_dir ~dry_run ~sprite_name local_dir remote_path =
  if dry_run then
    Printf.printf "  [dry-run] push dir %s -> sprite:%s\n%!" local_dir remote_path
  else begin
    let _ = sx ~dry_run:false ~sprite_name
      (Printf.sprintf "mkdir -p '%s'" remote_path) in
    let parent_dir = Filename.dirname local_dir in
    let base_name = Filename.basename local_dir in
    let cmd =
      Printf.sprintf "tar czf - -C %s %s | sprite exec -s %s bash -c %s"
        (Filename.quote parent_dir)
        (Filename.quote base_name)
        (Filename.quote sprite_name)
        (Filename.quote (Printf.sprintf "tar xzf - -C '%s'" (Filename.dirname remote_path)))
    in
    let code = system_checked cmd in
    if code <> 0 then
      Printf.eprintf "Warning: push_dir to %s failed (exit %d)\n%!" remote_path code
  end

(* Pull a remote file from the sprite to a local path. *)
let pull_file ~dry_run ~sprite_name remote_path local_path =
  if dry_run then
    Printf.printf "  [dry-run] pull sprite:%s -> %s\n%!" remote_path local_path
  else begin
    (* Ensure local directory exists *)
    let local_dir = Filename.dirname local_path in
    let _ = system_checked (Printf.sprintf "mkdir -p %s" (Filename.quote local_dir)) in
    let cmd =
      Printf.sprintf "sprite exec -s %s cat %s > %s"
        (Filename.quote sprite_name)
        (Filename.quote remote_path)
        (Filename.quote local_path)
    in
    let code = system_checked cmd in
    if code <> 0 then begin
      Printf.eprintf "Error: pull_file from %s failed (exit %d)\n%!" remote_path code;
      exit 1
    end
  end

(* Pull a remote directory from the sprite to a local path using tar. *)
let pull_dir ~dry_run ~sprite_name remote_path local_path =
  if dry_run then
    Printf.printf "  [dry-run] pull dir sprite:%s -> %s\n%!" remote_path local_path
  else begin
    let _ = system_checked (Printf.sprintf "mkdir -p %s" (Filename.quote local_path)) in
    let cmd =
      Printf.sprintf "sprite exec -s %s tar czf - -C %s . | tar xzf - -C %s"
        (Filename.quote sprite_name)
        (Filename.quote remote_path)
        (Filename.quote local_path)
    in
    let code = system_checked cmd in
    if code <> 0 then begin
      Printf.eprintf "Error: pull_dir from %s failed (exit %d)\n%!" remote_path code;
      exit 1
    end
  end

(* Check whether a remote path on the sprite is a directory. *)
let is_remote_dir ~dry_run ~sprite_name remote_path =
  if dry_run then begin
    Printf.printf "  [dry-run] check if %s is dir on sprite\n%!" remote_path;
    false
  end else
    let (_code, output) = sx ~dry_run:false ~sprite_name
      (Printf.sprintf "[ -d '%s' ] && echo dir || echo file" remote_path) in
    String.trim output = "dir"

(* Create a checkpoint for the sprite. Returns exit code. *)
let checkpoint_create ~dry_run sprite_name =
  if dry_run then begin
    Printf.printf "  [dry-run] sprite checkpoint create -s %s\n%!" sprite_name;
    0
  end else
    let (code, _) = run_sprite ~dry_run:false ["checkpoint"; "create"; "-s"; sprite_name] in
    code

(* Open a console to the sprite. Returns exit code. *)
let console ~dry_run sprite_name =
  if dry_run then begin
    Printf.printf "  [dry-run] sprite console -s %s\n%!" sprite_name;
    0
  end else
    run_sprite_interactive ~dry_run:false ["console"; "-s"; sprite_name]
