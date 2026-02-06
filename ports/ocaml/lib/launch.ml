(* launch.ml — sprite-tool launch: create and configure a sprite with coding agent *)

let usage () =
  Printf.printf
{|Usage: sprite-tool launch [options] <sprite-name> [plan-file]

Options:
  --dry-run              Show what would happen without executing
  --no-checkpoint        Disable auto-checkpointing
  --upload <dir>         Upload a local directory to /home/sprite/<dirname>
                         (repeatable: --upload ./data --upload ./tests)

Environment variables:
  ENV_FILE               Path to .env file (default: ./.env)
  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)
  AGENT                  "opencode" (default) or "claude"
  CLAUDE_AUTH            "subscription" (default) or "apikey"
  MODEL                  Model override (see below)
  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)

Model examples:
  OpenCode: MODEL=opencode/big-pickle  (free, default)
            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)
            MODEL=openai/gpt-4o
            MODEL=google/gemini-2.5-pro
  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku

Examples:
  sprite-tool launch my-project plan.md
  sprite-tool launch --upload ./data my-project plan.md
  sprite-tool launch --upload ./data --upload ./tests my-project plan.md
  sprite-tool launch --dry-run my-project plan.md
  MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md
  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md
|};
  exit 1

(* Background checkpoint thread state *)
let checkpoint_running = ref false
let checkpoint_thread : Thread.t option ref = ref None

let start_checkpointing ~dry_run ~sprite_name ~interval =
  checkpoint_running := true;
  let t = Thread.create (fun () ->
    Printf.printf "Auto-checkpointing every %ds (background thread)\n%!" interval;
    while !checkpoint_running do
      Unix.sleep interval;
      if !checkpoint_running then begin
        let now = Unix.localtime (Unix.gettimeofday ()) in
        Printf.printf "[checkpoint] Creating checkpoint at %02d:%02d:%02d...\n%!"
          now.Unix.tm_hour now.Unix.tm_min now.Unix.tm_sec;
        let code = Sprite.checkpoint_create ~dry_run sprite_name in
        if code = 0 then
          Printf.printf "[checkpoint] Done.\n%!"
        else
          Printf.printf "[checkpoint] Failed (non-fatal).\n%!"
      end
    done
  ) () in
  checkpoint_thread := Some t

let stop_checkpointing () =
  checkpoint_running := false;
  match !checkpoint_thread with
  | Some _t ->
    (* The thread will exit on the next iteration check.
       We cannot forcibly join a sleeping thread in OCaml without signaling,
       so we just mark it and let it wind down. *)
    checkpoint_thread := None
  | None -> ()

(* Parse --flag arguments, return (dry_run, no_checkpoint, upload_dirs, remaining_args) *)
let parse_flags args =
  let dry_run = ref false in
  let no_checkpoint = ref false in
  let upload_dirs = ref [] in
  let rest = ref args in
  let continue_parsing = ref true in
  while !continue_parsing do
    match !rest with
    | "--dry-run" :: tl ->
      dry_run := true;
      rest := tl
    | "--no-checkpoint" :: tl ->
      no_checkpoint := true;
      rest := tl
    | "--upload" :: dir :: tl ->
      upload_dirs := dir :: !upload_dirs;
      rest := tl
    | "--help" :: _ | "-h" :: _ ->
      usage ()
    | s :: _ when String.length s > 0 && s.[0] = '-' && String.length s > 1 && s.[1] = '-' ->
      Printf.eprintf "Unknown option: %s\n%!" s;
      usage ()
    | _ ->
      continue_parsing := false
  done;
  (!dry_run, !no_checkpoint, List.rev !upload_dirs, !rest)

(* Setup the coding agent on the sprite *)
let setup_agent ~dry_run ~sprite_name (cfg : Config.t) =
  match String.lowercase_ascii cfg.agent with
  | "claude" ->
    Printf.printf "Setting up claude...\n%!";
    let _ = Sprite.sx ~dry_run ~sprite_name
      "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code" in
    if cfg.claude_auth = "subscription" then begin
      let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
      let creds = Filename.concat home ".claude/.credentials.json" in
      if Sys.file_exists creds then begin
        Printf.printf "Copying claude subscription credentials...\n%!";
        Sprite.push_file ~dry_run ~sprite_name creds "/home/sprite/.claude/.credentials.json";
        let _ = Sprite.sx ~dry_run ~sprite_name "chmod 600 ~/.claude/.credentials.json" in
        ()
      end else begin
        Printf.eprintf "ERROR: ~/.claude/.credentials.json not found\n%!";
        Printf.eprintf "Run 'claude' locally first to authenticate, then re-run this script.\n%!";
        exit 1
      end
    end else if cfg.claude_auth = "apikey" && String.length cfg.anthropic_api_key > 0 then begin
      Printf.printf "Setting ANTHROPIC_API_KEY in sprite...\n%!";
      let cmd = Printf.sprintf
        "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"%s\"' >> ~/.bashrc"
        cfg.anthropic_api_key in
      let _ = Sprite.sx ~dry_run ~sprite_name cmd in
      ()
    end else begin
      Printf.eprintf "ERROR: No valid claude auth configured\n%!";
      Printf.eprintf "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY\n%!";
      exit 1
    end

  | "opencode" ->
    Printf.printf "Setting up opencode...\n%!";
    let _ = Sprite.sx ~dry_run ~sprite_name
      "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash" in
    let _ = Sprite.sx ~dry_run ~sprite_name
      "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc" in
    ()

  | other ->
    Printf.eprintf "ERROR: Unknown AGENT '%s'. Use 'claude' or 'opencode'.\n%!" other;
    exit 1

(* Launch the agent with the plan or open console *)
let launch_agent ~dry_run ~sprite_name (cfg : Config.t) plan_file =
  match String.lowercase_ascii cfg.agent with
  | "claude" ->
    let model_flag =
      if String.length cfg.model > 0 then " --model " ^ cfg.model
      else ""
    in
    if String.length plan_file > 0 then begin
      Printf.printf "Launching claude with plan...\n%!";
      let cmd = Printf.sprintf "cd /home/sprite && claude%s -p 'read plan.md and complete the plan please'" model_flag in
      let (code, _) = Sprite.sx ~dry_run ~sprite_name cmd in
      code
    end else begin
      Printf.printf "Opening console...\n%!";
      Sprite.console ~dry_run sprite_name
    end

  | "opencode" ->
    let oc_model =
      if String.length cfg.model > 0 then cfg.model
      else "opencode/big-pickle"
    in
    if String.length plan_file > 0 then begin
      Printf.printf "Launching opencode with plan...\n%!";
      let cmd = Printf.sprintf
        "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m %s 'read plan.md and complete the plan please'"
        oc_model in
      let (code, _) = Sprite.sx ~dry_run ~sprite_name cmd in
      code
    end else begin
      Printf.printf "Opening console...\n%!";
      Sprite.console ~dry_run sprite_name
    end

  | _ ->
    Printf.eprintf "ERROR: Unknown agent\n%!";
    exit 1

let run args =
  let (dry_run, no_checkpoint, upload_dirs, rest) = parse_flags args in
  (match rest with [] -> usage () | _ -> ());
  let sprite_name = List.hd rest in
  let plan_file = match rest with _ :: pf :: _ -> pf | _ -> "" in

  (* Load configuration *)
  let cfg = Config.load () in

  (* Register cleanup *)
  at_exit (fun () -> stop_checkpointing ());

  (* Step 1: Check/install sprite CLI *)
  Sprite.ensure_cli ~dry_run;

  (* Step 2: Auth *)
  Sprite.auth ~dry_run cfg.sprite_token;

  (* Step 3: Create sprite or reuse existing *)
  if dry_run then
    Printf.printf "  [dry-run] Would create or reuse sprite '%s'\n%!" sprite_name
  else if Sprite.exists ~dry_run sprite_name then
    Printf.printf "Sprite '%s' already exists, using it.\n%!" sprite_name
  else
    Sprite.create ~dry_run sprite_name;

  (* Step 4: Push .env to sprite *)
  if Sys.file_exists cfg.env_file then begin
    Printf.printf "Pushing %s...\n%!" cfg.env_file;
    Sprite.push_file ~dry_run ~sprite_name cfg.env_file "/home/sprite/.env"
  end;

  (* Step 5: Push plan file if provided *)
  if String.length plan_file > 0 && Sys.file_exists plan_file then begin
    Printf.printf "Pushing %s...\n%!" plan_file;
    Sprite.push_file ~dry_run ~sprite_name plan_file "/home/sprite/plan.md"
  end;

  (* Step 6: Upload directories *)
  List.iter (fun dir ->
    if Sys.file_exists dir && Sys.is_directory dir then begin
      let dirname = Filename.basename dir in
      let remote = Printf.sprintf "/home/sprite/%s" dirname in
      Printf.printf "Uploading directory: %s -> %s\n%!" dir remote;
      Sprite.push_dir ~dry_run ~sprite_name dir remote
    end else
      Printf.printf "WARNING: --upload dir '%s' not found, skipping.\n%!" dir
  ) upload_dirs;

  (* Step 7: Git init + beads *)
  Printf.printf "Initializing git...\n%!";
  let _ = Sprite.sx ~dry_run ~sprite_name
    "cd /home/sprite && git init -b main 2>/dev/null || true" in

  Printf.printf "Installing beads...\n%!";
  let _ = Sprite.sx ~dry_run ~sprite_name
    "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash" in

  (* Step 8: Install and auth coding agent *)
  setup_agent ~dry_run ~sprite_name cfg;

  (* Step 9: Summary *)
  Printf.printf "\n==========================================\n%!";
  Printf.printf "Sprite '%s' is ready!\n%!" sprite_name;
  Printf.printf "Agent: %s%s\n%!" cfg.agent
    (if String.length cfg.model > 0 then Printf.sprintf " (model: %s)" cfg.model else "");
  if not no_checkpoint then
    Printf.printf "Checkpointing: every %ds\n%!" cfg.checkpoint_interval;
  Printf.printf "==========================================\n%!";

  if dry_run then begin
    Printf.printf "\n[dry-run] Would launch %s with plan. No changes were made.\n%!" cfg.agent;
    exit 0
  end;

  (* Step 10: Start checkpointing if enabled and plan provided *)
  if String.length plan_file > 0 then begin
    if not no_checkpoint then
      start_checkpointing ~dry_run ~sprite_name ~interval:cfg.checkpoint_interval;

    (* Step 11: Launch agent *)
    let _agent_exit = launch_agent ~dry_run ~sprite_name cfg plan_file in

    (* Step 12: Final checkpoint *)
    stop_checkpointing ();
    Printf.printf "Creating final checkpoint...\n%!";
    let code = Sprite.checkpoint_create ~dry_run sprite_name in
    if code = 0 then
      Printf.printf "Final checkpoint saved.\n%!"
    else
      Printf.printf "Final checkpoint failed (non-fatal).\n%!"
  end else begin
    Printf.printf "Opening console...\n%!";
    let _ = Sprite.console ~dry_run sprite_name in
    ()
  end
