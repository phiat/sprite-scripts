(* watch.ml — sprite-tool watch: poll a sprite's beads tracker task for progress *)

let usage () =
  Printf.printf
{|Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

Arguments:
  sprite-name     Name of the sprite to watch
  task-id         Beads task ID to track (default: auto-detect first open critical task)
  poll-interval   Seconds between polls (default: 30)

Examples:
  sprite-tool watch ember-red-hawk
  sprite-tool watch ember-red-hawk CRM-1
  sprite-tool watch ember-red-hawk CRM-1 60
|};
  exit 1

(* Auto-detect the task ID from the sprite *)
let detect_task_id ~sprite_name =
  (* Try critical priority first *)
  let (_code, output) = Sprite.sx ~dry_run:false ~sprite_name
    "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'" in
  let task = String.trim output in
  if String.length task > 0 then
    Some task
  else begin
    (* Fall back to first open task *)
    Printf.printf "No critical task found. Falling back to first open task...\n%!";
    let (_code2, output2) = Sprite.sx ~dry_run:false ~sprite_name
      "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'" in
    let task2 = String.trim output2 in
    if String.length task2 > 0 then Some task2
    else None
  end

(* Check if the status string indicates a completed task *)
let is_task_done status =
  let lower = String.lowercase_ascii status in
  List.exists (fun kw ->
    (* Check if the keyword appears anywhere in the lowered status *)
    let kw_len = String.length kw in
    let s_len = String.length lower in
    if kw_len > s_len then false
    else
      let found = ref false in
      for i = 0 to s_len - kw_len do
        if String.sub lower i kw_len = kw then
          found := true
      done;
      !found
  ) ["closed"; "done"; "completed"]

let run args =
  match args with
  | [] -> usage ()
  | _ ->
    let sprite_name = List.hd args in
    let task_id_arg = match args with _ :: tid :: _ -> tid | _ -> "" in
    let poll_interval =
      match args with
      | _ :: _ :: pi :: _ ->
        (try int_of_string pi
         with Failure _ ->
           Printf.eprintf "Error: invalid poll interval %S\n%!" pi;
           exit 1)
      | _ -> 30
    in

    (* Auto-detect task ID if not specified *)
    let task_id =
      if String.length task_id_arg > 0 then
        task_id_arg
      else begin
        Printf.printf "Detecting tracker task...\n%!";
        match detect_task_id ~sprite_name with
        | Some id ->
          Printf.printf "Tracking task: %s\n%!" id;
          id
        | None ->
          Printf.eprintf "ERROR: No beads tasks found on sprite '%s'\n%!" sprite_name;
          Printf.eprintf "Specify a task ID manually: sprite-tool watch %s <task-id>\n%!" sprite_name;
          exit 1
      end
    in

    Printf.printf "Watching sprite '%s' task '%s' (every %ds)\n%!" sprite_name task_id poll_interval;
    Printf.printf "Press Ctrl+C to stop\n\n%!";

    let keep_running = ref true in
    while !keep_running do
      (* Clear screen *)
      let _ = Sprite.system_checked "clear" in

      let now = Unix.localtime (Unix.gettimeofday ()) in
      Printf.printf "=== sprite-watch: %s / %s === %02d:%02d:%02d ===\n\n%!"
        sprite_name task_id now.Unix.tm_hour now.Unix.tm_min now.Unix.tm_sec;

      (* Show task status *)
      let (_code, status_output) = Sprite.sx ~dry_run:false ~sprite_name
        (Printf.sprintf "cd /home/sprite && bd show %s 2>/dev/null" task_id) in
      if String.length status_output > 0 then
        Printf.printf "%s\n%!" status_output
      else
        Printf.printf "(could not read task)\n%!";
      Printf.printf "\n%!";

      (* Show recent comments *)
      Printf.printf "--- Recent updates ---\n%!";
      let (_code2, comments) = Sprite.sx ~dry_run:false ~sprite_name
        (Printf.sprintf "cd /home/sprite && bd comments %s 2>/dev/null | tail -8" task_id) in
      if String.length comments > 0 then
        Printf.printf "%s\n%!" comments
      else
        Printf.printf "(no comments)\n%!";
      Printf.printf "\n%!";

      (* Check if task is done *)
      let (_code3, status_line) = Sprite.sx ~dry_run:false ~sprite_name
        (Printf.sprintf "cd /home/sprite && bd show %s 2>/dev/null | grep -i status" task_id) in
      if is_task_done status_line then begin
        Printf.printf "==========================================\n%!";
        Printf.printf "PROJECT COMPLETE\n%!";
        Printf.printf "==========================================\n%!";
        keep_running := false
      end else
        Unix.sleep poll_interval
    done
