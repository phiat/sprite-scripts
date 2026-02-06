(* config.ml — Load configuration from environment variables and .env file *)

type t = {
  sprite_token : string;
  agent : string;
  claude_auth : string;
  anthropic_api_key : string;
  model : string;
  checkpoint_interval : int;
  env_file : string;
}

(* Parse a single KEY=VALUE line from a .env file.
   Skips comments (#) and blank lines.
   Strips optional surrounding quotes from the value. *)
let parse_env_line line =
  let line = String.trim line in
  if String.length line = 0 || line.[0] = '#' then
    None
  else
    match String.index_opt line '=' with
    | None -> None
    | Some i ->
      let key = String.trim (String.sub line 0 i) in
      let raw_val = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
      (* Strip surrounding single or double quotes *)
      let val_len = String.length raw_val in
      let value =
        if val_len >= 2 then
          let first = raw_val.[0] in
          let last = raw_val.[val_len - 1] in
          if (first = '"' && last = '"') || (first = '\'' && last = '\'') then
            String.sub raw_val 1 (val_len - 2)
          else
            raw_val
        else
          raw_val
      in
      Some (key, value)

(* Load a .env file by parsing KEY=VALUE lines and calling Unix.putenv *)
let load_env_file path =
  if Sys.file_exists path then begin
    let ic = open_in path in
    (try
       while true do
         let line = input_line ic in
         match parse_env_line line with
         | Some (key, value) -> Unix.putenv key value
         | None -> ()
       done
     with End_of_file -> ());
    close_in ic
  end

(* Helper: get env var with a default *)
let getenv_default key default_val =
  match Sys.getenv_opt key with
  | Some v when String.length (String.trim v) > 0 -> String.trim v
  | _ -> default_val

(* Load configuration: reads .env file, then populates config from env *)
let load () =
  let env_file = getenv_default "ENV_FILE" "./.env" in
  load_env_file env_file;
  (* SPRITE_TOKEN with SPRITES_TOKEN fallback *)
  let sprite_token =
    match Sys.getenv_opt "SPRITE_TOKEN" with
    | Some v when String.length (String.trim v) > 0 -> String.trim v
    | _ -> getenv_default "SPRITES_TOKEN" ""
  in
  let agent = getenv_default "AGENT" "opencode" in
  let claude_auth = getenv_default "CLAUDE_AUTH" "subscription" in
  let anthropic_api_key = getenv_default "ANTHROPIC_API_KEY" "" in
  let model = getenv_default "MODEL" "" in
  let checkpoint_interval =
    match Sys.getenv_opt "CHECKPOINT_INTERVAL" with
    | Some v ->
      (try int_of_string (String.trim v)
       with Failure _ ->
         Printf.eprintf "Warning: invalid CHECKPOINT_INTERVAL %S, using 300\n" v;
         300)
    | None -> 300
  in
  {
    sprite_token;
    agent;
    claude_auth;
    anthropic_api_key;
    model;
    checkpoint_interval;
    env_file;
  }
