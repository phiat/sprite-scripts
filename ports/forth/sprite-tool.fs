#! /usr/bin/env gforth

\ sprite-tool.fs -- Forth (gforth) port of sprite-scripts
\ Subcommands: launch, push, pull, watch
\ A stack-based implementation for a stack-based language.

\ ============================================================
\ Buffers and variables
\ ============================================================

\ We need multiple independent string buffers because shell-escape,
\ sprite-args, and the callers all build strings concurrently.

4096 constant cmd-max
create cmdbuf   cmd-max allot      \ primary command builder
create escbuf   1024 allot         \ shell-escape output
create argsbuf  256 allot          \ sprite-args output
create innerbuf 1024 allot         \ inner command builder (for push etc.)
create capbuf   8192 allot         \ capture output from file reads
create tmpbuf   1024 allot         \ misc temporary strings

variable cmdbuf-len
variable escbuf-len
variable argsbuf-len
variable innerbuf-len
variable tmpbuf-len

\ ============================================================
\ Command buffer operations
\ ============================================================

: cmd-reset ( -- ) 0 cmdbuf-len ! ;

: cmd+ ( addr len -- )
  dup cmdbuf-len @ + cmd-max >= if
    ." ERROR: command buffer overflow" cr bye
  then
  dup >r  \ save len
  cmdbuf cmdbuf-len @ + swap move
  r> cmdbuf-len +! ;

: cmd@ ( -- addr len ) cmdbuf cmdbuf-len @ ;

\ ============================================================
\ Inner buffer operations (for building nested shell commands)
\ ============================================================

: inner-reset ( -- ) 0 innerbuf-len ! ;

: inner+ ( addr len -- )
  dup >r
  innerbuf innerbuf-len @ + swap move
  r> innerbuf-len +! ;

: inner@ ( -- addr len ) innerbuf innerbuf-len @ ;

\ ============================================================
\ Temp buffer operations
\ ============================================================

: tmp-reset ( -- ) 0 tmpbuf-len ! ;

: tmp+ ( addr len -- )
  dup >r
  tmpbuf tmpbuf-len @ + swap move
  r> tmpbuf-len +! ;

: tmp@ ( -- addr len ) tmpbuf tmpbuf-len @ ;

\ ============================================================
\ Number to string
\ ============================================================

: number>str ( n -- addr len )
  s>d <# #s #> ;

\ ============================================================
\ Environment variable access
\ ============================================================

: env-or-default ( default-addr default-len name-addr name-len -- addr len )
  getenv
  dup 0= if
    2drop  \ env empty, keep default on stack
  else
    2swap 2drop  \ discard default, keep env value
  then ;

\ ============================================================
\ Config variables
\ ============================================================

2variable cfg-sprite-token
2variable cfg-agent
2variable cfg-claude-auth
2variable cfg-anthropic-key
2variable cfg-model
 variable cfg-checkpoint-interval
2variable cfg-env-file

variable flag-dry-run
variable flag-checkpointing

\ Upload dirs (up to 8)
8 constant max-uploads
create upload-addrs max-uploads cells allot
create upload-lens  max-uploads cells allot
variable upload-count

: upload-dir@ ( i -- addr len )
  dup cells upload-addrs + @
  swap cells upload-lens + @ ;

: upload-dir! ( addr len i -- )
  dup cells upload-lens + rot swap !
  cells upload-addrs + ! ;

\ Sprite name and plan file
2variable the-sprite
2variable the-plan-file

\ ============================================================
\ .env file parser
\ ============================================================

create line-buf 1024 allot

: is-ws? ( char -- flag )  dup bl = swap 9 = or ;

: skip-spaces ( addr len -- addr' len' )
  begin dup 0> while over c@ is-ws? while 1 /string repeat then ;

: trim-trailing ( addr len -- addr len' )
  begin dup 0> while 2dup + 1- c@ is-ws? while 1- repeat then ;

: strip-quotes ( addr len -- addr' len' )
  dup 2 < if exit then
  over c@                        \ first char ( addr len first )
  dup [char] " = swap [char] ' = or 0= if exit then
  over c@                        \ first char again ( addr len first )
  2 pick 2 pick + 1- c@         \ last char ( addr len first last )
  <> if exit then                \ quotes don't match, drop last
  1 /string 1- ;                 \ strip both ends

: find-char ( addr len char -- offset|-1 )
  >r
  0 begin 2dup > while
    2 pick over + c@ r@ = if nip nip r> drop exit then
    1+
  repeat
  drop drop r> drop -1 ;

\ Parse one .env line -- best effort, since gforth lacks setenv.
\ We read the file mainly for structure; the actual values come from
\ the real environment (which the user sources before invoking us).
: parse-env-line ( addr len -- )
  skip-spaces trim-trailing
  dup 0= if 2drop exit then
  over c@ [char] # = if 2drop exit then
  2dup [char] = find-char
  dup -1 = if drop 2drop exit then
  \ We found '='; we don't have setenv so just discard.
  drop 2drop ;

: load-env-file ( addr len -- )
  r/o open-file if drop exit then  \ ior nonzero: drop fileid, done
  >r  \ save fileid
  begin
    line-buf 1024 r@ read-line if
      \ ior nonzero: error reading. drop len, close, exit.
      drop r> close-file drop exit
    then
    \ stack: len not-eof-flag
    swap line-buf swap parse-env-line
    \ not-eof-flag is now on top; while checks it
  while repeat
  r> close-file drop ;

\ ============================================================
\ Load all configuration from environment
\ ============================================================

: load-config ( -- )
  s" ./.env" s" ENV_FILE" env-or-default cfg-env-file 2!
  cfg-env-file 2@ load-env-file

  s" SPRITE_TOKEN" getenv
  dup 0= if 2drop s" SPRITES_TOKEN" getenv then
  cfg-sprite-token 2!

  s" opencode"     s" AGENT"       env-or-default cfg-agent 2!
  s" subscription" s" CLAUDE_AUTH"  env-or-default cfg-claude-auth 2!
  s" ANTHROPIC_API_KEY" getenv cfg-anthropic-key 2!
  s" MODEL" getenv cfg-model 2!

  s" CHECKPOINT_INTERVAL" getenv
  dup 0= if 2drop 300 else s>number? if d>s else drop 300 then then
  cfg-checkpoint-interval !

  0 flag-dry-run !
  1 flag-checkpointing !
  0 upload-count ! ;

\ ============================================================
\ Shell helpers
\ ============================================================

: shell ( addr len -- ) system ;

\ Shell-escape into escbuf: wraps in single quotes, escapes embedded quotes.
\ For embedded single quotes, we close the quote, add backslash-quote, reopen.
\ That is the 4-char sequence: ' \ ' '   (written as individual chars below)
\ Returns addr len pointing into escbuf.
: shell-escape ( addr len -- addr2 len2 )
  0 escbuf-len !
  [char] ' escbuf c! 1 escbuf-len !
  0 ?do
    dup i + c@
    dup [char] ' = if
      drop
      \ Append the 4 chars: ' \ ' '
      [char] ' escbuf escbuf-len @ + c! 1 escbuf-len +!
      [char] \ escbuf escbuf-len @ + c! 1 escbuf-len +!
      [char] ' escbuf escbuf-len @ + c! 1 escbuf-len +!
      [char] ' escbuf escbuf-len @ + c! 1 escbuf-len +!
    else
      escbuf escbuf-len @ + c!
      1 escbuf-len +!
    then
  loop
  drop
  [char] ' escbuf escbuf-len @ + c!
  1 escbuf-len +!
  escbuf escbuf-len @ ;

\ Append shell-escaped string to cmdbuf.
\ Because shell-escape uses escbuf, we must copy to cmdbuf immediately.
: cmd+esc ( addr len -- )
  shell-escape cmd+ ;

\ Build sprite-args into argsbuf: "-s 'sprite-name'" or ""
\ Returns addr len pointing into argsbuf.
: sprite-args ( -- addr len )
  the-sprite 2@ dup 0= if 2drop argsbuf 0 exit then  \ empty sprite -> empty string
  \ We can't use shell-escape directly because caller may be mid-escape.
  \ So we manually build into argsbuf.
  0 argsbuf-len !
  s" -s " argsbuf swap move 3 argsbuf-len !
  \ Inline shell-escape into argsbuf (same algorithm as shell-escape)
  [char] ' argsbuf argsbuf-len @ + c! 1 argsbuf-len +!
  the-sprite 2@ 0 ?do
    dup i + c@
    dup [char] ' = if
      drop
      [char] ' argsbuf argsbuf-len @ + c! 1 argsbuf-len +!
      [char] \ argsbuf argsbuf-len @ + c! 1 argsbuf-len +!
      [char] ' argsbuf argsbuf-len @ + c! 1 argsbuf-len +!
      [char] ' argsbuf argsbuf-len @ + c! 1 argsbuf-len +!
    else
      argsbuf argsbuf-len @ + c! 1 argsbuf-len +!
    then
  loop drop
  [char] ' argsbuf argsbuf-len @ + c! 1 argsbuf-len +!
  argsbuf argsbuf-len @ ;

\ ============================================================
\ Sprite execution: run command inside the sprite
\ ============================================================

\ Execute command on sprite (uses the-sprite via sprite-args)
: sprite-exec { ca cl | -- }
  flag-dry-run @ if
    ." [dry-run] sprite exec " sprite-args type ."  bash -c " [char] " emit
    ca cl type [char] " emit cr exit
  then
  cmd-reset
  s" sprite exec " cmd+
  sprite-args cmd+
  s"  bash -c " cmd+
  ca cl cmd+esc
  cmd@ shell ;

\ Execute on sprite and capture output via temp file
: sprite-exec-capture { ca cl | -- addr len }
  flag-dry-run @ if
    ." [dry-run] (capture) " ca cl type cr
    s" " exit
  then
  cmd-reset
  s" sprite exec " cmd+
  sprite-args cmd+
  s"  bash -c " cmd+
  ca cl cmd+esc
  s"  2>/dev/null > /tmp/.sprite-forth-cap" cmd+
  cmd@ shell
  \ Read capture file
  s" /tmp/.sprite-forth-cap" r/o open-file if
    drop s" " exit
  then
  >r
  capbuf 8192 r@ read-file if
    r> close-file drop drop s" " exit
  then
  r> close-file drop
  \ Trim trailing newline
  dup 0> if capbuf over + 1- c@ 10 = if 1- then then
  capbuf swap ;

\ ============================================================
\ File and directory operations
\ ============================================================

: file-exists? ( addr len -- flag )
  r/o open-file if drop false else close-file drop true then ;

: is-directory? ( addr len -- flag )
  cmd-reset s" test -d " cmd+ cmd+esc
  cmd@ system $? 0= ;

\ Push a local file to the sprite
: sprite-push-file { sa sl da dl | -- }
  flag-dry-run @ if
    ." [dry-run] push " sa sl type ."  -> sprite:" da dl type cr exit
  then
  \ Build inner shell command: mkdir -p '$(dirname DEST)' && cat > 'DEST'
  inner-reset
  s" mkdir -p '$(dirname " inner+
  da dl inner+
  s" )' && cat > '" inner+
  da dl inner+
  s" '" inner+
  \ Build outer: cat 'SRC' | sprite exec ARGS bash -c 'INNER'
  cmd-reset
  s" cat " cmd+
  sa sl cmd+esc
  s"  | sprite exec " cmd+
  sprite-args cmd+
  s"  bash -c " cmd+
  inner@ cmd+esc
  cmd@ shell ;

\ Push a local directory to the sprite (preserves basename)
: sprite-push-dir { sa sl da dl | -- }
  flag-dry-run @ if
    ." [dry-run] push dir " sa sl type ."  -> sprite:" da dl type cr exit
  then
  inner-reset
  s" mkdir -p '" inner+
  da dl inner+
  s" ' && tar xzf - -C '$(dirname " inner+
  da dl inner+
  s" )'" inner+
  cmd-reset
  s" tar czf - -C $(dirname " cmd+
  sa sl cmd+esc
  s" ) $(basename " cmd+
  sa sl cmd+esc
  s" ) | sprite exec " cmd+
  sprite-args cmd+
  s"  bash -c " cmd+
  inner@ cmd+esc
  cmd@ shell ;

\ Push directory with --strip-components=1 (for push subcommand)
: sprite-push-dir-strip { sa sl da dl | -- }
  flag-dry-run @ if
    ." [dry-run] push dir " sa sl type ."  -> sprite:" da dl type cr exit
  then
  inner-reset
  s" mkdir -p '" inner+
  da dl inner+
  s" ' && tar xzf - -C '" inner+
  da dl inner+
  s" ' --strip-components=1" inner+
  cmd-reset
  s" tar czf - -C $(dirname " cmd+
  sa sl cmd+esc
  s" ) $(basename " cmd+
  sa sl cmd+esc
  s" ) | sprite exec " cmd+
  sprite-args cmd+
  s"  bash -c " cmd+
  inner@ cmd+esc
  cmd@ shell ;

\ ============================================================
\ Usage messages
\ ============================================================

: usage-main ( -- )
  ." Usage: sprite-tool <command> [args...]" cr cr
  ." Commands:" cr
  ."   launch    Create and configure a sprite with coding agent, git, beads" cr
  ."   push      Push local file or directory to a sprite" cr
  ."   pull      Pull file or directory from a sprite" cr
  ."   watch     Poll a sprite's beads tracker task for progress" cr cr
  ." Run 'sprite-tool <command> --help' for command-specific help." cr
  1 (bye) ;

: usage-launch ( -- )
  ." Usage: sprite-tool launch [options] <sprite-name> [plan-file]" cr cr
  ." Options:" cr
  ."   --dry-run              Show what would happen without executing" cr
  ."   --no-checkpoint        Disable auto-checkpointing" cr
  ."   --upload <dir>         Upload a local directory to /home/sprite/<dirname>" cr
  ."                          (repeatable)" cr cr
  ." Environment variables:" cr
  ."   ENV_FILE               Path to .env file (default: ./.env)" cr
  ."   SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN)" cr
  ."   AGENT                  'opencode' (default) or 'claude'" cr
  ."   CLAUDE_AUTH            'subscription' (default) or 'apikey'" cr
  ."   MODEL                  Model override" cr
  ."   CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300)" cr cr
  ." Examples:" cr
  ."   sprite-tool launch my-project plan.md" cr
  ."   sprite-tool launch --upload ./data my-project plan.md" cr
  ."   sprite-tool launch --dry-run my-project plan.md" cr
  1 (bye) ;

: usage-push ( -- )
  ." Usage: sprite-tool push <local-path> <remote-path> [sprite-name]" cr cr
  ." Examples:" cr
  ."   sprite-tool push ./file.txt /home/sprite/file.txt" cr
  ."   sprite-tool push ./mydir /home/sprite/mydir" cr
  ."   sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk" cr
  1 (bye) ;

: usage-pull ( -- )
  ." Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]" cr cr
  ." Examples:" cr
  ."   sprite-tool pull /home/sprite/file.txt ./file.txt" cr
  ."   sprite-tool pull /home/sprite/mydir ./mydir" cr
  ."   sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk" cr
  1 (bye) ;

: usage-watch ( -- )
  ." Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]" cr cr
  ." Arguments:" cr
  ."   sprite-name     Name of the sprite to watch" cr
  ."   task-id         Beads task ID (default: auto-detect)" cr
  ."   poll-interval   Seconds between polls (default: 30)" cr cr
  ." Examples:" cr
  ."   sprite-tool watch ember-red-hawk" cr
  ."   sprite-tool watch ember-red-hawk CRM-1" cr
  ."   sprite-tool watch ember-red-hawk CRM-1 60" cr
  1 (bye) ;

\ ============================================================
\ Argument collection from command line
\ ============================================================

16 constant max-args
create arg-addrs max-args cells allot
create arg-lens  max-args cells allot
variable arg-count

: store-arg ( addr len i -- )
  dup cells arg-lens + rot swap !
  cells arg-addrs + ! ;

: fetch-arg ( i -- addr len )
  dup cells arg-addrs + @
  swap cells arg-lens + @ ;

: collect-args ( -- )
  0 arg-count !
  begin
    next-arg dup 0<> while
    arg-count @ max-args < while
    arg-count @ store-arg
    1 arg-count +!
  repeat then
  2drop ;

\ Check if arg at index is --help or -h
: arg-is-help? ( i -- flag )
  fetch-arg
  2dup s" --help" str= if 2drop true exit then
  s" -h" str= ;

\ ============================================================
\ LAUNCH subcommand
\ ============================================================

: do-launch ( -- )
  load-config

  0 { idx }

  \ Parse option flags using a flag to control the loop
  true { parsing }
  begin idx arg-count @ < parsing and while
    idx fetch-arg
    over c@ [char] - <> if
      \ Not a flag, stop option parsing
      2drop false to parsing
    else 2dup s" --dry-run" str= if
      2drop 1 flag-dry-run ! idx 1+ to idx
    else 2dup s" --no-checkpoint" str= if
      2drop 0 flag-checkpointing ! idx 1+ to idx
    else 2dup s" --upload" str= if
      2drop idx 1+ to idx
      idx arg-count @ >= if
        ." ERROR: --upload requires a directory argument" cr usage-launch
      then
      idx fetch-arg upload-count @ upload-dir!
      1 upload-count +!
      idx 1+ to idx
    else 2dup s" --help" str= if
      2drop usage-launch
    else 2dup s" -h" str= if
      2drop usage-launch
    else
      \ Unknown flag -- treat as positional arg
      2drop false to parsing
    then then then then then then
  repeat

  \ Remaining: sprite-name [plan-file]
  idx arg-count @ >= if
    ." ERROR: sprite-name is required" cr usage-launch
  then
  idx fetch-arg the-sprite 2!
  idx 1+ to idx
  idx arg-count @ < if
    idx fetch-arg the-plan-file 2!
  else
    s" " the-plan-file 2!
  then

  \ ==== Step 1: Check/install sprite CLI ====
  s" command -v sprite >/dev/null 2>&1" system $? 0<> if
    flag-dry-run @ if
      ." [dry-run] Would install sprite CLI" cr
    else
      ." Installing sprite CLI..." cr
      s" curl -fsSL https://sprites.dev/install.sh | sh" shell
    then
  then

  \ ==== Step 2: Auth ====
  cfg-sprite-token 2@ dup 0<> if
    ." Authenticating sprite with token..." cr
    flag-dry-run @ 0= if
      cmd-reset
      s" sprite auth setup --token " cmd+
      cfg-sprite-token 2@ cmd+esc
      cmd@ shell
    then
  else
    2drop
    ." No SPRITE_TOKEN set. Running interactive login..." cr
    flag-dry-run @ 0= if s" sprite login" shell then
  then

  \ ==== Step 3: Create sprite ====
  flag-dry-run @ if
    ." [dry-run] Would create or reuse sprite '" the-sprite 2@ type ." '" cr
  else
    cmd-reset
    s" sprite ls 2>/dev/null | grep -qw " cmd+
    the-sprite 2@ cmd+esc
    cmd@ system $? 0= if
      ." Sprite '" the-sprite 2@ type ." ' already exists, using it." cr
    else
      ." Creating sprite: " the-sprite 2@ type cr
      cmd-reset
      s" sprite create -skip-console " cmd+
      the-sprite 2@ cmd+esc
      cmd@ shell
    then
  then

  \ ==== Step 4: Push .env ====
  cfg-env-file 2@ file-exists? if
    ." Pushing " cfg-env-file 2@ type ." ..." cr
    cfg-env-file 2@ s" /home/sprite/.env" sprite-push-file
  then

  \ ==== Step 5: Push plan file ====
  the-plan-file 2@ dup 0<> if
    2dup file-exists? if
      ." Pushing " 2dup type ." ..." cr
      s" /home/sprite/plan.md" sprite-push-file
    else
      2drop
    then
  else
    2drop
  then

  \ ==== Step 6: Upload directories ====
  upload-count @ 0 ?do
    i upload-dir@ 2dup is-directory? if
      \ Get basename via temp file
      cmd-reset
      s" basename " cmd+ 2dup cmd+esc
      s"  > /tmp/.sprite-forth-bn" cmd+
      cmd@ shell
      s" /tmp/.sprite-forth-bn" r/o open-file 0= if
        >r capbuf 256 r@ read-file drop
        r> close-file drop
        \ trim newline
        dup 0> if capbuf over + 1- c@ 10 = if 1- then then
        capbuf swap
      else
        drop s" unknown"
      then
      \ Build dest: /home/sprite/<basename>
      tmp-reset
      s" /home/sprite/" tmp+
      tmp+  \ basename
      ." Uploading directory: " 2dup type ."  -> " tmp@ type cr
      tmp@ sprite-push-dir
    else
      ." WARNING: --upload dir '" type ." ' not found, skipping." cr
    then
  loop

  \ ==== Step 7: Git + beads ====
  ." Initializing git..." cr
  s" cd /home/sprite && git init -b main 2>/dev/null || true" sprite-exec

  ." Installing beads..." cr
  s" curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash"
  sprite-exec

  \ ==== Step 8: Install and auth coding agent ====
  cfg-agent 2@ s" claude" str= if
    ." Setting up claude..." cr
    s" command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code"
    sprite-exec

    cfg-claude-auth 2@ s" subscription" str= if
      \ Build local credentials path
      tmp-reset
      s" HOME" getenv tmp+
      s" /.claude/.credentials.json" tmp+
      tmp@ 2dup file-exists? if
        ." Copying claude subscription credentials..." cr
        s" /home/sprite/.claude/.credentials.json" sprite-push-file
        s" chmod 600 ~/.claude/.credentials.json" sprite-exec
      else
        2drop
        ." ERROR: ~/.claude/.credentials.json not found" cr
        ." Run 'claude' locally first to authenticate, then re-run." cr
        1 (bye)
      then
    else cfg-claude-auth 2@ s" apikey" str= cfg-anthropic-key 2@ nip 0<> and if
      ." Setting ANTHROPIC_API_KEY in sprite..." cr
      inner-reset
      s" grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=" inner+
      cfg-anthropic-key 2@ inner+
      s" ' >> ~/.bashrc" inner+
      inner@ sprite-exec
    else
      ." ERROR: No valid claude auth configured" cr
      ." Set CLAUDE_AUTH=subscription or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY" cr
      1 (bye)
    then then

  else cfg-agent 2@ s" opencode" str= if
    ." Setting up opencode..." cr
    s" [ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash"
    sprite-exec
    s" grep -q 'source.*\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc"
    sprite-exec
  else
    ." ERROR: Unknown AGENT '" cfg-agent 2@ type ." '. Use 'claude' or 'opencode'." cr
    1 (bye)
  then then

  \ ==== Step 9: Ready banner and launch ====
  cr
  ." ==========================================" cr
  ." Sprite '" the-sprite 2@ type ." ' is ready!" cr
  ." Agent: " cfg-agent 2@ type
  cfg-model 2@ dup 0<> if
    ."  (model: " type ." )"
  else 2drop then
  cr
  flag-checkpointing @ if
    ." Checkpointing: every " cfg-checkpoint-interval @ . ." s" cr
  then
  ." ==========================================" cr

  flag-dry-run @ if
    cr ." [dry-run] Would launch " cfg-agent 2@ type ."  with plan. No changes were made." cr
    0 (bye)
  then

  the-plan-file 2@ dup 0<> if
    2drop
    \ NOTE: Forth has no background threads. We do a final checkpoint only.
    flag-checkpointing @ if
      ." (Checkpointing: final checkpoint only -- no background threads in Forth)" cr
    then

    cfg-agent 2@ s" claude" str= if
      ." Launching claude with plan..." cr
      inner-reset
      s" cd /home/sprite && claude " inner+
      cfg-model 2@ dup 0<> if
        s" --model " inner+ inner+
        s"  " inner+
      else 2drop then
      s" -p 'read plan.md and complete the plan please'" inner+
      inner@ sprite-exec
    else
      ." Launching opencode with plan..." cr
      inner-reset
      s" set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m " inner+
      cfg-model 2@ dup 0<> if
        inner+
      else
        2drop s" opencode/big-pickle" inner+
      then
      s"  'read plan.md and complete the plan please'" inner+
      inner@ sprite-exec
    then

    \ Final checkpoint
    flag-checkpointing @ if
      ." Creating final checkpoint..." cr
      cmd-reset
      s" sprite checkpoint create -s " cmd+
      the-sprite 2@ cmd+esc
      s"  2>/dev/null" cmd+
      cmd@ system $? 0= if
        ." Final checkpoint saved." cr
      else
        ." Final checkpoint failed (non-fatal)." cr
      then
    then
  else
    2drop
    ." Opening console..." cr
    cmd-reset
    s" sprite console -s " cmd+
    the-sprite 2@ cmd+esc
    cmd@ shell
  then

  0 (bye) ;

\ ============================================================
\ PUSH subcommand
\ ============================================================

: do-push ( -- )
  arg-count @ 2 < if usage-push then
  0 arg-is-help? if usage-push then

  0 fetch-arg { la ll }
  1 fetch-arg { ra rl }

  arg-count @ 3 >= if
    2 fetch-arg the-sprite 2!
  else
    s" " the-sprite 2!
  then

  \ Check local path exists
  la ll file-exists? la ll is-directory? or 0= if
    ." Error: " la ll type ."  does not exist" cr
    1 (bye)
  then

  la ll is-directory? if
    ." Pushing directory: " la ll type ."  -> " ra rl type cr
    la ll ra rl sprite-push-dir-strip
  else
    ." Pushing file: " la ll type ."  -> " ra rl type cr
    la ll ra rl sprite-push-file
  then

  ." Done." cr
  0 (bye) ;

\ ============================================================
\ PULL subcommand
\ ============================================================

: do-pull ( -- )
  arg-count @ 2 < if usage-pull then
  0 arg-is-help? if usage-pull then

  0 fetch-arg { ra rl }
  1 fetch-arg { la ll }

  arg-count @ 3 >= if
    2 fetch-arg the-sprite 2!
  else
    s" " the-sprite 2!
  then

  \ Check if remote path is directory
  inner-reset
  s" [ -d '" inner+
  ra rl inner+
  s" ' ] && echo dir || echo file" inner+
  inner@ sprite-exec-capture

  2dup s" dir" str= if
    2drop
    ." Pulling directory: " ra rl type ."  -> " la ll type cr
    cmd-reset
    s" mkdir -p " cmd+ la ll cmd+esc
    cmd@ shell

    cmd-reset
    s" sprite exec " cmd+
    sprite-args cmd+
    s"  tar czf - -C " cmd+
    ra rl cmd+esc
    s"  . | tar xzf - -C " cmd+
    la ll cmd+esc
    cmd@ shell
  else
    2drop
    ." Pulling file: " ra rl type ."  -> " la ll type cr
    cmd-reset
    s" mkdir -p $(dirname " cmd+
    la ll cmd+esc
    s" )" cmd+
    cmd@ shell

    cmd-reset
    s" sprite exec " cmd+
    sprite-args cmd+
    s"  cat " cmd+
    ra rl cmd+esc
    s"  > " cmd+
    la ll cmd+esc
    cmd@ shell
  then

  ." Done." cr
  0 (bye) ;

\ ============================================================
\ WATCH subcommand
\ ============================================================

: do-watch ( -- )
  arg-count @ 1 < if usage-watch then
  0 arg-is-help? if usage-watch then

  0 fetch-arg the-sprite 2!

  s" " { ta tl }
  arg-count @ 2 >= if
    1 fetch-arg to tl to ta
  then

  30 { poll }
  arg-count @ 3 >= if
    2 fetch-arg s>number? if d>s to poll else drop then
  then

  \ Auto-detect task if not specified
  tl 0= if
    ." Detecting tracker task..." cr
    s" cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'"
    sprite-exec-capture
    dup 0= if
      2drop
      ." No critical task found. Falling back to first open task..." cr
      s" cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'"
      sprite-exec-capture
    then
    dup 0= if
      2drop
      ." ERROR: No beads tasks found on sprite '" the-sprite 2@ type ." '" cr
      ." Specify a task ID: sprite-tool watch " the-sprite 2@ type ."  <task-id>" cr
      1 (bye)
    then
    to tl to ta
    ." Tracking task: " ta tl type cr
  then

  ." Watching sprite '" the-sprite 2@ type ." ' task '" ta tl type
  ." ' (every " poll . ." s)" cr
  ." Press Ctrl+C to stop" cr cr

  begin
    \ Clear screen
    27 emit ." [2J" 27 emit ." [H"

    \ Timestamp
    s" date +%H:%M:%S > /tmp/.sprite-forth-ts" shell
    s" /tmp/.sprite-forth-ts" r/o open-file 0= if
      >r capbuf 64 r@ read-file drop
      r> close-file drop
      dup 0> if capbuf over + 1- c@ 10 = if 1- then then
      capbuf swap
    else
      drop s" ??:??:??"
    then

    ." === sprite-watch: " the-sprite 2@ type ."  / " ta tl type ."  === "
    type ."  ===" cr cr

    \ Task status
    inner-reset
    s" cd /home/sprite && bd show " inner+
    ta tl inner+
    s"  2>/dev/null" inner+
    inner@ sprite-exec-capture
    dup 0<> if type cr else 2drop ." (could not read task)" cr then
    cr

    \ Recent comments
    ." --- Recent updates ---" cr
    inner-reset
    s" cd /home/sprite && bd comments " inner+
    ta tl inner+
    s"  2>/dev/null | tail -8" inner+
    inner@ sprite-exec-capture
    dup 0<> if type cr else 2drop ." (no comments)" cr then
    cr

    \ Check completion
    inner-reset
    s" cd /home/sprite && bd show " inner+
    ta tl inner+
    s"  2>/dev/null | grep -qi 'status.*\(closed\|done\|completed\)' && echo DONE || echo OPEN" inner+
    inner@ sprite-exec-capture
    s" DONE" str= if
      ." ==========================================" cr
      ." PROJECT COMPLETE" cr
      ." ==========================================" cr
      0 (bye)
    then

    \ Sleep
    cmd-reset
    s" sleep " cmd+
    poll number>str cmd+
    cmd@ shell
  again ;

\ ============================================================
\ Main dispatch
\ ============================================================

: main ( -- )
  next-arg
  dup 0= if 2drop usage-main then

  \ Skip gforth's "--" separator if present
  2dup s" --" str= if 2drop next-arg then
  dup 0= if 2drop usage-main then

  2dup s" launch" str= if 2drop collect-args do-launch exit then
  2dup s" push"   str= if 2drop collect-args do-push   exit then
  2dup s" pull"   str= if 2drop collect-args do-pull   exit then
  2dup s" watch"  str= if 2drop collect-args do-watch  exit then
  2dup s" --help" str= if 2drop usage-main then
  2dup s" -h"     str= if 2drop usage-main then

  ." Unknown command: " type cr cr
  usage-main ;

main
bye
