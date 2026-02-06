--  launch.adb -- sprite-tool launch subcommand implementation
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Environment_Variables;
with Config;
with Sprite_Exec;

package body Launch is

   --  Maximum number of --upload directories
   Max_Upload_Dirs : constant := 64;

   ---------------------------------------------------------------------------
   --  Print_Usage
   ---------------------------------------------------------------------------
   procedure Print_Usage is
   begin
      Put_Line ("Usage: sprite-tool launch [options] <sprite-name> [plan-file]");
      New_Line;
      Put_Line ("Options:");
      Put_Line ("  --dry-run              Show what would happen without executing");
      Put_Line ("  --no-checkpoint        Disable auto-checkpointing");
      Put_Line ("  --upload <dir>         Upload a local directory to /home/sprite/<dirname>");
      Put_Line ("                         (repeatable: --upload ./data --upload ./tests)");
      New_Line;
      Put_Line ("Environment variables:");
      Put_Line ("  ENV_FILE               Path to .env file (default: ./.env)");
      Put_Line ("  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)");
      Put_Line ("  AGENT                  ""opencode"" (default) or ""claude""");
      Put_Line ("  CLAUDE_AUTH            ""subscription"" (default) or ""apikey""");
      Put_Line ("  MODEL                  Model override");
      Put_Line ("  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)");
      New_Line;
      Put_Line ("Examples:");
      Put_Line ("  sprite-tool launch my-project plan.md");
      Put_Line ("  sprite-tool launch --upload ./data my-project plan.md");
      Put_Line ("  sprite-tool launch --dry-run my-project plan.md");
   end Print_Usage;

   ---------------------------------------------------------------------------
   --  Basename -- extract filename from a path
   ---------------------------------------------------------------------------
   function Basename (Path : in String) return String is
   begin
      for I in reverse Path'Range loop
         if Path (I) = '/' then
            return Path (I + 1 .. Path'Last);
         end if;
      end loop;
      return Path;
   end Basename;

   ---------------------------------------------------------------------------
   --  Execute
   ---------------------------------------------------------------------------
   procedure Execute (Start_Arg : in Positive) is
      Dry_Run       : Boolean := False;
      Checkpointing : Boolean := True;

      type Dir_Array is array (1 .. Max_Upload_Dirs) of Unbounded_String;
      Upload_Dirs   : Dir_Array := (others => Null_Unbounded_String);
      Upload_Count  : Natural := 0;

      Sprite_Name   : Unbounded_String := Null_Unbounded_String;
      Plan_File     : Unbounded_String := Null_Unbounded_String;

      I             : Positive := Start_Arg;
      Dummy         : Integer;
      Cfg           : Config.App_Config;
   begin
      --  Parse flags and positional arguments
      while I <= Argument_Count loop
         declare
            Arg : constant String := Argument (I);
         begin
            if Arg = "--dry-run" then
               Dry_Run := True;
               I := I + 1;
            elsif Arg = "--no-checkpoint" then
               Checkpointing := False;
               I := I + 1;
            elsif Arg = "--upload" then
               I := I + 1;
               if I > Argument_Count then
                  Put_Line (Standard_Error, "ERROR: --upload requires a directory argument");
                  Print_Usage;
                  Set_Exit_Status (Failure);
                  return;
               end if;
               Upload_Count := Upload_Count + 1;
               Upload_Dirs (Upload_Count) := To_Unbounded_String (Argument (I));
               I := I + 1;
            elsif Arg = "--help" or Arg = "-h" then
               Print_Usage;
               return;
            elsif Arg'Length > 2 and then Arg (Arg'First .. Arg'First + 1) = "--" then
               Put_Line (Standard_Error, "ERROR: Unknown option: " & Arg);
               Print_Usage;
               Set_Exit_Status (Failure);
               return;
            elsif Sprite_Name = Null_Unbounded_String then
               Sprite_Name := To_Unbounded_String (Arg);
               I := I + 1;
            elsif Plan_File = Null_Unbounded_String then
               Plan_File := To_Unbounded_String (Arg);
               I := I + 1;
            else
               Put_Line (Standard_Error, "ERROR: Unexpected argument: " & Arg);
               Print_Usage;
               Set_Exit_Status (Failure);
               return;
            end if;
         end;
      end loop;

      if Sprite_Name = Null_Unbounded_String then
         Put_Line (Standard_Error, "ERROR: sprite-name is required");
         Print_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      --  Load configuration
      Cfg := Config.Load;

      declare
         SN      : constant String := To_String (Sprite_Name);
         PF      : constant String := To_String (Plan_File);
         Token   : constant String := To_String (Cfg.Sprite_Token);
         Agent   : constant String := To_String (Cfg.Agent);
         Auth    : constant String := To_String (Cfg.Claude_Auth);
         API_Key : constant String := To_String (Cfg.Anthropic_API_Key);
         Model_S : constant String := To_String (Cfg.Model);
         Env_F   : constant String := To_String (Cfg.Env_File);
      begin

         -----------------------------------------------------------------
         --  Step 1: Check/install sprite CLI
         -----------------------------------------------------------------
         Put_Line ("==> Checking sprite CLI...");
         if not Sprite_Exec.Command_Exists ("sprite") then
            if Dry_Run then
               Put_Line ("  [dry-run] Would install sprite CLI");
            else
               Put_Line ("Installing sprite CLI...");
               Sprite_Exec.Run_Shell_Or_Die
                 ("curl -fsSL https://sprites.dev/install.sh | sh");
               --  Add to PATH
               if Ada.Environment_Variables.Exists ("HOME") then
                  declare
                     Home    : constant String :=
                       Ada.Environment_Variables.Value ("HOME");
                     Cur_Path : constant String :=
                       (if Ada.Environment_Variables.Exists ("PATH")
                        then Ada.Environment_Variables.Value ("PATH")
                        else "");
                  begin
                     Ada.Environment_Variables.Set
                       ("PATH", Home & "/.local/bin:" & Cur_Path);
                  end;
               end if;
            end if;
         end if;

         -----------------------------------------------------------------
         --  Step 2: Authenticate
         -----------------------------------------------------------------
         Put_Line ("==> Authenticating...");
         if Token'Length > 0 then
            Put_Line ("Authenticating sprite with token...");
            if not Dry_Run then
               Dummy := Sprite_Exec.Sprite_Run
                 ("auth setup --token '" & Token & "'");
            end if;
         else
            Put_Line ("No SPRITE_TOKEN set. Running interactive login...");
            if not Dry_Run then
               Dummy := Sprite_Exec.Sprite_Run ("login");
            end if;
         end if;

         -----------------------------------------------------------------
         --  Step 3: Create sprite (or reuse existing)
         -----------------------------------------------------------------
         Put_Line ("==> Checking for existing sprite " & SN & "...");
         if Dry_Run then
            Put_Line ("  [dry-run] Would create or reuse sprite '" & SN & "'");
         else
            declare
               Status : Integer;
               Output : constant Unbounded_String :=
                 Sprite_Exec.Sprite_Run_Capture ("ls", Status);
               Out_Str : constant String := To_String (Output);
            begin
               --  Check if sprite name appears in ls output
               declare
                  Found : Boolean := False;
               begin
                  --  Simple substring search for the sprite name
                  for J in Out_Str'First .. Out_Str'Last - SN'Length + 1 loop
                     if Out_Str (J .. J + SN'Length - 1) = SN then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if Found then
                     Put_Line ("Sprite '" & SN & "' already exists, using it.");
                  else
                     Put_Line ("Creating sprite: " & SN);
                     Dummy := Sprite_Exec.Sprite_Run
                       ("create -skip-console " & SN);
                  end if;
               end;
            end;
         end if;

         -----------------------------------------------------------------
         --  Step 4: Push .env to sprite
         -----------------------------------------------------------------
         if Ada.Directories.Exists (Env_F) then
            Put_Line ("==> Pushing " & Env_F & " to /home/sprite/.env...");
            Sprite_Exec.Push_File (SN, Env_F, "/home/sprite/.env", Dry_Run);
         end if;

         -----------------------------------------------------------------
         --  Step 5: Push plan file
         -----------------------------------------------------------------
         if PF'Length > 0 then
            if Ada.Directories.Exists (PF) then
               Put_Line ("==> Pushing plan file " & PF
                         & " to /home/sprite/plan.md...");
               Sprite_Exec.Push_File (SN, PF, "/home/sprite/plan.md", Dry_Run);
            else
               Put_Line (Standard_Error,
                         "ERROR: Plan file not found: " & PF);
               Set_Exit_Status (Failure);
               return;
            end if;
         end if;

         -----------------------------------------------------------------
         --  Step 6: Upload directories
         -----------------------------------------------------------------
         for J in 1 .. Upload_Count loop
            declare
               Dir_Path : constant String := To_String (Upload_Dirs (J));
               Dir_Base : constant String := Basename (Dir_Path);
               Remote   : constant String := "/home/sprite/" & Dir_Base;
            begin
               if Ada.Directories.Exists (Dir_Path) then
                  Put_Line ("==> Uploading " & Dir_Path & " -> " & Remote & "...");
                  Sprite_Exec.Push_Dir (SN, Dir_Path, Remote, Dry_Run);
               else
                  Put_Line ("WARNING: --upload dir '" & Dir_Path
                            & "' not found, skipping.");
               end if;
            end;
         end loop;

         -----------------------------------------------------------------
         --  Step 7: Git init + beads
         -----------------------------------------------------------------
         Put_Line ("==> Initializing git on sprite...");
         Dummy := Sprite_Exec.Sprite_Exec_Cmd
           (SN,
            "cd /home/sprite && git init -b main 2>/dev/null || true",
            Dry_Run);

         Put_Line ("==> Installing beads...");
         Dummy := Sprite_Exec.Sprite_Exec_Cmd
           (SN,
            "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash",
            Dry_Run);

         -----------------------------------------------------------------
         --  Step 8: Install and auth coding agent
         -----------------------------------------------------------------
         Put_Line ("==> Setting up agent: " & Agent & "...");

         if Agent = "claude" then
            --  Install claude CLI
            Dummy := Sprite_Exec.Sprite_Exec_Cmd
              (SN,
               "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code",
               Dry_Run);

            --  Authenticate
            if Auth = "subscription" then
               declare
                  Home : constant String :=
                    (if Ada.Environment_Variables.Exists ("HOME")
                     then Ada.Environment_Variables.Value ("HOME")
                     else "");
                  Cred_Path : constant String :=
                    Home & "/.claude/.credentials.json";
               begin
                  if Home'Length > 0
                    and then Ada.Directories.Exists (Cred_Path)
                  then
                     Put_Line ("Copying claude subscription credentials...");
                     Sprite_Exec.Push_File
                       (SN, Cred_Path,
                        "/home/sprite/.claude/.credentials.json", Dry_Run);
                     Dummy := Sprite_Exec.Sprite_Exec_Cmd
                       (SN, "chmod 600 ~/.claude/.credentials.json", Dry_Run);
                  else
                     Put_Line (Standard_Error,
                               "ERROR: ~/.claude/.credentials.json not found");
                     Put_Line (Standard_Error,
                               "Run 'claude' locally first to authenticate, then re-run.");
                     Set_Exit_Status (Failure);
                     return;
                  end if;
               end;
            elsif Auth = "apikey" and then API_Key'Length > 0 then
               Put_Line ("Setting ANTHROPIC_API_KEY in sprite...");
               Dummy := Sprite_Exec.Sprite_Exec_Cmd
                 (SN,
                  "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=""" & API_Key & """' >> ~/.bashrc",
                  Dry_Run);
            else
               Put_Line (Standard_Error,
                         "ERROR: No valid claude auth configured");
               Put_Line (Standard_Error,
                         "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY");
               Set_Exit_Status (Failure);
               return;
            end if;

         elsif Agent = "opencode" then
            --  Install opencode
            Dummy := Sprite_Exec.Sprite_Exec_Cmd
              (SN,
               "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash",
               Dry_Run);
            --  Source .env from bashrc
            Dummy := Sprite_Exec.Sprite_Exec_Cmd
              (SN,
               "grep -q 'source.*\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc",
               Dry_Run);

         else
            Put_Line (Standard_Error,
                      "ERROR: Unknown AGENT '" & Agent
                      & "'. Use 'claude' or 'opencode'.");
            Set_Exit_Status (Failure);
            return;
         end if;

         -----------------------------------------------------------------
         --  Step 9: Summary + Launch
         -----------------------------------------------------------------
         New_Line;
         Put_Line ("==========================================");
         Put_Line ("Sprite '" & SN & "' is ready!");
         if Model_S'Length > 0 then
            Put_Line ("Agent: " & Agent & " (model: " & Model_S & ")");
         else
            Put_Line ("Agent: " & Agent);
         end if;
         if Checkpointing then
            Put_Line ("Checkpointing: every"
                      & Natural'Image (Cfg.Checkpoint_Interval) & "s");
         end if;
         Put_Line ("==========================================");

         if Dry_Run then
            New_Line;
            Put_Line ("[dry-run] Would launch " & Agent
                      & " with plan. No changes were made.");
            return;
         end if;

         if PF'Length > 0 then
            --  Start background checkpoint task
            if Checkpointing then
               Put_Line ("Auto-checkpointing every"
                         & Natural'Image (Cfg.Checkpoint_Interval) & "s");
               --  Launch checkpoint loop in background via shell
               Dummy := Sprite_Exec.Run_Shell
                 ("( while true; do sleep "
                  & Natural'Image (Cfg.Checkpoint_Interval)
                  & "; echo '[checkpoint] Creating checkpoint...'; "
                  & "sprite checkpoint create -s '" & SN
                  & "' 2>/dev/null && echo '[checkpoint] Done.' || echo '[checkpoint] Failed (non-fatal).'; "
                  & "done ) &");
            end if;

            Put_Line ("Launching " & Agent & " with plan...");

            if Agent = "claude" then
               declare
                  Model_Flag : constant String :=
                    (if Model_S'Length > 0
                     then " --model " & Model_S
                     else "");
                  Agent_Cmd : constant String :=
                    "cd /home/sprite && claude" & Model_Flag
                    & " -p 'read plan.md and complete the plan please'";
               begin
                  Dummy := Sprite_Exec.Sprite_Exec_Cmd (SN, Agent_Cmd);
               end;

            elsif Agent = "opencode" then
               declare
                  OC_Model : constant String :=
                    (if Model_S'Length > 0
                     then Model_S
                     else "opencode/big-pickle");
                  Agent_Cmd : constant String :=
                    "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m "
                    & OC_Model & " 'read plan.md and complete the plan please'";
               begin
                  Dummy := Sprite_Exec.Sprite_Exec_Cmd (SN, Agent_Cmd);
               end;
            end if;

            --  Final checkpoint
            Put_Line ("==> Creating final checkpoint...");
            Dummy := Sprite_Exec.Sprite_Run
              ("checkpoint create -s '" & SN & "'");
            if Dummy = 0 then
               Put_Line ("Final checkpoint saved.");
            else
               Put_Line ("Final checkpoint failed (non-fatal).");
            end if;
         else
            Put_Line ("Opening console...");
            Dummy := Sprite_Exec.Sprite_Run ("console -s '" & SN & "'");
         end if;

         Put_Line ("Done.");
      end;
   end Execute;

end Launch;
