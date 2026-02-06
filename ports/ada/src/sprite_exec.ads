--  sprite_exec.ads -- Wrapper for executing sprite CLI commands
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Sprite_Exec is

   --  Execute a shell command via system(3). Returns the exit status.
   function Run_Shell (Cmd : in String) return Integer;

   --  Execute a shell command, abort the program if it fails.
   procedure Run_Shell_Or_Die (Cmd : in String);

   --  Execute `sprite exec -s <sprite> bash -c "<cmd>"`.
   --  Returns the exit status.
   function Sprite_Exec_Cmd
     (Sprite_Name : in String;
      Cmd         : in String;
      Dry_Run     : in Boolean := False) return Integer;

   --  Execute `sprite exec -s <sprite> bash -c "<cmd>"` and capture stdout.
   --  Returns the captured output (trimmed). Sets Exit_Status.
   function Sprite_Exec_Capture
     (Sprite_Name : in String;
      Cmd         : in String;
      Exit_Status : out Integer;
      Dry_Run     : in Boolean := False) return Unbounded_String;

   --  Run a plain `sprite <args>` command. Returns exit status.
   function Sprite_Run (Args : in String; Dry_Run : in Boolean := False) return Integer;

   --  Run a plain `sprite <args>` command and capture stdout.
   function Sprite_Run_Capture
     (Args        : in String;
      Exit_Status : out Integer;
      Dry_Run     : in Boolean := False) return Unbounded_String;

   --  Check if a command exists on PATH.
   function Command_Exists (Name : in String) return Boolean;

   --  Push a local file to a sprite via cat redirection.
   procedure Push_File
     (Sprite_Name : in String;
      Local_Path  : in String;
      Remote_Path : in String;
      Dry_Run     : in Boolean := False);

   --  Push a local directory to a sprite via tar.
   procedure Push_Dir
     (Sprite_Name : in String;
      Local_Dir   : in String;
      Remote_Dir  : in String;
      Dry_Run     : in Boolean := False);

   --  Check if remote path is a directory on the sprite.
   function Is_Remote_Dir
     (Sprite_Name : in String;
      Remote_Path : in String;
      Dry_Run     : in Boolean := False) return Boolean;

   --  Pull a remote file from sprite to local path.
   procedure Pull_File
     (Sprite_Name : in String;
      Remote_Path : in String;
      Local_Path  : in String;
      Dry_Run     : in Boolean := False);

   --  Pull a remote directory from sprite to local path.
   procedure Pull_Dir
     (Sprite_Name : in String;
      Remote_Path : in String;
      Local_Dir   : in String;
      Dry_Run     : in Boolean := False);

end Sprite_Exec;
