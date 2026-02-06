--  pull_cmd.adb -- sprite-tool pull subcommand implementation
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Sprite_Exec;

package body Pull_Cmd is

   ---------------------------------------------------------------------------
   --  Print_Usage
   ---------------------------------------------------------------------------
   procedure Print_Usage is
   begin
      Put_Line ("Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]");
      New_Line;
      Put_Line ("Pull a file or directory from a sprite to a local path.");
      Put_Line ("Automatically detects whether the remote path is a file or directory.");
      New_Line;
      Put_Line ("If sprite-name is not provided, uses the SPRITE_NAME environment variable.");
      New_Line;
      Put_Line ("Examples:");
      Put_Line ("  sprite-tool pull /home/sprite/file.txt ./file.txt");
      Put_Line ("  sprite-tool pull /home/sprite/mydir ./mydir");
      Put_Line ("  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk");
   end Print_Usage;

   ---------------------------------------------------------------------------
   --  Execute
   ---------------------------------------------------------------------------
   procedure Execute (Start_Arg : in Positive) is
      Arg_Count   : constant Natural := Argument_Count;
      Remaining   : constant Natural := Arg_Count - Start_Arg + 1;
      Remote_Path : Unbounded_String;
      Local_Path  : Unbounded_String;
      Sprite_Name : Unbounded_String;
   begin
      --  We need at least 2 arguments after "pull"
      if Remaining < 2 then
         Print_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Remote_Path := To_Unbounded_String (Argument (Start_Arg));
      Local_Path  := To_Unbounded_String (Argument (Start_Arg + 1));

      --  Optional sprite name (3rd positional arg or env var)
      if Remaining >= 3 then
         Sprite_Name := To_Unbounded_String (Argument (Start_Arg + 2));
      elsif Ada.Environment_Variables.Exists ("SPRITE_NAME")
        and then Ada.Environment_Variables.Value ("SPRITE_NAME")'Length > 0
      then
         Sprite_Name :=
           To_Unbounded_String (Ada.Environment_Variables.Value ("SPRITE_NAME"));
      else
         Put_Line (Standard_Error,
                   "ERROR: sprite name not provided and SPRITE_NAME not set");
         Print_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      declare
         RP : constant String := To_String (Remote_Path);
         LP : constant String := To_String (Local_Path);
         SN : constant String := To_String (Sprite_Name);
      begin
         if Sprite_Exec.Is_Remote_Dir (SN, RP) then
            Put_Line ("Pulling directory: " & RP & " -> " & LP);
            Sprite_Exec.Pull_Dir (SN, RP, LP);
         else
            Put_Line ("Pulling file: " & RP & " -> " & LP);
            Sprite_Exec.Pull_File (SN, RP, LP);
         end if;

         Put_Line ("Done.");
      end;
   end Execute;

end Pull_Cmd;
