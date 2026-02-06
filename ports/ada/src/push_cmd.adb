--  push_cmd.adb -- sprite-tool push subcommand implementation
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Directories;          use Ada.Directories;
with Ada.Environment_Variables;
with Sprite_Exec;

package body Push_Cmd is

   ---------------------------------------------------------------------------
   --  Print_Usage
   ---------------------------------------------------------------------------
   procedure Print_Usage is
   begin
      Put_Line ("Usage: sprite-tool push <local-path> <remote-path> [sprite-name]");
      New_Line;
      Put_Line ("Push a local file or directory to a sprite.");
      New_Line;
      Put_Line ("If sprite-name is not provided, uses the SPRITE_NAME environment variable.");
      New_Line;
      Put_Line ("Examples:");
      Put_Line ("  sprite-tool push ./file.txt /home/sprite/file.txt");
      Put_Line ("  sprite-tool push ./mydir /home/sprite/mydir");
      Put_Line ("  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk");
   end Print_Usage;

   ---------------------------------------------------------------------------
   --  Execute
   ---------------------------------------------------------------------------
   procedure Execute (Start_Arg : in Positive) is
      Arg_Count   : constant Natural := Argument_Count;
      Remaining   : constant Natural := Arg_Count - Start_Arg + 1;
      Local_Path  : Unbounded_String;
      Remote_Path : Unbounded_String;
      Sprite_Name : Unbounded_String;
   begin
      --  We need at least 2 arguments after "push"
      if Remaining < 2 then
         Print_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Local_Path  := To_Unbounded_String (Argument (Start_Arg));
      Remote_Path := To_Unbounded_String (Argument (Start_Arg + 1));

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
         LP : constant String := To_String (Local_Path);
         RP : constant String := To_String (Remote_Path);
         SN : constant String := To_String (Sprite_Name);
      begin
         if not Ada.Directories.Exists (LP) then
            Put_Line (Standard_Error, "Error: " & LP & " does not exist");
            Set_Exit_Status (Failure);
            return;
         end if;

         if Ada.Directories.Kind (LP) = Ada.Directories.Directory then
            Put_Line ("Pushing directory: " & LP & " -> " & RP);
            Sprite_Exec.Push_Dir (SN, LP, RP);
         else
            Put_Line ("Pushing file: " & LP & " -> " & RP);
            Sprite_Exec.Push_File (SN, LP, RP);
         end if;

         Put_Line ("Done.");
      end;
   end Execute;

end Push_Cmd;
