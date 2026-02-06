--  sprite_tool.adb -- Main entry point and subcommand dispatch
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Launch;
with Push_Cmd;
with Pull_Cmd;
with Watch_Cmd;

procedure Sprite_Tool is

   procedure Print_Usage is
   begin
      Put_Line ("sprite-tool: A CLI tool for managing sprites with coding agents");
      New_Line;
      Put_Line ("Usage: sprite-tool <command> [args...]");
      New_Line;
      Put_Line ("Commands:");
      Put_Line ("  launch    Create and configure a sprite with a coding agent");
      Put_Line ("  push      Push a local file or directory to a sprite");
      Put_Line ("  pull      Pull a file or directory from a sprite");
      Put_Line ("  watch     Poll a beads task for progress");
      New_Line;
      Put_Line ("Run 'sprite-tool <command> --help' for more information on a command.");
   end Print_Usage;

begin
   if Argument_Count < 1 then
      Print_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      Subcmd : constant String := Argument (1);
   begin
      if Subcmd = "launch" then
         Launch.Execute (Start_Arg => 2);

      elsif Subcmd = "push" then
         Push_Cmd.Execute (Start_Arg => 2);

      elsif Subcmd = "pull" then
         Pull_Cmd.Execute (Start_Arg => 2);

      elsif Subcmd = "watch" then
         Watch_Cmd.Execute (Start_Arg => 2);

      elsif Subcmd = "--help" or Subcmd = "-h" or Subcmd = "help" then
         Print_Usage;

      else
         Put_Line (Standard_Error, "ERROR: Unknown command: " & Subcmd);
         New_Line;
         Print_Usage;
         Set_Exit_Status (Failure);
      end if;
   end;
end Sprite_Tool;
