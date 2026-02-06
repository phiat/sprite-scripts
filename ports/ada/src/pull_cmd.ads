--  pull_cmd.ads -- sprite-tool pull subcommand
package Pull_Cmd is

   --  Execute the pull subcommand.
   --  Parses arguments starting from Start_Arg index in Ada.Command_Line.
   --  Expected: <remote-path> <local-path> [sprite-name]
   procedure Execute (Start_Arg : in Positive);

   --  Print usage information for the pull subcommand.
   procedure Print_Usage;

end Pull_Cmd;
