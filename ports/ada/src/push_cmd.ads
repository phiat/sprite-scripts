--  push_cmd.ads -- sprite-tool push subcommand
package Push_Cmd is

   --  Execute the push subcommand.
   --  Parses arguments starting from Start_Arg index in Ada.Command_Line.
   --  Expected: <local-path> <remote-path> [sprite-name]
   procedure Execute (Start_Arg : in Positive);

   --  Print usage information for the push subcommand.
   procedure Print_Usage;

end Push_Cmd;
