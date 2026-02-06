--  watch_cmd.ads -- sprite-tool watch subcommand
package Watch_Cmd is

   --  Execute the watch subcommand.
   --  Parses arguments starting from Start_Arg index in Ada.Command_Line.
   --  Expected: <sprite-name> [task-id] [poll-interval]
   procedure Execute (Start_Arg : in Positive);

   --  Print usage information for the watch subcommand.
   procedure Print_Usage;

end Watch_Cmd;
