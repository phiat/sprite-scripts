--  launch.ads -- sprite-tool launch subcommand
package Launch is

   --  Execute the launch subcommand.
   --  Parses arguments starting from Start_Arg index in Ada.Command_Line.
   --  Expected: [--dry-run] [--no-checkpoint] [--upload dir]... sprite-name [plan-file]
   procedure Execute (Start_Arg : in Positive);

   --  Print usage information for the launch subcommand.
   procedure Print_Usage;

end Launch;
