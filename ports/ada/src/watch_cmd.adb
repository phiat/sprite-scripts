--  watch_cmd.adb -- sprite-tool watch subcommand implementation
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Sprite_Exec;

package body Watch_Cmd is

   ---------------------------------------------------------------------------
   --  Print_Usage
   ---------------------------------------------------------------------------
   procedure Print_Usage is
   begin
      Put_Line ("Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]");
      New_Line;
      Put_Line ("Arguments:");
      Put_Line ("  sprite-name     Name of the sprite to watch");
      Put_Line ("  task-id         Beads task ID to track (default: auto-detect)");
      Put_Line ("  poll-interval   Seconds between polls (default: 30)");
      New_Line;
      Put_Line ("Examples:");
      Put_Line ("  sprite-tool watch ember-red-hawk");
      Put_Line ("  sprite-tool watch ember-red-hawk CRM-1");
      Put_Line ("  sprite-tool watch ember-red-hawk CRM-1 60");
   end Print_Usage;

   ---------------------------------------------------------------------------
   --  Trim -- remove leading/trailing whitespace
   ---------------------------------------------------------------------------
   function Trim (S : in String) return String is
      First : Natural := S'First;
      Last  : Natural := S'Last;
   begin
      while First <= Last
        and then (S (First) = ' ' or S (First) = ASCII.HT
                  or S (First) = ASCII.LF or S (First) = ASCII.CR)
      loop
         First := First + 1;
      end loop;
      while Last >= First
        and then (S (Last) = ' ' or S (Last) = ASCII.HT
                  or S (Last) = ASCII.LF or S (Last) = ASCII.CR)
      loop
         Last := Last - 1;
      end loop;
      if First > Last then
         return "";
      end if;
      return S (First .. Last);
   end Trim;

   ---------------------------------------------------------------------------
   --  Is_Task_Done -- check if status output indicates completion
   ---------------------------------------------------------------------------
   function Is_Task_Done (Status : in String) return Boolean is
      Lower : constant String := To_Lower (Status);
   begin
      if Index (Lower, "closed") > 0 then
         return True;
      end if;
      if Index (Lower, "done") > 0 then
         return True;
      end if;
      if Index (Lower, "completed") > 0 then
         return True;
      end if;
      return False;
   end Is_Task_Done;

   ---------------------------------------------------------------------------
   --  Execute
   ---------------------------------------------------------------------------
   procedure Execute (Start_Arg : in Positive) is
      Arg_Count     : constant Natural := Argument_Count;
      Remaining     : constant Natural := Arg_Count - Start_Arg + 1;
      Sprite_Name   : Unbounded_String;
      Task_ID       : Unbounded_String := Null_Unbounded_String;
      Poll_Interval : Duration := 30.0;
   begin
      if Remaining < 1 then
         Print_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Sprite_Name := To_Unbounded_String (Argument (Start_Arg));

      if Remaining >= 2 then
         Task_ID := To_Unbounded_String (Argument (Start_Arg + 1));
      end if;

      if Remaining >= 3 then
         begin
            Poll_Interval := Duration (Natural'Value (Argument (Start_Arg + 2)));
         exception
            when others =>
               Put_Line (Standard_Error,
                         "ERROR: Invalid poll interval: "
                         & Argument (Start_Arg + 2));
               Set_Exit_Status (Failure);
               return;
         end;
      end if;

      declare
         SN : constant String := To_String (Sprite_Name);
      begin
         --  Auto-detect task ID if not provided
         if Task_ID = Null_Unbounded_String
           or else Length (Task_ID) = 0
         then
            Put_Line ("Detecting tracker task...");

            --  Try critical priority first
            declare
               Status    : Integer;
               Output    : constant Unbounded_String :=
                 Sprite_Exec.Sprite_Exec_Capture
                   (SN,
                    "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'",
                    Status);
               Out_Str   : constant String := Trim (To_String (Output));
            begin
               if Out_Str'Length > 0 then
                  Task_ID := To_Unbounded_String (Out_Str);
               end if;
            end;

            --  Fallback to first open task
            if Task_ID = Null_Unbounded_String
              or else Length (Task_ID) = 0
            then
               Put_Line ("No critical task found. Falling back to first open task...");
               declare
                  Status  : Integer;
                  Output  : constant Unbounded_String :=
                    Sprite_Exec.Sprite_Exec_Capture
                      (SN,
                       "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'",
                       Status);
                  Out_Str : constant String := Trim (To_String (Output));
               begin
                  if Out_Str'Length > 0 then
                     Task_ID := To_Unbounded_String (Out_Str);
                  end if;
               end;
            end if;

            if Task_ID = Null_Unbounded_String
              or else Length (Task_ID) = 0
            then
               Put_Line ("ERROR: No beads tasks found on sprite '" & SN & "'");
               Put_Line ("Specify a task ID manually: sprite-tool watch "
                         & SN & " <task-id>");
               Set_Exit_Status (Failure);
               return;
            end if;

            Put_Line ("Tracking task: " & To_String (Task_ID));
         end if;

         declare
            TID : constant String := To_String (Task_ID);
            Interval_Str : constant String :=
              Natural'Image (Natural (Poll_Interval));
         begin
            Put_Line ("Watching sprite '" & SN & "' task '" & TID
                      & "' (every" & Interval_Str & "s)");
            Put_Line ("Press Ctrl+C to stop");
            New_Line;

            --  Main watch loop
            loop
               --  Clear screen using ANSI escape
               Put (ASCII.ESC & "[2J" & ASCII.ESC & "[H");

               Put_Line ("=== sprite-watch: " & SN & " / " & TID & " ===");
               New_Line;

               --  Show task status
               declare
                  Status  : Integer;
                  Output  : constant Unbounded_String :=
                    Sprite_Exec.Sprite_Exec_Capture
                      (SN,
                       "cd /home/sprite && bd show " & TID & " 2>/dev/null",
                       Status);
                  Out_Str : constant String := To_String (Output);
               begin
                  if Status = 0 and then Out_Str'Length > 0 then
                     Put_Line (Out_Str);
                  else
                     Put_Line ("(could not read task)");
                  end if;
                  New_Line;

                  --  Show recent comments
                  Put_Line ("--- Recent updates ---");
                  declare
                     C_Status  : Integer;
                     C_Output  : constant Unbounded_String :=
                       Sprite_Exec.Sprite_Exec_Capture
                         (SN,
                          "cd /home/sprite && bd comments " & TID
                          & " 2>/dev/null | tail -8",
                          C_Status);
                     C_Str     : constant String := To_String (C_Output);
                  begin
                     if C_Str'Length > 0 then
                        Put_Line (C_Str);
                     else
                        Put_Line ("(no comments)");
                     end if;
                  end;
                  New_Line;

                  --  Check if done
                  declare
                     S_Status  : Integer;
                     S_Output  : constant Unbounded_String :=
                       Sprite_Exec.Sprite_Exec_Capture
                         (SN,
                          "cd /home/sprite && bd show " & TID
                          & " 2>/dev/null | grep -i status",
                          S_Status);
                     S_Str     : constant String := To_String (S_Output);
                  begin
                     if Is_Task_Done (S_Str) then
                        Put_Line ("==========================================");
                        Put_Line ("PROJECT COMPLETE");
                        Put_Line ("==========================================");
                        return;
                     end if;
                  end;
               end;

               --  Sleep for poll interval
               delay Poll_Interval;
            end loop;
         end;
      end;
   end Execute;

end Watch_Cmd;
