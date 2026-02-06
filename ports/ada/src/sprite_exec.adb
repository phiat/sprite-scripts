--  sprite_exec.adb -- Sprite CLI execution wrapper implementation
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Directories;
with System;
with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Strings;     use Interfaces.C.Strings;

package body Sprite_Exec is

   --  Import C system() for shell command execution
   function C_System (Cmd : chars_ptr) return int;
   pragma Import (C, C_System, "system");

   --  Import C popen/pclose for capturing command output
   type C_File_Ptr is new System.Address;

   function C_Popen (Cmd : chars_ptr; Mode : chars_ptr) return C_File_Ptr;
   pragma Import (C, C_Popen, "popen");

   function C_Pclose (Stream : C_File_Ptr) return int;
   pragma Import (C, C_Pclose, "pclose");

   function C_Fgets (Buf : chars_ptr; Size : int; Stream : C_File_Ptr) return chars_ptr;
   pragma Import (C, C_Fgets, "fgets");

   Null_C_File : constant C_File_Ptr := C_File_Ptr (System.Null_Address);

   ---------------------------------------------------------------------------
   --  Shell_Quote -- escape single quotes in a string for shell use
   ---------------------------------------------------------------------------
   function Shell_Quote (S : in String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in S'Range loop
         if S (I) = ''' then
            Append (Result, "'\''");
         else
            Append (Result, S (I));
         end if;
      end loop;
      return To_String (Result);
   end Shell_Quote;

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
   --  Dirname -- return directory portion of a path
   ---------------------------------------------------------------------------
   function Dirname (Path : in String) return String is
   begin
      for I in reverse Path'Range loop
         if Path (I) = '/' then
            if I = Path'First then
               return "/";
            else
               return Path (Path'First .. I - 1);
            end if;
         end if;
      end loop;
      return ".";
   end Dirname;

   ---------------------------------------------------------------------------
   --  Basename -- return filename portion of a path
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
   --  Run_Shell
   ---------------------------------------------------------------------------
   function Run_Shell (Cmd : in String) return Integer is
      C_Cmd  : chars_ptr := New_String (Cmd);
      Result : constant int := C_System (C_Cmd);
   begin
      Free (C_Cmd);
      return Integer (Result);
   end Run_Shell;

   ---------------------------------------------------------------------------
   --  Run_Shell_Or_Die
   ---------------------------------------------------------------------------
   procedure Run_Shell_Or_Die (Cmd : in String) is
      Status : constant Integer := Run_Shell (Cmd);
   begin
      if Status /= 0 then
         Put_Line (Standard_Error, "ERROR: Command failed (status"
                   & Integer'Image (Status) & "): " & Cmd);
         raise Program_Error with "Command failed: " & Cmd;
      end if;
   end Run_Shell_Or_Die;

   ---------------------------------------------------------------------------
   --  Capture_Shell -- execute command and capture its stdout
   ---------------------------------------------------------------------------
   function Capture_Shell
     (Cmd         : in String;
      Exit_Status : out Integer) return Unbounded_String
   is
      C_Cmd    : chars_ptr := New_String (Cmd);
      C_Mode   : chars_ptr := New_String ("r");
      Stream   : C_File_Ptr;
      Buf_Size : constant := 4096;
      C_Buf    : chars_ptr := New_String ((1 .. Buf_Size => ' '));
      Output   : Unbounded_String := Null_Unbounded_String;
      Ret      : chars_ptr;
      Status   : int;
   begin
      Stream := C_Popen (C_Cmd, C_Mode);
      Free (C_Cmd);
      Free (C_Mode);

      if Stream = Null_C_File then
         Free (C_Buf);
         Exit_Status := -1;
         return To_Unbounded_String ("ERROR: popen failed");
      end if;

      loop
         Ret := C_Fgets (C_Buf, int (Buf_Size), Stream);
         exit when Ret = Null_Ptr;
         Append (Output, Value (C_Buf));
      end loop;

      Status := C_Pclose (Stream);
      Free (C_Buf);
      Exit_Status := Integer (Status);
      return Output;
   end Capture_Shell;

   ---------------------------------------------------------------------------
   --  Sprite_Exec_Cmd
   ---------------------------------------------------------------------------
   function Sprite_Exec_Cmd
     (Sprite_Name : in String;
      Cmd         : in String;
      Dry_Run     : in Boolean := False) return Integer
   is
      Full_Cmd : constant String :=
        "sprite exec -s '" & Shell_Quote (Sprite_Name)
        & "' bash -c '" & Shell_Quote (Cmd) & "'";
   begin
      if Dry_Run then
         Put_Line ("  [dry-run] " & Full_Cmd);
         return 0;
      end if;
      return Run_Shell (Full_Cmd);
   end Sprite_Exec_Cmd;

   ---------------------------------------------------------------------------
   --  Sprite_Exec_Capture
   ---------------------------------------------------------------------------
   function Sprite_Exec_Capture
     (Sprite_Name : in String;
      Cmd         : in String;
      Exit_Status : out Integer;
      Dry_Run     : in Boolean := False) return Unbounded_String
   is
      Full_Cmd : constant String :=
        "sprite exec -s '" & Shell_Quote (Sprite_Name)
        & "' bash -c '" & Shell_Quote (Cmd) & "'";
   begin
      if Dry_Run then
         Put_Line ("  [dry-run] " & Full_Cmd);
         Exit_Status := 0;
         return Null_Unbounded_String;
      end if;

      declare
         Result : constant Unbounded_String :=
           Capture_Shell (Full_Cmd, Exit_Status);
      begin
         return To_Unbounded_String (Trim (To_String (Result)));
      end;
   end Sprite_Exec_Capture;

   ---------------------------------------------------------------------------
   --  Sprite_Run
   ---------------------------------------------------------------------------
   function Sprite_Run (Args : in String; Dry_Run : in Boolean := False) return Integer is
      Full_Cmd : constant String := "sprite " & Args;
   begin
      if Dry_Run then
         Put_Line ("  [dry-run] " & Full_Cmd);
         return 0;
      end if;
      return Run_Shell (Full_Cmd);
   end Sprite_Run;

   ---------------------------------------------------------------------------
   --  Sprite_Run_Capture
   ---------------------------------------------------------------------------
   function Sprite_Run_Capture
     (Args        : in String;
      Exit_Status : out Integer;
      Dry_Run     : in Boolean := False) return Unbounded_String
   is
      Full_Cmd : constant String := "sprite " & Args;
   begin
      if Dry_Run then
         Put_Line ("  [dry-run] " & Full_Cmd);
         Exit_Status := 0;
         return Null_Unbounded_String;
      end if;
      return Capture_Shell (Full_Cmd, Exit_Status);
   end Sprite_Run_Capture;

   ---------------------------------------------------------------------------
   --  Command_Exists
   ---------------------------------------------------------------------------
   function Command_Exists (Name : in String) return Boolean is
      Status : Integer;
      Dummy  : Unbounded_String;
   begin
      Dummy := Capture_Shell ("command -v " & Name & " >/dev/null 2>&1 && echo yes || echo no",
                              Status);
      return Trim (To_String (Dummy)) = "yes";
   end Command_Exists;

   ---------------------------------------------------------------------------
   --  Push_File
   ---------------------------------------------------------------------------
   procedure Push_File
     (Sprite_Name : in String;
      Local_Path  : in String;
      Remote_Path : in String;
      Dry_Run     : in Boolean := False)
   is
      Remote_Dir : constant String := Dirname (Remote_Path);
      Dummy      : Integer;
   begin
      if Dry_Run then
         Put_Line ("  [dry-run] push " & Local_Path & " -> sprite:" & Remote_Path);
         return;
      end if;

      if not Ada.Directories.Exists (Local_Path) then
         Put_Line (Standard_Error, "ERROR: Local file not found: " & Local_Path);
         return;
      end if;

      --  Create remote directory
      Dummy := Sprite_Exec_Cmd (Sprite_Name,
                                "mkdir -p '" & Shell_Quote (Remote_Dir) & "'");

      --  Push file content via cat redirection
      declare
         Cmd : constant String :=
           "cat '" & Shell_Quote (Local_Path)
           & "' | sprite exec -s '" & Shell_Quote (Sprite_Name)
           & "' bash -c 'cat > ''" & Shell_Quote (Remote_Path) & "'''";
      begin
         Dummy := Run_Shell (Cmd);
         if Dummy /= 0 then
            Put_Line (Standard_Error,
                      "WARNING: Push file may have failed (status"
                      & Integer'Image (Dummy) & ")");
         end if;
      end;
   end Push_File;

   ---------------------------------------------------------------------------
   --  Push_Dir
   ---------------------------------------------------------------------------
   procedure Push_Dir
     (Sprite_Name : in String;
      Local_Dir   : in String;
      Remote_Dir  : in String;
      Dry_Run     : in Boolean := False)
   is
      Parent : constant String := Dirname (Local_Dir);
      Base   : constant String := Basename (Local_Dir);
      Dummy  : Integer;
   begin
      if Dry_Run then
         Put_Line ("  [dry-run] push dir " & Local_Dir & " -> sprite:" & Remote_Dir);
         return;
      end if;

      if not Ada.Directories.Exists (Local_Dir) then
         Put_Line (Standard_Error, "ERROR: Local directory not found: " & Local_Dir);
         return;
      end if;

      --  Create remote directory
      Dummy := Sprite_Exec_Cmd (Sprite_Name,
                                "mkdir -p '" & Shell_Quote (Remote_Dir) & "'");

      --  Tar and push
      declare
         Cmd : constant String :=
           "tar czf - -C '" & Shell_Quote (Parent) & "' '" & Shell_Quote (Base)
           & "' | sprite exec -s '" & Shell_Quote (Sprite_Name)
           & "' bash -c 'tar xzf - -C ''" & Shell_Quote (Dirname (Remote_Dir)) & "'''";
      begin
         Dummy := Run_Shell (Cmd);
         if Dummy /= 0 then
            Put_Line (Standard_Error,
                      "WARNING: Push dir may have failed (status"
                      & Integer'Image (Dummy) & ")");
         end if;
      end;
   end Push_Dir;

   ---------------------------------------------------------------------------
   --  Is_Remote_Dir
   ---------------------------------------------------------------------------
   function Is_Remote_Dir
     (Sprite_Name : in String;
      Remote_Path : in String;
      Dry_Run     : in Boolean := False) return Boolean
   is
      Status : Integer;
      Output : Unbounded_String;
   begin
      if Dry_Run then
         Put_Line ("  [dry-run] check if " & Remote_Path & " is dir on sprite");
         return False;
      end if;

      Output := Sprite_Exec_Capture
        (Sprite_Name,
         "[ -d '" & Shell_Quote (Remote_Path) & "' ] && echo dir || echo file",
         Status);
      return To_String (Output) = "dir";
   end Is_Remote_Dir;

   ---------------------------------------------------------------------------
   --  Pull_File
   ---------------------------------------------------------------------------
   procedure Pull_File
     (Sprite_Name : in String;
      Remote_Path : in String;
      Local_Path  : in String;
      Dry_Run     : in Boolean := False)
   is
      Local_Dir : constant String := Dirname (Local_Path);
      Dummy     : Integer;
   begin
      if Dry_Run then
         Put_Line ("  [dry-run] pull sprite:" & Remote_Path & " -> " & Local_Path);
         return;
      end if;

      --  Create local directory
      if Local_Dir'Length > 0 and then Local_Dir /= "." then
         Dummy := Run_Shell ("mkdir -p '" & Shell_Quote (Local_Dir) & "'");
      end if;

      --  Pull file via sprite exec cat
      declare
         Cmd : constant String :=
           "sprite exec -s '" & Shell_Quote (Sprite_Name)
           & "' cat '" & Shell_Quote (Remote_Path)
           & "' > '" & Shell_Quote (Local_Path) & "'";
      begin
         Dummy := Run_Shell (Cmd);
         if Dummy /= 0 then
            Put_Line (Standard_Error,
                      "WARNING: Pull file may have failed (status"
                      & Integer'Image (Dummy) & ")");
         end if;
      end;
   end Pull_File;

   ---------------------------------------------------------------------------
   --  Pull_Dir
   ---------------------------------------------------------------------------
   procedure Pull_Dir
     (Sprite_Name : in String;
      Remote_Path : in String;
      Local_Dir   : in String;
      Dry_Run     : in Boolean := False)
   is
      Dummy : Integer;
   begin
      if Dry_Run then
         Put_Line ("  [dry-run] pull dir sprite:" & Remote_Path & " -> " & Local_Dir);
         return;
      end if;

      --  Create local directory
      Dummy := Run_Shell ("mkdir -p '" & Shell_Quote (Local_Dir) & "'");

      --  Pull via tar
      declare
         Cmd : constant String :=
           "sprite exec -s '" & Shell_Quote (Sprite_Name)
           & "' bash -c 'tar czf - -C ''" & Shell_Quote (Remote_Path)
           & "'' .'"
           & " | tar xzf - -C '" & Shell_Quote (Local_Dir) & "'";
      begin
         Dummy := Run_Shell (Cmd);
         if Dummy /= 0 then
            Put_Line (Standard_Error,
                      "WARNING: Pull dir may have failed (status"
                      & Integer'Image (Dummy) & ")");
         end if;
      end;
   end Pull_Dir;

end Sprite_Exec;
