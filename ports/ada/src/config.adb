--  config.adb -- Configuration loading implementation
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings;           use Ada.Strings;

package body Config is

   ---------------------------------------------------------------------------
   --  Env_Or_Default
   ---------------------------------------------------------------------------
   function Env_Or_Default (Name : in String; Default : in String) return String is
   begin
      if Ada.Environment_Variables.Exists (Name) then
         declare
            Val : constant String := Ada.Environment_Variables.Value (Name);
         begin
            if Val'Length > 0 then
               return Val;
            end if;
         end;
      end if;
      return Default;
   end Env_Or_Default;

   ---------------------------------------------------------------------------
   --  Env_Exists_Non_Empty
   ---------------------------------------------------------------------------
   function Env_Exists_Non_Empty (Name : in String) return Boolean is
   begin
      if not Ada.Environment_Variables.Exists (Name) then
         return False;
      end if;
      return Ada.Environment_Variables.Value (Name)'Length > 0;
   end Env_Exists_Non_Empty;

   ---------------------------------------------------------------------------
   --  Trim_Whitespace -- remove leading/trailing spaces and tabs
   ---------------------------------------------------------------------------
   function Trim_Whitespace (S : in String) return String is
      First : Natural := S'First;
      Last  : Natural := S'Last;
   begin
      while First <= Last
        and then (S (First) = ' ' or S (First) = ASCII.HT)
      loop
         First := First + 1;
      end loop;
      while Last >= First
        and then (S (Last) = ' ' or S (Last) = ASCII.HT)
      loop
         Last := Last - 1;
      end loop;
      if First > Last then
         return "";
      end if;
      return S (First .. Last);
   end Trim_Whitespace;

   ---------------------------------------------------------------------------
   --  Strip_Quotes -- remove surrounding single or double quotes
   ---------------------------------------------------------------------------
   function Strip_Quotes (S : in String) return String is
   begin
      if S'Length >= 2 then
         if (S (S'First) = '"' and S (S'Last) = '"')
           or (S (S'First) = ''' and S (S'Last) = ''')
         then
            return S (S'First + 1 .. S'Last - 1);
         end if;
      end if;
      return S;
   end Strip_Quotes;

   ---------------------------------------------------------------------------
   --  Load_Env_File
   ---------------------------------------------------------------------------
   procedure Load_Env_File (Path : in String) is
      File : File_Type;
   begin
      if not Ada.Directories.Exists (Path) then
         return;
      end if;

      Open (File, In_File, Path);

      while not End_Of_File (File) loop
         declare
            Raw_Line : constant String := Get_Line (File);
            Line     : constant String := Trim_Whitespace (Raw_Line);
         begin
            --  Skip empty lines and comments
            if Line'Length = 0 or else Line (Line'First) = '#' then
               null;  --  skip
            else
               --  Strip optional "export " prefix
               declare
                  Effective : constant String :=
                    (if Line'Length > 7
                       and then Line (Line'First .. Line'First + 6) = "export "
                     then Trim_Whitespace (Line (Line'First + 7 .. Line'Last))
                     else Line);
                  Eq_Pos : constant Natural := Index (Effective, "=");
               begin
                  if Eq_Pos > 0 then
                     declare
                        Key : constant String :=
                          Trim_Whitespace (Effective (Effective'First .. Eq_Pos - 1));
                        Val : constant String :=
                          Strip_Quotes (Trim_Whitespace
                            (Effective (Eq_Pos + 1 .. Effective'Last)));
                     begin
                        if Key'Length > 0 then
                           Ada.Environment_Variables.Set (Key, Val);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      Close (File);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
   end Load_Env_File;

   ---------------------------------------------------------------------------
   --  Load
   ---------------------------------------------------------------------------
   function Load return App_Config is
      Cfg      : App_Config;
      Env_Path : constant String := Env_Or_Default ("ENV_FILE", "./.env");
   begin
      Cfg.Env_File := To_Unbounded_String (Env_Path);

      --  Load .env file if it exists
      Load_Env_File (Env_Path);

      --  SPRITE_TOKEN with fallback to SPRITES_TOKEN
      declare
         Token : constant String := Env_Or_Default ("SPRITE_TOKEN", "");
      begin
         if Token'Length > 0 then
            Cfg.Sprite_Token := To_Unbounded_String (Token);
         else
            Cfg.Sprite_Token :=
              To_Unbounded_String (Env_Or_Default ("SPRITES_TOKEN", ""));
         end if;
      end;

      Cfg.Agent :=
        To_Unbounded_String (Env_Or_Default ("AGENT", "opencode"));

      Cfg.Claude_Auth :=
        To_Unbounded_String (Env_Or_Default ("CLAUDE_AUTH", "subscription"));

      Cfg.Anthropic_API_Key :=
        To_Unbounded_String (Env_Or_Default ("ANTHROPIC_API_KEY", ""));

      Cfg.Model :=
        To_Unbounded_String (Env_Or_Default ("MODEL", ""));

      --  Parse checkpoint interval
      declare
         Interval_Str : constant String :=
           Env_Or_Default ("CHECKPOINT_INTERVAL", "300");
      begin
         Cfg.Checkpoint_Interval := Natural'Value (Interval_Str);
      exception
         when others =>
            Put_Line ("WARNING: Invalid CHECKPOINT_INTERVAL '"
                      & Interval_Str & "', using default 300");
            Cfg.Checkpoint_Interval := 300;
      end;

      return Cfg;
   end Load;

end Config;
