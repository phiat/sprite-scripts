--  config.ads -- Configuration loading from environment and .env files
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Config is

   type App_Config is record
      Sprite_Token        : Unbounded_String := Null_Unbounded_String;
      Agent               : Unbounded_String := To_Unbounded_String ("opencode");
      Claude_Auth         : Unbounded_String := To_Unbounded_String ("subscription");
      Anthropic_API_Key   : Unbounded_String := Null_Unbounded_String;
      Model               : Unbounded_String := Null_Unbounded_String;
      Checkpoint_Interval : Natural := 300;
      Env_File            : Unbounded_String := To_Unbounded_String ("./.env");
   end record;

   --  Load configuration from environment variables and .env file.
   --  Reads ENV_FILE (default "./.env"), parses it if it exists,
   --  then populates the config from environment variables.
   function Load return App_Config;

private

   --  Parse a simple KEY=VALUE .env file and set environment variables.
   --  Handles lines with optional quoting (single or double quotes).
   --  Skips blank lines, comments (#), and export prefixes.
   procedure Load_Env_File (Path : in String);

   --  Get environment variable with a default fallback.
   function Env_Or_Default (Name : in String; Default : in String) return String;

   --  Check if environment variable exists and is non-empty.
   function Env_Exists_Non_Empty (Name : in String) return Boolean;

end Config;
