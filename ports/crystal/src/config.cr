module SpriteTool
  # Configuration loaded from environment variables and .env files.
  class Config
    property sprite_token : String
    property agent : String
    property claude_auth : String
    property anthropic_api_key : String
    property model : String
    property checkpoint_interval : Int32
    property env_file : String
    property dry_run : Bool
    property checkpointing : Bool
    property upload_dirs : Array(String)

    def initialize
      @sprite_token = ""
      @agent = "opencode"
      @claude_auth = "subscription"
      @anthropic_api_key = ""
      @model = ""
      @checkpoint_interval = 300
      @env_file = "./.env"
      @dry_run = false
      @checkpointing = true
      @upload_dirs = [] of String
    end

    # Hand-rolled .env parser.
    # Skips blank lines and #comments. Parses KEY=VALUE with optional
    # single/double quote stripping. Sets parsed values into ENV
    # (does not overwrite existing env vars).
    def self.parse_env_file(path : String) : Hash(String, String)
      parsed = {} of String => String
      return parsed unless File.exists?(path) && File.file?(path)

      File.each_line(path) do |raw_line|
        line = raw_line.strip
        next if line.empty? || line.starts_with?("#")
        next unless line.includes?("=")

        eq_index = line.index('=')
        next unless eq_index

        key = line[0...eq_index].strip
        value = line[(eq_index + 1)..].strip

        # Strip matching quotes
        if value.size >= 2
          first = value[0]
          last = value[-1]
          if first == last && (first == '"' || first == '\'')
            value = value[1..-2]
          end
        end

        parsed[key] = value
        # Set in ENV (don't overwrite existing)
        ENV[key] = value unless ENV[key]?
      end

      parsed
    end

    # Load configuration from .env file and environment variables.
    def self.load : Config
      config = Config.new
      config.env_file = ENV["ENV_FILE"]? || "./.env"

      # Parse .env file first (populates ENV for keys not already set)
      parse_env_file(config.env_file)

      # SPRITE_TOKEN with SPRITES_TOKEN fallback
      config.sprite_token = ENV["SPRITE_TOKEN"]? || ""
      if config.sprite_token.empty?
        config.sprite_token = ENV["SPRITES_TOKEN"]? || ""
      end

      config.agent = ENV["AGENT"]? || "opencode"
      config.claude_auth = ENV["CLAUDE_AUTH"]? || "subscription"
      config.anthropic_api_key = ENV["ANTHROPIC_API_KEY"]? || ""
      config.model = ENV["MODEL"]? || ""

      interval_str = ENV["CHECKPOINT_INTERVAL"]? || "300"
      interval = interval_str.to_i32?
      if interval.nil?
        STDERR.puts "Error: invalid CHECKPOINT_INTERVAL #{interval_str.inspect} (must be integer)"
        exit 1
      end
      config.checkpoint_interval = interval

      config
    end
  end
end
