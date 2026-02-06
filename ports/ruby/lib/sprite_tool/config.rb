# frozen_string_literal: true

module SpriteTool
  # Configuration loaded from environment variables and .env files.
  class Config
    attr_accessor :sprite_token, :agent, :claude_auth, :anthropic_api_key,
                  :model, :checkpoint_interval, :env_file,
                  :dry_run, :checkpointing, :upload_dirs

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
      @upload_dirs = []
    end

    # Hand-rolled .env parser.
    # Skips blank lines and #comments. Parses KEY=VALUE with optional
    # single/double quote stripping. Sets parsed values into ENV
    # (does not overwrite existing env vars).
    def self.parse_env_file(path)
      return {} unless File.file?(path)

      parsed = {}
      File.readlines(path).each do |line|
        line = line.strip
        next if line.empty? || line.start_with?("#")
        next unless line.include?("=")

        key, _, value = line.partition("=")
        key = key.strip
        value = value.strip

        # Strip matching quotes
        if value.length >= 2 && value[0] == value[-1] && %w[" '].include?(value[0])
          value = value[1..-2]
        end

        parsed[key] = value
        # Set in ENV (don't overwrite existing)
        ENV[key] ||= value
      end

      parsed
    end

    # Load configuration from .env file and environment variables.
    def self.load
      config = new
      config.env_file = ENV.fetch("ENV_FILE", "./.env")

      # Parse .env file first (populates ENV for keys not already set)
      parse_env_file(config.env_file)

      # SPRITE_TOKEN with SPRITES_TOKEN fallback
      config.sprite_token = ENV.fetch("SPRITE_TOKEN", "")
      config.sprite_token = ENV.fetch("SPRITES_TOKEN", "") if config.sprite_token.empty?

      config.agent = ENV.fetch("AGENT", "opencode")
      config.claude_auth = ENV.fetch("CLAUDE_AUTH", "subscription")
      config.anthropic_api_key = ENV.fetch("ANTHROPIC_API_KEY", "")
      config.model = ENV.fetch("MODEL", "")

      interval_str = ENV.fetch("CHECKPOINT_INTERVAL", "300")
      begin
        config.checkpoint_interval = Integer(interval_str)
      rescue ArgumentError
        abort "Error: invalid CHECKPOINT_INTERVAL #{interval_str.inspect} (must be integer)"
      end

      config
    end
  end
end
