package config

import (
	"fmt"
	"os"
	"strconv"

	"github.com/joho/godotenv"
)

// Config holds all configuration derived from environment variables and .env files.
type Config struct {
	SpriteToken        string
	Agent              string
	ClaudeAuth         string
	AnthropicAPIKey    string
	Model              string
	CheckpointInterval int
	EnvFile            string
}

// Load reads the .env file (if present) and populates Config from environment variables.
func Load() (*Config, error) {
	envFile := os.Getenv("ENV_FILE")
	if envFile == "" {
		envFile = ".env"
	}

	// Load .env file if it exists; ignore error if missing
	if _, err := os.Stat(envFile); err == nil {
		if err := godotenv.Load(envFile); err != nil {
			return nil, fmt.Errorf("loading %s: %w", envFile, err)
		}
	}

	token := os.Getenv("SPRITE_TOKEN")
	if token == "" {
		token = os.Getenv("SPRITES_TOKEN")
	}

	agent := os.Getenv("AGENT")
	if agent == "" {
		agent = "opencode"
	}

	claudeAuth := os.Getenv("CLAUDE_AUTH")
	if claudeAuth == "" {
		claudeAuth = "subscription"
	}

	interval := 300
	if v := os.Getenv("CHECKPOINT_INTERVAL"); v != "" {
		parsed, err := strconv.Atoi(v)
		if err != nil {
			return nil, fmt.Errorf("invalid CHECKPOINT_INTERVAL %q: %w", v, err)
		}
		interval = parsed
	}

	return &Config{
		SpriteToken:        token,
		Agent:              agent,
		ClaudeAuth:         claudeAuth,
		AnthropicAPIKey:    os.Getenv("ANTHROPIC_API_KEY"),
		Model:              os.Getenv("MODEL"),
		CheckpointInterval: interval,
		EnvFile:            envFile,
	}, nil
}
