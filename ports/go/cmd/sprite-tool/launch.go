package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"sprite-tool/internal/config"
	"sprite-tool/internal/sprite"

	"github.com/spf13/cobra"
)

var (
	launchDryRun      bool
	launchNoCheckpoint bool
	launchUploadDirs  []string
)

var launchCmd = &cobra.Command{
	Use:   "launch [flags] sprite-name [plan-file]",
	Short: "Create and configure a sprite with a coding agent",
	Long: `Launch creates a new sprite (or reuses an existing one), configures it with
git, beads, and a coding agent (claude or opencode), then starts the agent
with an optional plan file. Background checkpointing runs while the agent
is active.`,
	Args: cobra.RangeArgs(1, 2),
	RunE: runLaunch,
}

func init() {
	launchCmd.Flags().BoolVar(&launchDryRun, "dry-run", false, "Print commands instead of executing them")
	launchCmd.Flags().BoolVar(&launchNoCheckpoint, "no-checkpoint", false, "Disable background checkpointing")
	launchCmd.Flags().StringArrayVar(&launchUploadDirs, "upload", nil, "Local directory to upload to the sprite (repeatable)")
	rootCmd.AddCommand(launchCmd)
}

func runLaunch(cmd *cobra.Command, args []string) error {
	spriteName := args[0]
	var planFile string
	if len(args) > 1 {
		planFile = args[1]
	}

	// Load configuration
	cfg, err := config.Load()
	if err != nil {
		return fmt.Errorf("loading config: %w", err)
	}

	client := sprite.NewClient(spriteName, launchDryRun)

	// Step 1: Ensure sprite CLI is installed
	fmt.Println("==> Checking sprite CLI...")
	if err := sprite.EnsureCLI(launchDryRun); err != nil {
		return fmt.Errorf("ensuring sprite CLI: %w", err)
	}

	// Step 2: Authenticate
	fmt.Println("==> Authenticating...")
	if err := sprite.Auth(cfg.SpriteToken, launchDryRun); err != nil {
		return fmt.Errorf("authenticating: %w", err)
	}

	// Step 3: Create sprite or reuse existing
	fmt.Printf("==> Checking for existing sprite %s...\n", spriteName)
	exists, err := client.Exists()
	if err != nil {
		return fmt.Errorf("checking sprite existence: %w", err)
	}
	if exists {
		fmt.Printf("Sprite %s already exists, reusing.\n", spriteName)
	} else {
		fmt.Printf("Creating sprite %s...\n", spriteName)
		if err := client.Create(); err != nil {
			return fmt.Errorf("creating sprite: %w", err)
		}
	}

	// Step 4: Push .env to sprite
	if _, err := os.Stat(cfg.EnvFile); err == nil {
		fmt.Printf("==> Pushing %s to /home/sprite/.env...\n", cfg.EnvFile)
		if err := client.PushFile(cfg.EnvFile, "/home/sprite/.env"); err != nil {
			return fmt.Errorf("pushing .env: %w", err)
		}
	}

	// Step 5: Push plan file
	if planFile != "" {
		if _, err := os.Stat(planFile); err != nil {
			return fmt.Errorf("plan file %s not found: %w", planFile, err)
		}
		fmt.Printf("==> Pushing plan file %s to /home/sprite/plan.md...\n", planFile)
		if err := client.PushFile(planFile, "/home/sprite/plan.md"); err != nil {
			return fmt.Errorf("pushing plan file: %w", err)
		}
	}

	// Step 6: Upload directories
	for _, dir := range launchUploadDirs {
		absDir, err := filepath.Abs(dir)
		if err != nil {
			return fmt.Errorf("resolving upload dir %s: %w", dir, err)
		}
		info, err := os.Stat(absDir)
		if err != nil {
			return fmt.Errorf("upload dir %s not found: %w", dir, err)
		}
		if !info.IsDir() {
			return fmt.Errorf("upload path %s is not a directory", dir)
		}
		baseName := filepath.Base(absDir)
		remotePath := fmt.Sprintf("/home/sprite/%s", baseName)
		fmt.Printf("==> Uploading %s to %s...\n", absDir, remotePath)
		if err := client.PushDir(absDir, remotePath); err != nil {
			return fmt.Errorf("uploading dir %s: %w", dir, err)
		}
	}

	// Step 7: Git init on sprite
	fmt.Println("==> Initializing git on sprite...")
	_, err = client.Exec("cd /home/sprite && git init && git add -A && git commit -m 'initial' --allow-empty")
	if err != nil {
		return fmt.Errorf("git init: %w", err)
	}

	// Step 8: Install beads
	fmt.Println("==> Installing beads...")
	_, err = client.Exec("command -v bd >/dev/null 2>&1 || curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash")
	if err != nil {
		return fmt.Errorf("installing beads: %w", err)
	}

	// Step 9: Install and auth agent
	fmt.Printf("==> Setting up agent: %s...\n", cfg.Agent)
	if err := setupAgent(client, cfg); err != nil {
		return fmt.Errorf("setting up agent: %w", err)
	}

	// Step 10: Background checkpointing
	var cancelCheckpoint context.CancelFunc
	checkpointDone := make(chan struct{})
	if !launchNoCheckpoint {
		var ctx context.Context
		ctx, cancelCheckpoint = context.WithCancel(context.Background())
		interval := time.Duration(cfg.CheckpointInterval) * time.Second
		go client.RunCheckpointing(ctx, interval, checkpointDone)
	}

	// Handle interrupt to clean up checkpointing
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		<-sigCh
		fmt.Println("\nInterrupted. Cleaning up...")
		if cancelCheckpoint != nil {
			cancelCheckpoint()
			<-checkpointDone
		}
		// Final checkpoint on interrupt
		fmt.Println("==> Creating final checkpoint...")
		if err := client.CheckpointCreate(); err != nil {
			fmt.Printf("Warning: final checkpoint failed: %v\n", err)
		}
		os.Exit(0)
	}()

	// Step 11: Launch agent with plan or open console
	fmt.Println("==> Launching agent...")
	agentErr := launchAgent(client, cfg, planFile)

	// Step 12: Stop checkpointing and create final checkpoint
	if cancelCheckpoint != nil {
		cancelCheckpoint()
		<-checkpointDone
	}

	fmt.Println("==> Creating final checkpoint...")
	if err := client.CheckpointCreate(); err != nil {
		fmt.Printf("Warning: final checkpoint failed: %v\n", err)
	}

	if agentErr != nil {
		return fmt.Errorf("agent exited with error: %w", agentErr)
	}

	fmt.Println("Done.")
	return nil
}

func setupAgent(client *sprite.Client, cfg *config.Config) error {
	switch strings.ToLower(cfg.Agent) {
	case "claude":
		// Install claude CLI if not present
		_, err := client.Exec("command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code")
		if err != nil {
			return fmt.Errorf("installing claude: %w", err)
		}
		// Authenticate claude
		if cfg.ClaudeAuth == "apikey" || cfg.AnthropicAPIKey != "" {
			if cfg.AnthropicAPIKey == "" {
				return fmt.Errorf("ANTHROPIC_API_KEY is required when using API key auth for claude")
			}
			_, err = client.Exec(fmt.Sprintf("echo 'export ANTHROPIC_API_KEY=%s' >> /home/sprite/.bashrc", cfg.AnthropicAPIKey))
			if err != nil {
				return fmt.Errorf("setting ANTHROPIC_API_KEY: %w", err)
			}
		}
		return nil

	case "opencode":
		// Install opencode if not present
		_, err := client.Exec("command -v opencode >/dev/null 2>&1 || curl -fsSL https://opencode.ai/install | bash")
		if err != nil {
			return fmt.Errorf("installing opencode: %w", err)
		}
		// Source .env from bashrc
		_, err = client.Exec("grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc")
		if err != nil {
			return fmt.Errorf("adding .env sourcing to bashrc: %w", err)
		}
		// Set API key if provided
		if cfg.AnthropicAPIKey != "" {
			_, err = client.Exec(fmt.Sprintf("echo 'export ANTHROPIC_API_KEY=%s' >> /home/sprite/.bashrc", cfg.AnthropicAPIKey))
			if err != nil {
				return fmt.Errorf("setting ANTHROPIC_API_KEY for opencode: %w", err)
			}
		}
		return nil

	default:
		return fmt.Errorf("unknown agent: %s (supported: claude, opencode)", cfg.Agent)
	}
}

func launchAgent(client *sprite.Client, cfg *config.Config, planFile string) error {
	agent := strings.ToLower(cfg.Agent)

	// Build the agent command
	var agentCmd string
	switch agent {
	case "claude":
		agentCmd = "claude"
		if cfg.Model != "" {
			agentCmd += fmt.Sprintf(" --model %s", cfg.Model)
		}
		if planFile != "" {
			agentCmd += " --plan /home/sprite/plan.md"
		}
	case "opencode":
		model := cfg.Model
		if model == "" {
			model = "opencode/big-pickle"
		}
		if planFile != "" {
			agentCmd = fmt.Sprintf(
				"set -a && source /home/sprite/.env 2>/dev/null && set +a && ~/.opencode/bin/opencode run -m %s 'read plan.md and complete the plan please'",
				model)
		} else {
			agentCmd = fmt.Sprintf("~/.opencode/bin/opencode -m %s", model)
		}
	}

	if agentCmd != "" {
		fmt.Printf("Running: %s\n", agentCmd)
		return client.ExecInteractive("bash", "-lc", fmt.Sprintf("cd /home/sprite && %s", agentCmd))
	}

	// Fallback: open console
	fmt.Println("Opening sprite console...")
	return client.Console()
}
