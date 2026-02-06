package main

import (
	"fmt"
	"os"
	"path/filepath"

	"sprite-tool/internal/config"
	"sprite-tool/internal/sprite"

	"github.com/spf13/cobra"
)

var pullCmd = &cobra.Command{
	Use:   "pull remote-path local-path [sprite-name]",
	Short: "Pull a file or directory from a sprite",
	Long: `Pull copies a file or directory from the sprite to a local path.
Automatically detects whether the remote path is a file or directory.
The sprite name can be provided as a third argument or set via the
SPRITE_NAME environment variable.`,
	Args: cobra.RangeArgs(2, 3),
	RunE: runPull,
}

func init() {
	rootCmd.AddCommand(pullCmd)
}

func runPull(cmd *cobra.Command, args []string) error {
	remotePath := args[0]
	localPath := args[1]

	spriteName, err := resolveSpriteName(args, 2)
	if err != nil {
		return err
	}

	// Load config for .env
	_, _ = config.Load()

	client := sprite.NewClient(spriteName, false)

	// Resolve local path to absolute
	absLocal, err := filepath.Abs(localPath)
	if err != nil {
		return fmt.Errorf("resolving local path: %w", err)
	}

	// Check if remote path is a directory
	isDir, err := client.IsRemoteDir(remotePath)
	if err != nil {
		return fmt.Errorf("checking remote path type: %w", err)
	}

	if isDir {
		fmt.Printf("Pulling directory %s -> %s from sprite %s...\n", remotePath, absLocal, spriteName)
		if err := os.MkdirAll(absLocal, 0o755); err != nil {
			return fmt.Errorf("creating local dir: %w", err)
		}
		return client.PullDir(remotePath, absLocal)
	}

	fmt.Printf("Pulling file %s -> %s from sprite %s...\n", remotePath, absLocal, spriteName)
	return client.PullFile(remotePath, absLocal)
}
