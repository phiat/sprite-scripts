package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"sprite-tool/internal/config"
	"sprite-tool/internal/sprite"

	"github.com/spf13/cobra"
)

var pushCmd = &cobra.Command{
	Use:   "push local-path remote-path [sprite-name]",
	Short: "Push a local file or directory to a sprite",
	Long: `Push copies a local file or directory to the specified path on the sprite.
Directories are transferred using tar. The sprite name can be provided as
a third argument or set via the SPRITE_NAME environment variable.`,
	Args: cobra.RangeArgs(2, 3),
	RunE: runPush,
}

func init() {
	rootCmd.AddCommand(pushCmd)
}

func runPush(cmd *cobra.Command, args []string) error {
	localPath := args[0]
	remotePath := args[1]

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

	info, err := os.Stat(absLocal)
	if err != nil {
		return fmt.Errorf("local path %s: %w", absLocal, err)
	}

	if info.IsDir() {
		fmt.Printf("Pushing directory %s -> %s on sprite %s...\n", absLocal, remotePath, spriteName)
		// For directory push: tar up the directory and extract at remote destination
		// Use strip-components to place contents directly in remotePath
		return pushDirectory(client, absLocal, remotePath)
	}

	fmt.Printf("Pushing file %s -> %s on sprite %s...\n", absLocal, remotePath, spriteName)
	return client.PushFile(absLocal, remotePath)
}

func pushDirectory(client *sprite.Client, localDir, remotePath string) error {
	parentDir := filepath.Dir(localDir)
	baseName := filepath.Base(localDir)

	// Create remote destination
	_, err := client.Exec(fmt.Sprintf("mkdir -p %s", remotePath))
	if err != nil {
		return fmt.Errorf("creating remote dir: %w", err)
	}

	if client.DryRun {
		fmt.Printf("[dry-run] tar czf - -C %s %s | sprite exec -s %s bash -c \"tar xzf - -C %s --strip-components=1\"\n",
			parentDir, baseName, client.SpriteName, remotePath)
		return nil
	}

	// Create tar from local directory
	tarCmd := exec.Command("tar", "czf", "-", "-C", parentDir, baseName)
	tarOut, err := tarCmd.StdoutPipe()
	if err != nil {
		return fmt.Errorf("creating tar pipe: %w", err)
	}
	tarCmd.Stderr = os.Stderr

	if err := tarCmd.Start(); err != nil {
		return fmt.Errorf("starting tar: %w", err)
	}

	// Extract on remote with strip-components
	shellCmd := fmt.Sprintf("tar xzf - -C %s --strip-components=1", remotePath)
	_, err = client.ExecWithStdin(shellCmd, tarOut)
	if err != nil {
		_ = tarCmd.Wait()
		return fmt.Errorf("extracting on remote: %w", err)
	}

	return tarCmd.Wait()
}

// resolveSpriteName gets the sprite name from args or environment.
func resolveSpriteName(args []string, index int) (string, error) {
	if len(args) > index {
		return args[index], nil
	}
	name := os.Getenv("SPRITE_NAME")
	if name == "" {
		return "", fmt.Errorf("sprite name not provided and SPRITE_NAME not set")
	}
	return name, nil
}
