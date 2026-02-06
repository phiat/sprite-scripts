package main

import (
	"fmt"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"sprite-tool/internal/config"
	"sprite-tool/internal/sprite"

	"github.com/spf13/cobra"
)

var watchCmd = &cobra.Command{
	Use:   "watch sprite-name [task-id] [poll-interval]",
	Short: "Poll a beads task for progress",
	Long: `Watch monitors a beads task on the sprite, showing task status and recent
comments. It auto-detects the task if not specified, preferring critical
priority tasks. The poll interval defaults to 5 seconds.`,
	Args: cobra.RangeArgs(1, 3),
	RunE: runWatch,
}

func init() {
	rootCmd.AddCommand(watchCmd)
}

func runWatch(cmd *cobra.Command, args []string) error {
	spriteName := args[0]
	var taskID string
	pollInterval := 5 * time.Second

	if len(args) > 1 {
		taskID = args[1]
	}
	if len(args) > 2 {
		d, err := time.ParseDuration(args[2] + "s")
		if err != nil {
			return fmt.Errorf("invalid poll interval %q: %w", args[2], err)
		}
		pollInterval = d
	}

	// Load config for .env
	_, _ = config.Load()

	client := sprite.NewClient(spriteName, false)

	// Auto-detect task ID if not provided
	if taskID == "" {
		detected, err := detectTaskID(client)
		if err != nil {
			return fmt.Errorf("auto-detecting task ID: %w", err)
		}
		if detected == "" {
			return fmt.Errorf("no tasks found on sprite %s", spriteName)
		}
		taskID = detected
		fmt.Printf("Auto-detected task: %s\n", taskID)
	}

	// Set up signal handling for clean exit
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGINT, syscall.SIGTERM)

	fmt.Printf("Watching task %s on sprite %s (poll every %v)...\n", taskID, spriteName, pollInterval)
	fmt.Println("Press Ctrl+C to stop.")
	time.Sleep(1 * time.Second)

	for {
		// Clear screen using ANSI escape codes
		fmt.Print("\033[2J\033[H")

		// Show task status
		fmt.Printf("=== Task %s on %s === [%s]\n\n", taskID, spriteName, time.Now().Format("15:04:05"))

		status, err := client.Exec(fmt.Sprintf("bd show %s", taskID))
		if err != nil {
			fmt.Printf("Error getting task status: %v\n", err)
		} else {
			fmt.Println(status)
		}

		fmt.Println("\n--- Recent Comments ---")
		comments, err := client.Exec(fmt.Sprintf("bd comments %s | tail -8", taskID))
		if err != nil {
			fmt.Printf("Error getting comments: %v\n", err)
		} else {
			if comments == "" {
				fmt.Println("(no comments yet)")
			} else {
				fmt.Println(comments)
			}
		}

		// Check if task is done
		if isTaskDone(status) {
			fmt.Println("\n=== Task completed! ===")
			return nil
		}

		// Wait for next poll or signal
		select {
		case <-sigCh:
			fmt.Println("\nStopped watching.")
			return nil
		case <-time.After(pollInterval):
			// continue loop
		}
	}
}

// detectTaskID tries to find a task ID on the sprite.
// First tries critical priority tasks, then falls back to any task.
func detectTaskID(client *sprite.Client) (string, error) {
	// Try critical priority first
	out, err := client.Exec("bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'")
	if err == nil && strings.TrimSpace(out) != "" {
		return strings.TrimSpace(out), nil
	}

	// Fallback to any task
	out, err = client.Exec("bd list 2>/dev/null | head -1 | awk '{print $1}'")
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(out), nil
}

// isTaskDone checks if the task status indicates completion.
func isTaskDone(status string) bool {
	lower := strings.ToLower(status)
	doneKeywords := []string{"closed", "done", "completed"}
	for _, kw := range doneKeywords {
		if strings.Contains(lower, kw) {
			return true
		}
	}
	return false
}
