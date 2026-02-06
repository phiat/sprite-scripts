package sprite

import (
	"context"
	"fmt"
	"time"
)

// RunCheckpointing starts a goroutine that creates checkpoints at the given interval.
// It returns when the context is cancelled. Call this as a goroutine.
// The done channel is closed when the goroutine exits.
func (c *Client) RunCheckpointing(ctx context.Context, interval time.Duration, done chan<- struct{}) {
	defer close(done)

	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	fmt.Printf("Background checkpointing every %v for sprite %s\n", interval, c.SpriteName)

	for {
		select {
		case <-ctx.Done():
			fmt.Println("Checkpointing stopped.")
			return
		case <-ticker.C:
			fmt.Printf("Creating checkpoint for %s...\n", c.SpriteName)
			if err := c.CheckpointCreate(); err != nil {
				fmt.Printf("Warning: checkpoint failed: %v\n", err)
			} else {
				fmt.Println("Checkpoint created successfully.")
			}
		}
	}
}
