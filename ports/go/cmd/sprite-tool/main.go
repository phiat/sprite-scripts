package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "sprite-tool",
	Short: "A CLI tool for managing sprites with coding agents",
	Long: `sprite-tool provides commands to launch, push, pull, and watch sprites.
It wraps the sprite CLI with convenience commands for coding agent workflows.`,
}

func main() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
