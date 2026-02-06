package sprite

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
)

// Client wraps interactions with the sprite CLI.
type Client struct {
	SpriteName string
	DryRun     bool
}

// NewClient creates a new sprite client for the given sprite name.
func NewClient(spriteName string, dryRun bool) *Client {
	return &Client{
		SpriteName: spriteName,
		DryRun:     dryRun,
	}
}

// Exec runs `sprite exec -s <sprite> bash -c "<cmd>"` and returns combined output.
// In dry-run mode it prints the command and returns empty output.
func (c *Client) Exec(cmd string) (string, error) {
	args := []string{"exec", "-s", c.SpriteName, "bash", "-c", cmd}
	if c.DryRun {
		fmt.Printf("[dry-run] sprite %s\n", strings.Join(args, " "))
		return "", nil
	}
	out, err := exec.Command("sprite", args...).CombinedOutput()
	if err != nil {
		return string(out), fmt.Errorf("sprite exec failed: %w\noutput: %s", err, string(out))
	}
	return strings.TrimSpace(string(out)), nil
}

// ExecWithStdin runs `sprite exec -s <sprite> bash -c "<cmd>"` piping stdin.
func (c *Client) ExecWithStdin(cmd string, stdin io.Reader) (string, error) {
	args := []string{"exec", "-s", c.SpriteName, "bash", "-c", cmd}
	if c.DryRun {
		fmt.Printf("[dry-run] sprite %s\n", strings.Join(args, " "))
		return "", nil
	}
	command := exec.Command("sprite", args...)
	command.Stdin = stdin
	out, err := command.CombinedOutput()
	if err != nil {
		return string(out), fmt.Errorf("sprite exec with stdin failed: %w\noutput: %s", err, string(out))
	}
	return strings.TrimSpace(string(out)), nil
}

// ExecWithStdout runs `sprite exec -s <sprite> <args...>` and pipes stdout to the given writer.
func (c *Client) ExecWithStdout(stdout io.Writer, cmdArgs ...string) error {
	args := append([]string{"exec", "-s", c.SpriteName}, cmdArgs...)
	if c.DryRun {
		fmt.Printf("[dry-run] sprite %s\n", strings.Join(args, " "))
		return nil
	}
	command := exec.Command("sprite", args...)
	command.Stdout = stdout
	command.Stderr = os.Stderr
	return command.Run()
}

// ExecInteractive runs `sprite exec -s <sprite> <args...>` with full terminal I/O.
func (c *Client) ExecInteractive(cmdArgs ...string) error {
	args := append([]string{"exec", "-s", c.SpriteName}, cmdArgs...)
	if c.DryRun {
		fmt.Printf("[dry-run] sprite %s\n", strings.Join(args, " "))
		return nil
	}
	command := exec.Command("sprite", args...)
	command.Stdin = os.Stdin
	command.Stdout = os.Stdout
	command.Stderr = os.Stderr
	return command.Run()
}

// Run runs a plain `sprite <args...>` command and returns combined output.
func (c *Client) Run(args ...string) (string, error) {
	if c.DryRun {
		fmt.Printf("[dry-run] sprite %s\n", strings.Join(args, " "))
		return "", nil
	}
	out, err := exec.Command("sprite", args...).CombinedOutput()
	if err != nil {
		return string(out), fmt.Errorf("sprite %s failed: %w\noutput: %s", strings.Join(args, " "), err, string(out))
	}
	return strings.TrimSpace(string(out)), nil
}

// RunPassthrough runs a plain `sprite <args...>` command with stdout/stderr attached to the terminal.
func (c *Client) RunPassthrough(args ...string) error {
	if c.DryRun {
		fmt.Printf("[dry-run] sprite %s\n", strings.Join(args, " "))
		return nil
	}
	command := exec.Command("sprite", args...)
	command.Stdin = os.Stdin
	command.Stdout = os.Stdout
	command.Stderr = os.Stderr
	return command.Run()
}

// Exists checks if the sprite already exists by parsing `sprite ls` output.
func (c *Client) Exists() (bool, error) {
	if c.DryRun {
		fmt.Printf("[dry-run] sprite ls (checking for %s)\n", c.SpriteName)
		return false, nil
	}
	out, err := exec.Command("sprite", "ls").CombinedOutput()
	if err != nil {
		return false, fmt.Errorf("sprite ls failed: %w\noutput: %s", err, string(out))
	}
	lines := strings.Split(string(out), "\n")
	for _, line := range lines {
		if strings.Contains(line, c.SpriteName) {
			return true, nil
		}
	}
	return false, nil
}

// Create creates a new sprite with `sprite create -skip-console <name>`.
func (c *Client) Create() error {
	if c.DryRun {
		fmt.Printf("[dry-run] sprite create -skip-console %s\n", c.SpriteName)
		return nil
	}
	cmd := exec.Command("sprite", "create", "-skip-console", c.SpriteName)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// CheckpointCreate creates a checkpoint of the sprite.
func (c *Client) CheckpointCreate() error {
	if c.DryRun {
		fmt.Printf("[dry-run] sprite checkpoint create -s %s\n", c.SpriteName)
		return nil
	}
	cmd := exec.Command("sprite", "checkpoint", "create", "-s", c.SpriteName)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// Console opens a console to the sprite.
func (c *Client) Console() error {
	if c.DryRun {
		fmt.Printf("[dry-run] sprite console %s\n", c.SpriteName)
		return nil
	}
	cmd := exec.Command("sprite", "console", c.SpriteName)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// EnsureCLI checks if sprite CLI is installed, and installs it if not.
func EnsureCLI(dryRun bool) error {
	_, err := exec.LookPath("sprite")
	if err == nil {
		return nil
	}
	fmt.Println("sprite CLI not found, installing...")
	if dryRun {
		fmt.Println("[dry-run] curl -fsSL https://sprites.dev/install.sh | sh")
		return nil
	}
	cmd := exec.Command("bash", "-c", "curl -fsSL https://sprites.dev/install.sh | sh")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// Auth authenticates with the sprite service using a token or interactive login.
func Auth(token string, dryRun bool) error {
	if token != "" {
		if dryRun {
			fmt.Println("[dry-run] sprite auth setup --token <TOKEN>")
			return nil
		}
		cmd := exec.Command("sprite", "auth", "setup", "--token", token)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	}
	if dryRun {
		fmt.Println("[dry-run] sprite login")
		return nil
	}
	cmd := exec.Command("sprite", "login")
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// ExecPiped runs `sprite exec -s <sprite> bash -c "<cmd>"` and returns stdout as a reader
// while attaching stderr to os.Stderr. The caller must call Wait() on the returned cmd.
func (c *Client) ExecPiped(shellCmd string) (io.ReadCloser, *exec.Cmd, error) {
	args := []string{"exec", "-s", c.SpriteName, "bash", "-c", shellCmd}
	if c.DryRun {
		fmt.Printf("[dry-run] sprite %s\n", strings.Join(args, " "))
		return io.NopCloser(bytes.NewReader(nil)), nil, nil
	}
	command := exec.Command("sprite", args...)
	command.Stderr = os.Stderr
	stdout, err := command.StdoutPipe()
	if err != nil {
		return nil, nil, fmt.Errorf("creating stdout pipe: %w", err)
	}
	if err := command.Start(); err != nil {
		return nil, nil, fmt.Errorf("starting command: %w", err)
	}
	return stdout, command, nil
}
