package sprite

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
)

// PushFile pushes a local file to a remote path on the sprite.
// It creates parent directories on the remote, then cats the file content.
func (c *Client) PushFile(localPath, remotePath string) error {
	remoteDir := filepath.Dir(remotePath)

	// Create remote directory
	_, err := c.Exec(fmt.Sprintf("mkdir -p %s", remoteDir))
	if err != nil {
		return fmt.Errorf("creating remote dir %s: %w", remoteDir, err)
	}

	if c.DryRun {
		fmt.Printf("[dry-run] push file %s -> %s\n", localPath, remotePath)
		return nil
	}

	// Read local file
	f, err := os.Open(localPath)
	if err != nil {
		return fmt.Errorf("opening local file %s: %w", localPath, err)
	}
	defer f.Close()

	// Pipe file content to remote
	shellCmd := fmt.Sprintf("cat > %s", remotePath)
	_, err = c.ExecWithStdin(shellCmd, f)
	if err != nil {
		return fmt.Errorf("pushing file to %s: %w", remotePath, err)
	}

	return nil
}

// PushDir pushes a local directory to a remote path on the sprite.
// It uses tar to stream the directory contents.
func (c *Client) PushDir(localDir, remoteDir string) error {
	parentDir := filepath.Dir(localDir)
	baseName := filepath.Base(localDir)

	// Create remote parent directory
	remoteParent := filepath.Dir(remoteDir)
	_, err := c.Exec(fmt.Sprintf("mkdir -p %s", remoteParent))
	if err != nil {
		return fmt.Errorf("creating remote parent dir %s: %w", remoteParent, err)
	}

	if c.DryRun {
		fmt.Printf("[dry-run] tar czf - -C %s %s | sprite exec -s %s bash -c \"tar xzf - -C %s\"\n",
			parentDir, baseName, c.SpriteName, remoteParent)
		return nil
	}

	// Create tar of local directory
	tarCmd := exec.Command("tar", "czf", "-", "-C", parentDir, baseName)
	tarOut, err := tarCmd.StdoutPipe()
	if err != nil {
		return fmt.Errorf("creating tar stdout pipe: %w", err)
	}
	tarCmd.Stderr = os.Stderr

	if err := tarCmd.Start(); err != nil {
		return fmt.Errorf("starting tar: %w", err)
	}

	// Pipe tar output to sprite exec
	shellCmd := fmt.Sprintf("tar xzf - -C %s", remoteParent)
	_, err = c.ExecWithStdin(shellCmd, tarOut)
	if err != nil {
		_ = tarCmd.Wait()
		return fmt.Errorf("pushing dir to %s: %w", remoteDir, err)
	}

	if err := tarCmd.Wait(); err != nil {
		return fmt.Errorf("tar command failed: %w", err)
	}

	return nil
}

// PullFile pulls a remote file from the sprite to a local path.
func (c *Client) PullFile(remotePath, localPath string) error {
	// Ensure local directory exists
	localDir := filepath.Dir(localPath)
	if err := os.MkdirAll(localDir, 0o755); err != nil {
		return fmt.Errorf("creating local dir %s: %w", localDir, err)
	}

	if c.DryRun {
		fmt.Printf("[dry-run] pull file %s -> %s\n", remotePath, localPath)
		return nil
	}

	// Open local file for writing
	f, err := os.Create(localPath)
	if err != nil {
		return fmt.Errorf("creating local file %s: %w", localPath, err)
	}
	defer f.Close()

	// Run sprite exec cat and pipe to local file
	err = c.ExecWithStdout(f, "cat", remotePath)
	if err != nil {
		return fmt.Errorf("pulling file %s: %w", remotePath, err)
	}

	return nil
}

// PullDir pulls a remote directory from the sprite to a local path.
func (c *Client) PullDir(remotePath, localPath string) error {
	// Ensure local directory exists
	if err := os.MkdirAll(localPath, 0o755); err != nil {
		return fmt.Errorf("creating local dir %s: %w", localPath, err)
	}

	if c.DryRun {
		fmt.Printf("[dry-run] pull dir %s -> %s\n", remotePath, localPath)
		return nil
	}

	// Get tar stream from remote
	stdout, spriteCmd, err := c.ExecPiped(fmt.Sprintf("tar czf - -C %s .", remotePath))
	if err != nil {
		return fmt.Errorf("starting remote tar: %w", err)
	}

	// Extract locally
	tarCmd := exec.Command("tar", "xzf", "-", "-C", localPath)
	tarCmd.Stdin = stdout
	tarCmd.Stderr = os.Stderr

	if err := tarCmd.Run(); err != nil {
		if spriteCmd != nil {
			_ = spriteCmd.Wait()
		}
		return fmt.Errorf("extracting tar locally: %w", err)
	}

	if spriteCmd != nil {
		if err := spriteCmd.Wait(); err != nil {
			return fmt.Errorf("remote tar command failed: %w", err)
		}
	}

	return nil
}

// IsRemoteDir checks whether a remote path is a directory.
func (c *Client) IsRemoteDir(remotePath string) (bool, error) {
	if c.DryRun {
		fmt.Printf("[dry-run] check if %s is dir on sprite\n", remotePath)
		return false, nil
	}
	out, err := c.Exec(fmt.Sprintf("[ -d %s ] && echo dir || echo file", remotePath))
	if err != nil {
		return false, err
	}
	return out == "dir", nil
}

// PushFileContent pushes raw content (as bytes) to a remote path on the sprite.
func (c *Client) PushFileContent(content []byte, remotePath string) error {
	remoteDir := filepath.Dir(remotePath)

	_, err := c.Exec(fmt.Sprintf("mkdir -p %s", remoteDir))
	if err != nil {
		return fmt.Errorf("creating remote dir %s: %w", remoteDir, err)
	}

	if c.DryRun {
		fmt.Printf("[dry-run] push content -> %s\n", remotePath)
		return nil
	}

	shellCmd := fmt.Sprintf("cat > %s", remotePath)
	_, err = c.ExecWithStdin(shellCmd, io.NopCloser(bytes.NewReader(content)))
	if err != nil {
		return fmt.Errorf("pushing content to %s: %w", remotePath, err)
	}

	return nil
}
