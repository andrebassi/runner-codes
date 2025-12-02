package firecracker

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"syscall"
	"time"
)

// Client manages Firecracker VM operations
type Client struct {
	binaryPath string
	kernelPath string
	socketPath string
	httpClient *http.Client
	process    *exec.Cmd
}

// Config holds VM configuration
type Config struct {
	SocketPath string
	KernelPath string
	RootfsPath string
	MemSizeMB  int
	VCPUs      int
	VsockPath  string // Path for vsock UDS (e.g., /tmp/fc-python.vsock)
}

// NewClient creates a new Firecracker client
func NewClient(binaryPath, kernelPath string) *Client {
	return &Client{
		binaryPath: binaryPath,
		kernelPath: kernelPath,
	}
}

// Start starts a new Firecracker VM
func (c *Client) Start(ctx context.Context, cfg Config) error {
	c.socketPath = cfg.SocketPath

	// Remove existing socket
	os.Remove(c.socketPath)

	// Start Firecracker process
	c.process = exec.CommandContext(ctx, c.binaryPath, "--api-sock", c.socketPath)
	c.process.SysProcAttr = &syscall.SysProcAttr{
		Setpgid: true,
	}

	if err := c.process.Start(); err != nil {
		return fmt.Errorf("failed to start firecracker: %w", err)
	}

	// Create HTTP client with Unix socket
	c.httpClient = &http.Client{
		Transport: &http.Transport{
			DialContext: func(_ context.Context, _, _ string) (net.Conn, error) {
				return net.Dial("unix", c.socketPath)
			},
		},
		Timeout: 30 * time.Second,
	}

	// Wait for socket to be ready
	for i := 0; i < 50; i++ {
		if _, err := os.Stat(c.socketPath); err == nil {
			time.Sleep(20 * time.Millisecond)
			return nil
		}
		time.Sleep(10 * time.Millisecond)
	}

	return fmt.Errorf("timeout waiting for firecracker socket")
}

// ConfigureVM configures the VM (kernel, rootfs, etc)
func (c *Client) ConfigureVM(ctx context.Context, cfg Config) error {
	// Set boot source
	// Use infra.operator guest as init process (PID 1) listening on vsock port 5000
	bootSource := map[string]interface{}{
		"kernel_image_path": cfg.KernelPath,
		"boot_args":         "console=ttyS0 reboot=k panic=1 pci=off init=/usr/local/bin/infra.operator -- guest --port 5000",
	}
	if err := c.apiPut(ctx, "/boot-source", bootSource); err != nil {
		return fmt.Errorf("failed to set boot source: %w", err)
	}

	// Set rootfs
	rootfs := map[string]interface{}{
		"drive_id":       "rootfs",
		"path_on_host":   cfg.RootfsPath,
		"is_root_device": true,
		"is_read_only":   false,
	}
	if err := c.apiPut(ctx, "/drives/rootfs", rootfs); err != nil {
		return fmt.Errorf("failed to set rootfs: %w", err)
	}

	// Set machine config
	machineConfig := map[string]interface{}{
		"vcpu_count":   cfg.VCPUs,
		"mem_size_mib": cfg.MemSizeMB,
	}
	if err := c.apiPut(ctx, "/machine-config", machineConfig); err != nil {
		return fmt.Errorf("failed to set machine config: %w", err)
	}

	// Configure vsock for guest communication
	if cfg.VsockPath != "" {
		vsock := map[string]interface{}{
			"guest_cid": 3,
			"uds_path":  cfg.VsockPath,
		}
		if err := c.apiPut(ctx, "/vsock", vsock); err != nil {
			return fmt.Errorf("failed to set vsock: %w", err)
		}
	}

	return nil
}

// StartVM starts the configured VM
func (c *Client) StartVM(ctx context.Context) error {
	action := map[string]string{
		"action_type": "InstanceStart",
	}
	return c.apiPut(ctx, "/actions", action)
}

// PauseVM pauses the VM
func (c *Client) PauseVM(ctx context.Context) error {
	state := map[string]string{
		"state": "Paused",
	}
	return c.apiPatch(ctx, "/vm", state)
}

// ResumeVM resumes a paused VM
func (c *Client) ResumeVM(ctx context.Context) error {
	state := map[string]string{
		"state": "Resumed",
	}
	return c.apiPatch(ctx, "/vm", state)
}

// CreateSnapshot creates a snapshot of the VM
func (c *Client) CreateSnapshot(ctx context.Context, snapshotDir string) error {
	// Ensure directory exists
	if err := os.MkdirAll(snapshotDir, 0755); err != nil {
		return fmt.Errorf("failed to create snapshot dir: %w", err)
	}

	snapshot := map[string]interface{}{
		"snapshot_type": "Full",
		"snapshot_path": filepath.Join(snapshotDir, "vmstate.snapshot"),
		"mem_file_path": filepath.Join(snapshotDir, "mem.snapshot"),
	}
	return c.apiPut(ctx, "/snapshot/create", snapshot)
}

// LoadSnapshot loads a VM from snapshot
func (c *Client) LoadSnapshot(ctx context.Context, snapshotDir string) error {
	snapshot := map[string]interface{}{
		"snapshot_path": filepath.Join(snapshotDir, "vmstate.snapshot"),
		"mem_backend": map[string]string{
			"backend_path": filepath.Join(snapshotDir, "mem.snapshot"),
			"backend_type": "File",
		},
		"enable_diff_snapshots": false,
		"resume_vm":             true,
	}
	return c.apiPut(ctx, "/snapshot/load", snapshot)
}

// Stop stops the Firecracker process
func (c *Client) Stop() error {
	if c.process != nil && c.process.Process != nil {
		// Kill process group
		syscall.Kill(-c.process.Process.Pid, syscall.SIGKILL)
		c.process.Wait()
	}
	os.Remove(c.socketPath)
	return nil
}

// apiPut makes a PUT request to the Firecracker API
func (c *Client) apiPut(ctx context.Context, path string, body interface{}) error {
	return c.apiRequest(ctx, http.MethodPut, path, body)
}

// apiPatch makes a PATCH request to the Firecracker API
func (c *Client) apiPatch(ctx context.Context, path string, body interface{}) error {
	return c.apiRequest(ctx, http.MethodPatch, path, body)
}

// apiRequest makes an HTTP request to the Firecracker API
func (c *Client) apiRequest(ctx context.Context, method, path string, body interface{}) error {
	jsonBody, err := json.Marshal(body)
	if err != nil {
		return fmt.Errorf("failed to marshal body: %w", err)
	}

	req, err := http.NewRequestWithContext(ctx, method, "http://localhost"+path, bytes.NewReader(jsonBody))
	if err != nil {
		return fmt.Errorf("failed to create request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return fmt.Errorf("request failed: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNoContent && resp.StatusCode != http.StatusOK {
		var errBody bytes.Buffer
		errBody.ReadFrom(resp.Body)
		return fmt.Errorf("API error (status %d): %s", resp.StatusCode, errBody.String())
	}

	return nil
}

// SocketPath returns the current socket path
func (c *Client) SocketPath() string {
	return c.socketPath
}
