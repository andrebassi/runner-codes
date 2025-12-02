package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/exec"
	"time"
)

// FirecrackerClient manages Firecracker API communication
type FirecrackerClient struct {
	socketPath string
	client     *http.Client
}

// NewFirecrackerClient creates a new Firecracker API client
func NewFirecrackerClient(socketPath string) *FirecrackerClient {
	transport := &http.Transport{
		DialContext: func(_ context.Context, _, _ string) (net.Conn, error) {
			return net.Dial("unix", socketPath)
		},
	}

	return &FirecrackerClient{
		socketPath: socketPath,
		client: &http.Client{
			Transport: transport,
			Timeout:   10 * time.Second,
		},
	}
}

// putJSON sends a PUT request with JSON payload to the Firecracker API
func (fc *FirecrackerClient) putJSON(endpoint string, payload interface{}) error {
	data, err := json.Marshal(payload)
	if err != nil {
		return fmt.Errorf("marshal payload: %w", err)
	}

	url := fmt.Sprintf("http://localhost%s", endpoint)
	req, err := http.NewRequest("PUT", url, bytes.NewReader(data))
	if err != nil {
		return fmt.Errorf("create request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := fc.client.Do(req)
	if err != nil {
		return fmt.Errorf("execute request: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode >= 400 {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("API error %d: %s", resp.StatusCode, string(body))
	}

	return nil
}

// patchJSON sends a PATCH request with JSON payload to the Firecracker API
func (fc *FirecrackerClient) patchJSON(endpoint string, payload interface{}) error {
	data, err := json.Marshal(payload)
	if err != nil {
		return fmt.Errorf("marshal payload: %w", err)
	}

	url := fmt.Sprintf("http://localhost%s", endpoint)
	req, err := http.NewRequest("PATCH", url, bytes.NewReader(data))
	if err != nil {
		return fmt.Errorf("create request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := fc.client.Do(req)
	if err != nil {
		return fmt.Errorf("execute request: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode >= 400 {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("API error %d: %s", resp.StatusCode, string(body))
	}

	return nil
}

// StartFirecracker starts a Firecracker process with the given socket path and instance ID
func StartFirecracker(socketPath, instanceID string) (*exec.Cmd, error) {
	// Remove existing socket if present
	os.Remove(socketPath)

	cmd := exec.Command("/usr/local/bin/firecracker", "--api-sock", socketPath, "--id", instanceID)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("start firecracker: %w", err)
	}

	// Wait for socket to be available
	if err := waitForSocket(socketPath, 5*time.Second); err != nil {
		cmd.Process.Kill()
		return nil, fmt.Errorf("wait for socket: %w", err)
	}

	return cmd, nil
}

// waitForSocket waits for a unix socket to become available
func waitForSocket(socketPath string, timeout time.Duration) error {
	deadline := time.Now().Add(timeout)
	for time.Now().Before(deadline) {
		if _, err := os.Stat(socketPath); err == nil {
			// Try to connect to verify it's ready
			conn, err := net.DialTimeout("unix", socketPath, 100*time.Millisecond)
			if err == nil {
				conn.Close()
				return nil
			}
		}
		time.Sleep(100 * time.Millisecond)
	}
	return fmt.Errorf("socket not ready within %v", timeout)
}

// ConfigureMachine sets the machine configuration
func (fc *FirecrackerClient) ConfigureMachine(vcpus, memMiB int) error {
	config := MachineConfig{
		VCPUCount:  vcpus,
		MemSizeMiB: memMiB,
		SMT:        false,
	}
	return fc.putJSON("/machine-config", config)
}

// ConfigureBootSource sets the kernel and boot arguments
func (fc *FirecrackerClient) ConfigureBootSource(kernelPath, bootArgs string) error {
	if bootArgs == "" {
		bootArgs = "console=ttyS0 reboot=k panic=1 pci=off root=/dev/vda1 rw"
	}
	config := BootSource{
		KernelImagePath: kernelPath,
		BootArgs:        bootArgs,
	}
	return fc.putJSON("/boot-source", config)
}

// ConfigureDrive adds a drive to the VM
func (fc *FirecrackerClient) ConfigureDrive(driveID, path string, isRoot, readOnly bool) error {
	config := Drive{
		DriveID:      driveID,
		PathOnHost:   path,
		IsRootDevice: isRoot,
		IsReadOnly:   readOnly,
	}
	return fc.putJSON(fmt.Sprintf("/drives/%s", driveID), config)
}

// ConfigureVsock configures the vsock device
func (fc *FirecrackerClient) ConfigureVsock(guestCID uint32, udsPath string) error {
	config := VsockConfig{
		GuestCID: guestCID,
		UDSPath:  udsPath,
	}
	return fc.putJSON("/vsock", config)
}

// StartInstance starts the VM
func (fc *FirecrackerClient) StartInstance() error {
	action := Action{ActionType: "InstanceStart"}
	return fc.putJSON("/actions", action)
}

// StopInstance sends Ctrl+Alt+Del to the VM
func (fc *FirecrackerClient) StopInstance() error {
	action := Action{ActionType: "SendCtrlAltDel"}
	return fc.putJSON("/actions", action)
}

// PauseVM pauses the microVM (required before creating snapshot)
func (fc *FirecrackerClient) PauseVM() error {
	return fc.patchJSON("/vm", map[string]string{"state": "Paused"})
}

// ResumeVM resumes a paused microVM
func (fc *FirecrackerClient) ResumeVM() error {
	return fc.patchJSON("/vm", map[string]string{"state": "Resumed"})
}

// LoadSnapshot loads a VM from a snapshot (warm boot)
func (fc *FirecrackerClient) LoadSnapshot(vmstatePath, memPath string) error {
	payload := map[string]interface{}{
		"snapshot_path": vmstatePath,
		"mem_backend": map[string]string{
			"backend_type": "File",
			"backend_path": memPath,
		},
		"enable_diff_snapshots": false,
		"resume_vm":             true,
	}
	return fc.putJSON("/snapshot/load", payload)
}

// CreateSnapshot creates a full snapshot of the VM
func (fc *FirecrackerClient) CreateSnapshot(vmstatePath, memPath string) error {
	payload := map[string]interface{}{
		"snapshot_type": "Full",
		"snapshot_path": vmstatePath,
		"mem_file_path": memPath,
	}
	return fc.putJSON("/snapshot/create", payload)
}

// ProvisionAndStart creates and starts a complete Firecracker instance
func ProvisionAndStart(config FirecrackerConfig) (*exec.Cmd, *FirecrackerClient, error) {
	// Start Firecracker process
	cmd, err := StartFirecracker(config.APISocket, config.InstanceID)
	if err != nil {
		return nil, nil, err
	}

	// Create API client
	client := NewFirecrackerClient(config.APISocket)

	// Configure machine
	if err := client.ConfigureMachine(config.VCPUs, config.MemMiB); err != nil {
		cmd.Process.Kill()
		return nil, nil, fmt.Errorf("configure machine: %w", err)
	}

	// Configure boot source
	if err := client.ConfigureBootSource(config.KernelPath, config.BootArgs); err != nil {
		cmd.Process.Kill()
		return nil, nil, fmt.Errorf("configure boot source: %w", err)
	}

	// Configure root drive
	if err := client.ConfigureDrive("rootfs", config.RootfsPath, true, false); err != nil {
		cmd.Process.Kill()
		return nil, nil, fmt.Errorf("configure drive: %w", err)
	}

	// Configure vsock
	vsockPath := fmt.Sprintf("/tmp/fc-%s.vsock", config.InstanceID)
	if err := client.ConfigureVsock(3, vsockPath); err != nil {
		cmd.Process.Kill()
		return nil, nil, fmt.Errorf("configure vsock: %w", err)
	}

	// Start the instance
	if err := client.StartInstance(); err != nil {
		cmd.Process.Kill()
		return nil, nil, fmt.Errorf("start instance: %w", err)
	}

	return cmd, client, nil
}
