package host

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"
	"time"
)

const (
	defaultVCPUs  = 1
	defaultMemMiB = 512
)

// RunnerConfig holds configuration for the host runner
type RunnerConfig struct {
	KernelPath  string
	RootfsPath  string
	Lang        string
	Code        string
	Timeout     int
	VCPUs       int
	MemMiB      int
	VsockPort   uint
	UseSnapshot bool
	CacheDir    string
	S3Bucket    string
	S3Region    string
}

// DefaultRunnerConfig returns a RunnerConfig with default values
func DefaultRunnerConfig() RunnerConfig {
	return RunnerConfig{
		KernelPath:  "/srv/firecracker/vmlinux",
		RootfsPath:  "/srv/firecracker/rootfs.ext4",
		Lang:        "python",
		Timeout:     10,
		VCPUs:       defaultVCPUs,
		MemMiB:      defaultMemMiB,
		VsockPort:   DefaultVsockPort,
		UseSnapshot: true,
		CacheDir:    DefaultCacheDir,
		S3Bucket:    DefaultS3Bucket,
		S3Region:    DefaultS3Region,
	}
}

// Runner manages Firecracker VM lifecycle and code execution
type Runner struct {
	config RunnerConfig
	cache  *SnapshotCache
}

// NewRunner creates a new host runner
func NewRunner(config RunnerConfig) *Runner {
	return &Runner{
		config: config,
	}
}

// Run executes code in a Firecracker microVM
func (r *Runner) Run() (*RunResult, error) {
	// If using snapshot mode, we don't need kernel/rootfs validation
	if !r.config.UseSnapshot {
		// Validate inputs for cold boot mode
		if _, err := os.Stat(r.config.KernelPath); os.IsNotExist(err) {
			return nil, fmt.Errorf("kernel not found: %s", r.config.KernelPath)
		}
		if _, err := os.Stat(r.config.RootfsPath); os.IsNotExist(err) {
			return nil, fmt.Errorf("rootfs not found: %s", r.config.RootfsPath)
		}
	}

	// Use demo code if none provided
	code := r.config.Code
	if code == "" {
		code = getDemoCode(r.config.Lang)
	}

	log.Printf("Starting Runner Codes Host Runner")
	log.Printf("  Language:    %s", r.config.Lang)
	log.Printf("  Mode:        %s", map[bool]string{true: "Warm Boot (Snapshot)", false: "Cold Boot"}[r.config.UseSnapshot])
	log.Printf("  Vsock Port:  %d", r.config.VsockPort)

	// Setup signal handling for cleanup
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	var vsockUDS string
	var cleanupFunc func()

	if r.config.UseSnapshot {
		// Warm boot mode using snapshots
		log.Println("Initializing snapshot cache...")
		r.cache = NewSnapshotCache(r.config.CacheDir, r.config.S3Bucket, r.config.S3Region)

		log.Printf("Ensuring snapshot is cached for %s...", r.config.Lang)
		startCache := time.Now()
		memPath, vmstatePath, err := r.cache.EnsureCached(r.config.Lang)
		if err != nil {
			return nil, fmt.Errorf("failed to ensure snapshot cached: %v", err)
		}
		log.Printf("Snapshot ready (cache time: %v)", time.Since(startCache))

		// Clean up old vsock files before starting
		cleanupOldVsockFiles()

		// Generate unique instance ID
		instanceID := fmt.Sprintf("%s-%d", r.config.Lang, time.Now().UnixNano())
		apiSocket := fmt.Sprintf("/tmp/fc-%s.sock", instanceID)

		log.Printf("  Instance ID: %s", instanceID)
		log.Printf("  Snapshot:    %s", vmstatePath)

		// Start Firecracker and load snapshot
		log.Println("Starting Firecracker with snapshot restore...")
		startBoot := time.Now()
		cmd, err := StartFirecracker(apiSocket, instanceID)
		if err != nil {
			return nil, fmt.Errorf("failed to start firecracker: %v", err)
		}

		client := NewFirecrackerClient(apiSocket)

		// Load snapshot (warm boot)
		if err := client.LoadSnapshot(vmstatePath, memPath); err != nil {
			cmd.Process.Kill()
			os.Remove(apiSocket)
			return nil, fmt.Errorf("failed to load snapshot: %v", err)
		}

		log.Printf("VM restored from snapshot in %v", time.Since(startBoot))

		// Wait for vsock UDS to be created by Firecracker
		time.Sleep(100 * time.Millisecond)

		// Find the vsock UDS created by the restored snapshot
		vsockUDS, err = findVsockUDS(startBoot)
		if err != nil {
			cmd.Process.Kill()
			os.Remove(apiSocket)
			return nil, fmt.Errorf("failed to find vsock UDS: %v", err)
		}
		log.Printf("  Vsock UDS:   %s", vsockUDS)

		cleanupFunc = func() {
			log.Println("Shutting down instance...")
			client.StopInstance()
			cmd.Process.Kill()
			cmd.Wait()
			os.Remove(apiSocket)
			os.Remove(vsockUDS)
			log.Println("Instance terminated")
		}

		// Handle signals in background
		go func() {
			<-sigChan
			log.Println("Received shutdown signal")
			cleanupFunc()
			os.Exit(0)
		}()

	} else {
		// Cold boot mode (original behavior)
		instanceID := fmt.Sprintf("%d", time.Now().UnixNano())
		apiSocket := fmt.Sprintf("/tmp/fc-%s.sock", instanceID)
		vsockUDS = fmt.Sprintf("/tmp/fc-%s.vsock", instanceID)

		log.Printf("  Instance ID: %s", instanceID)
		log.Printf("  Kernel:      %s", r.config.KernelPath)
		log.Printf("  Rootfs:      %s", r.config.RootfsPath)
		log.Printf("  vCPUs:       %d", r.config.VCPUs)
		log.Printf("  Memory:      %d MiB", r.config.MemMiB)
		log.Printf("  Vsock UDS:   %s", vsockUDS)

		// Create Firecracker config
		config := FirecrackerConfig{
			InstanceID: instanceID,
			KernelPath: r.config.KernelPath,
			RootfsPath: r.config.RootfsPath,
			VCPUs:      r.config.VCPUs,
			MemMiB:     r.config.MemMiB,
			APISocket:  apiSocket,
		}

		// Start Firecracker instance
		log.Println("Starting Firecracker instance (cold boot)...")
		startBoot := time.Now()
		cmd, client, err := ProvisionAndStart(config)
		if err != nil {
			return nil, fmt.Errorf("failed to start instance: %v", err)
		}

		log.Printf("VM started in %v", time.Since(startBoot))

		cleanupFunc = func() {
			log.Println("Shutting down instance...")
			client.StopInstance()
			cmd.Process.Kill()
			cmd.Wait()
			os.Remove(apiSocket)
			log.Println("Instance terminated")
		}

		// Handle signals in background
		go func() {
			<-sigChan
			log.Println("Received shutdown signal")
			cleanupFunc()
			os.Exit(0)
		}()

		log.Println("Instance started, waiting for guest to boot...")

		// Wait for guest to boot and guest-runner to start
		time.Sleep(3 * time.Second)
	}

	// Cleanup on exit
	defer cleanupFunc()

	// Execute code via vsock UDS
	log.Printf("Executing %s code via vsock UDS...", r.config.Lang)
	result, err := ExecuteCodeViaUDS(vsockUDS, uint32(r.config.VsockPort), r.config.Lang, code, r.config.Timeout)
	if err != nil {
		log.Printf("Execution failed: %v", err)
		errMsg := err.Error()
		return &RunResult{
			Status:   "failed",
			Stdout:   "",
			Stderr:   errMsg,
			ExitCode: 1,
			Error:    errMsg,
		}, nil
	}

	return result, nil
}

// FormattedRunResult is a copy of RunResult with ExecutionTime as formatted string
type FormattedRunResult struct {
	TraceID       string      `json:"trace_id,omitempty"`
	Status        string      `json:"status"`
	Stdout        string      `json:"stdout"`
	Stderr        interface{} `json:"stderr"`
	Exception     interface{} `json:"exception"`
	ExecutionTime string      `json:"executionTime"` // Formatted as "~Xms" or "~X.Xs"
	ExitCode      int         `json:"exit_code,omitempty"`
	Error         string      `json:"error,omitempty"`
	Stdin         string      `json:"stdin,omitempty"`
}

// formatExecutionTime formats milliseconds as human-readable string
func formatExecutionTime(ms int64) string {
	if ms >= 1000 {
		// Format as seconds with 1 decimal place
		return fmt.Sprintf("~%.1fs", float64(ms)/1000.0)
	}
	return fmt.Sprintf("~%dms", ms)
}

// PrintResult prints the result in JSON format with formatted execution time
func PrintResult(result *RunResult) {
	formatted := FormattedRunResult{
		TraceID:       result.TraceID,
		Status:        result.Status,
		Stdout:        result.Stdout,
		Stderr:        result.Stderr,
		Exception:     result.Exception,
		ExecutionTime: formatExecutionTime(result.ExecutionTime),
		ExitCode:      result.ExitCode,
		Error:         result.Error,
		Stdin:         result.Stdin,
	}

	output, err := json.MarshalIndent(formatted, "", "  ")
	if err != nil {
		log.Printf("Failed to marshal result: %v", err)
		return
	}

	fmt.Println("\n=== Execution Result ===")
	fmt.Println(string(output))
}

func getDemoCode(lang string) string {
	demos := map[string]string{
		"python": `print("Hello from Firecracker microVM!")
import sys
print(f"Python version: {sys.version}")`,

		"node": `console.log("Hello from Firecracker microVM!");
console.log("Node version:", process.version);`,

		"rust": `fn main() {
    println!("Hello from Firecracker microVM!");
}`,

		"ruby": `puts "Hello from Firecracker microVM!"
puts "Ruby version: #{RUBY_VERSION}"`,
	}

	if code, ok := demos[lang]; ok {
		return code
	}
	return `print("Hello from Firecracker microVM!")`
}

// findVsockUDS searches for vsock UDS files created by Firecracker.
// When a snapshot is restored, Firecracker recreates the vsock UDS at the
// path that was embedded in the snapshot. This function finds vsock UDS
// files created after the specified time (when Firecracker started).
func findVsockUDS(startTime time.Time) (string, error) {
	pattern := "/tmp/fc-*.vsock"

	// Try multiple times since the vsock UDS might take a moment to appear
	var matches []string
	var err error
	for i := 0; i < 10; i++ {
		matches, err = filepath.Glob(pattern)
		if err != nil {
			return "", fmt.Errorf("glob vsock files: %v", err)
		}
		if len(matches) > 0 {
			break
		}
		time.Sleep(100 * time.Millisecond)
	}

	if len(matches) == 0 {
		return "", fmt.Errorf("no vsock UDS files found matching %s", pattern)
	}

	// Find vsock files created AFTER Firecracker started
	var validPaths []string
	for _, path := range matches {
		info, err := os.Stat(path)
		if err != nil {
			continue
		}
		// Only consider files created after startTime (with 1 second tolerance)
		if info.ModTime().After(startTime.Add(-1 * time.Second)) {
			validPaths = append(validPaths, path)
			log.Printf("Found vsock candidate: %s (created %v ago)", path, time.Since(info.ModTime()).Round(time.Millisecond))
		}
	}

	if len(validPaths) == 0 {
		return "", fmt.Errorf("no vsock UDS files found created after %v (found %d stale files)", startTime.Format(time.RFC3339), len(matches))
	}

	// If multiple valid paths, use the most recently modified one
	var newestPath string
	var newestTime time.Time
	for _, path := range validPaths {
		info, _ := os.Stat(path)
		if newestPath == "" || info.ModTime().After(newestTime) {
			newestPath = path
			newestTime = info.ModTime()
		}
	}

	return newestPath, nil
}

// cleanupOldVsockFiles removes old vsock UDS files from /tmp
func cleanupOldVsockFiles() {
	pattern := "/tmp/fc-*.vsock"
	matches, err := filepath.Glob(pattern)
	if err != nil {
		return
	}
	for _, path := range matches {
		os.Remove(path)
	}
	// Also clean up old socket files
	pattern = "/tmp/fc-*.sock"
	matches, _ = filepath.Glob(pattern)
	for _, path := range matches {
		os.Remove(path)
	}
}
