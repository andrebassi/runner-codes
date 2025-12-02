package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"
)

const (
	defaultVCPUs  = 1
	defaultMemMiB = 512
)

func main() {
	// Parse flags
	kernelPath := flag.String("kernel", "/srv/firecracker/vmlinux", "Path to kernel image")
	rootfsPath := flag.String("rootfs", "/srv/firecracker/rootfs.ext4", "Path to rootfs image")
	lang := flag.String("lang", "python", "Language for code execution (python, node, rust, ruby)")
	code := flag.String("code", "", "Code to execute (if empty, uses demo snippet)")
	timeout := flag.Int("timeout", 10, "Execution timeout in seconds")
	vcpus := flag.Int("vcpus", defaultVCPUs, "Number of vCPUs")
	memMiB := flag.Int("mem", defaultMemMiB, "Memory in MiB")
	vsockPort := flag.Uint("port", DefaultVsockPort, "Vsock port for guest-runner")
	useSnapshot := flag.Bool("snapshot", false, "Use snapshot for warm boot (faster)")
	cacheDir := flag.String("cache-dir", DefaultCacheDir, "Directory for snapshot cache")
	s3Bucket := flag.String("s3-bucket", DefaultS3Bucket, "S3 bucket for snapshots")
	s3Region := flag.String("s3-region", DefaultS3Region, "S3 region")

	flag.Parse()

	// If using snapshot mode, we don't need kernel/rootfs validation
	if !*useSnapshot {
		// Validate inputs for cold boot mode
		if _, err := os.Stat(*kernelPath); os.IsNotExist(err) {
			log.Fatalf("Kernel not found: %s", *kernelPath)
		}
		if _, err := os.Stat(*rootfsPath); os.IsNotExist(err) {
			log.Fatalf("Rootfs not found: %s", *rootfsPath)
		}
	}

	// Use demo code if none provided
	if *code == "" {
		*code = getDemoCode(*lang)
	}

	log.Printf("Starting LLM-Firecracker Host Agent")
	log.Printf("  Language:    %s", *lang)
	log.Printf("  Mode:        %s", map[bool]string{true: "Warm Boot (Snapshot)", false: "Cold Boot"}[*useSnapshot])
	log.Printf("  Vsock Port:  %d", *vsockPort)

	// Setup signal handling for cleanup
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	var vsockUDS string
	var cleanupFunc func()

	if *useSnapshot {
		// Warm boot mode using snapshots
		log.Println("Initializing snapshot cache...")
		cache := NewSnapshotCache(*cacheDir, *s3Bucket, *s3Region)

		log.Printf("Ensuring snapshot is cached for %s...", *lang)
		startCache := time.Now()
		memPath, vmstatePath, err := cache.EnsureCached(*lang)
		if err != nil {
			log.Fatalf("Failed to ensure snapshot cached: %v", err)
		}
		log.Printf("Snapshot ready (cache time: %v)", time.Since(startCache))

		// Generate unique instance ID
		instanceID := fmt.Sprintf("%s-%d", *lang, time.Now().UnixNano())
		apiSocket := fmt.Sprintf("/tmp/fc-%s.sock", instanceID)
		vsockUDS = fmt.Sprintf("/tmp/fc-%s.vsock", instanceID)

		log.Printf("  Instance ID: %s", instanceID)
		log.Printf("  Snapshot:    %s", vmstatePath)

		// Start Firecracker and load snapshot
		log.Println("Starting Firecracker with snapshot restore...")
		startBoot := time.Now()
		cmd, err := StartFirecracker(apiSocket, instanceID)
		if err != nil {
			log.Fatalf("Failed to start firecracker: %v", err)
		}

		client := NewFirecrackerClient(apiSocket)

		// Load snapshot (warm boot)
		if err := client.LoadSnapshot(vmstatePath, memPath); err != nil {
			cmd.Process.Kill()
			os.Remove(apiSocket)
			log.Fatalf("Failed to load snapshot: %v", err)
		}

		log.Printf("VM restored from snapshot in %v", time.Since(startBoot))

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

		// Minimal wait for snapshot restore to stabilize
		time.Sleep(100 * time.Millisecond)

	} else {
		// Cold boot mode (original behavior)
		instanceID := fmt.Sprintf("%d", time.Now().UnixNano())
		apiSocket := fmt.Sprintf("/tmp/fc-%s.sock", instanceID)
		vsockUDS = fmt.Sprintf("/tmp/fc-%s.vsock", instanceID)

		log.Printf("  Instance ID: %s", instanceID)
		log.Printf("  Kernel:      %s", *kernelPath)
		log.Printf("  Rootfs:      %s", *rootfsPath)
		log.Printf("  vCPUs:       %d", *vcpus)
		log.Printf("  Memory:      %d MiB", *memMiB)
		log.Printf("  Vsock UDS:   %s", vsockUDS)

		// Create Firecracker config
		config := FirecrackerConfig{
			InstanceID: instanceID,
			KernelPath: *kernelPath,
			RootfsPath: *rootfsPath,
			VCPUs:      *vcpus,
			MemMiB:     *memMiB,
			APISocket:  apiSocket,
		}

		// Start Firecracker instance
		log.Println("Starting Firecracker instance (cold boot)...")
		startBoot := time.Now()
		cmd, client, err := ProvisionAndStart(config)
		if err != nil {
			log.Fatalf("Failed to start instance: %v", err)
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
	log.Printf("Executing %s code via vsock UDS...", *lang)
	result, err := ExecuteCodeViaUDS(vsockUDS, uint32(*vsockPort), *lang, *code, *timeout)
	if err != nil {
		log.Printf("Execution failed: %v", err)

		// Return error result
		errorResult := RunResult{
			Stdout:   "",
			Stderr:   err.Error(),
			ExitCode: 1,
			Error:    err.Error(),
		}
		printResult(&errorResult)
		os.Exit(1)
	}

	// Print result
	printResult(result)
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

func printResult(result *RunResult) {
	output, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		log.Printf("Failed to marshal result: %v", err)
		return
	}

	fmt.Println("\n=== Execution Result ===")
	fmt.Println(string(output))
}
