package main

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"text/tabwriter"
	"time"

	"infra-operator/internal/config"
	"infra-operator/internal/rootfs"
	"infra-operator/internal/s3"
	"infra-operator/internal/snapshot"
	"infra-operator/pkg/api"
)

var cfg = config.DefaultConfig()

func init() {
	cfg.LoadFromEnv()
}

// ========================
// ROOTFS HANDLERS
// ========================

// createRootfsWithArgs creates a rootfs using command line arguments (no hardcoded config) - legacy
func createRootfsWithArgs(name string, size int, packages, postInstall, output string) error {
	return createRootfsWithBase(name, size, "ubuntu", packages, postInstall, output)
}

// createRootfsFromDocker creates a rootfs from an official Docker image (RECOMMENDED)
func createRootfsFromDocker(name string, size int, dockerImage, output string) error {
	if name == "" {
		return fmt.Errorf("--name is required")
	}
	if dockerImage == "" {
		return fmt.Errorf("--image is required")
	}

	builder := rootfs.NewBuilder(cfg)
	ctx := context.Background()

	return builder.BuildFromDocker(ctx, name, size, dockerImage, output)
}

// createRootfsWithBase creates a rootfs with specified base OS (alpine, debian, ubuntu)
func createRootfsWithBase(name string, size int, base, packages, postInstall, output string) error {
	if name == "" {
		return fmt.Errorf("--name is required")
	}

	// Validate base
	var baseType rootfs.BaseType
	switch strings.ToLower(base) {
	case "alpine":
		baseType = rootfs.BaseAlpine
	case "debian":
		baseType = rootfs.BaseDebian
	case "ubuntu":
		baseType = rootfs.BaseUbuntu
	default:
		return fmt.Errorf("invalid base: %s (must be alpine, debian, or ubuntu)", base)
	}

	builder := rootfs.NewBuilder(cfg)
	ctx := context.Background()

	fmt.Printf("Creating rootfs for %s (%s)...\n", name, base)
	fmt.Printf("  Base: %s\n", base)
	fmt.Printf("  Size: %d MB\n", size)
	fmt.Printf("  Packages: %s\n", packages)
	if postInstall != "" {
		fmt.Printf("  Post-install: %s\n", postInstall)
	}

	return builder.BuildWithBase(ctx, name, size, baseType, packages, postInstall, output)
}

func listRootfs(remote bool) error {
	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)

	if remote {
		// List from S3
		client, err := s3.NewClient(cfg.S3Bucket, cfg.S3Region)
		if err != nil {
			return fmt.Errorf("failed to create S3 client: %w", err)
		}

		objects, err := client.ListRootfs(context.Background())
		if err != nil {
			return fmt.Errorf("failed to list S3 objects: %w", err)
		}

		fmt.Fprintf(w, "LANGUAGE\tSIZE\tLAST MODIFIED\tS3 KEY\n")
		for _, obj := range objects {
			// Extract language from key
			lang := strings.TrimPrefix(obj.Key, "rootfs-")
			lang = strings.TrimSuffix(lang, ".ext4")
			fmt.Fprintf(w, "%s\t%s\t%s\t%s\n",
				lang,
				formatSize(obj.Size),
				obj.LastModified.Format("2006-01-02 15:04"),
				obj.Key,
			)
		}
	} else {
		// List local
		builder := rootfs.NewBuilder(cfg)
		images, err := builder.List()
		if err != nil {
			return fmt.Errorf("failed to list local rootfs: %w", err)
		}

		fmt.Fprintf(w, "LANGUAGE\tSIZE\tPATH\n")
		for _, img := range images {
			fmt.Fprintf(w, "%s\t%s\t%s\n",
				img.Language,
				formatSize(img.Size),
				img.Path,
			)
		}
	}

	w.Flush()
	return nil
}

func uploadRootfs(lang, bucket string) error {
	client, err := s3.NewClient(cfg.S3Bucket, cfg.S3Region)
	if err != nil {
		return fmt.Errorf("failed to create S3 client: %w", err)
	}

	localPath := cfg.RootfsPath(lang)
	if _, err := os.Stat(localPath); os.IsNotExist(err) {
		return fmt.Errorf("rootfs not found: %s", localPath)
	}

	fmt.Printf("Uploading %s to S3...\n", localPath)
	ctx := context.Background()

	if err := client.UploadRootfs(ctx, lang, localPath); err != nil {
		return fmt.Errorf("upload failed: %w", err)
	}

	fmt.Printf("Uploaded successfully: s3://%s/rootfs-%s.ext4\n", cfg.S3Bucket, lang)
	return nil
}

func downloadRootfs(lang, bucket, output string) error {
	client, err := s3.NewClient(cfg.S3Bucket, cfg.S3Region)
	if err != nil {
		return fmt.Errorf("failed to create S3 client: %w", err)
	}

	if output == "" {
		output = cfg.RootfsPath(lang)
	}

	fmt.Printf("Downloading rootfs-%s.ext4 from S3...\n", lang)
	ctx := context.Background()

	if err := client.DownloadRootfs(ctx, lang, output); err != nil {
		return fmt.Errorf("download failed: %w", err)
	}

	fmt.Printf("Downloaded successfully: %s\n", output)
	return nil
}

// ========================
// SNAPSHOT HANDLERS
// ========================

func createSnapshot(lang, rootfsPath, kernelPath, firecrackerPath string, memMB, vcpus int) error {
	// Check if Firecracker exists, download if missing
	if _, err := os.Stat(firecrackerPath); os.IsNotExist(err) {
		fmt.Printf("Firecracker not found at %s, downloading v1.7.0...\n", firecrackerPath)
		fcURL := "https://github.com/firecracker-microvm/firecracker/releases/download/v1.7.0/firecracker-v1.7.0-x86_64.tgz"

		// Ensure directory exists
		fcDir := filepath.Dir(firecrackerPath)
		if err := os.MkdirAll(fcDir, 0755); err != nil {
			return fmt.Errorf("failed to create firecracker directory: %w", err)
		}

		if err := downloadAndExtractFirecracker(fcURL, firecrackerPath); err != nil {
			return fmt.Errorf("failed to download firecracker: %w", err)
		}
		fmt.Printf("Firecracker downloaded to %s\n", firecrackerPath)
	}

	// Check if kernel exists, download if missing
	if _, err := os.Stat(kernelPath); os.IsNotExist(err) {
		fmt.Printf("Kernel not found at %s, downloading kernel 5.10...\n", kernelPath)
		kernelURL := "https://storage.googleapis.com/fireactions/kernels/amd64/5.10/vmlinux"

		// Ensure directory exists
		kernelDir := filepath.Dir(kernelPath)
		if err := os.MkdirAll(kernelDir, 0755); err != nil {
			return fmt.Errorf("failed to create kernel directory: %w", err)
		}

		if err := downloadFile(kernelURL, kernelPath); err != nil {
			return fmt.Errorf("failed to download kernel: %w", err)
		}
		os.Chmod(kernelPath, 0755)
		fmt.Printf("Kernel downloaded to %s\n", kernelPath)
	}

	// Update config with paths
	cfg.Firecracker.BinaryPath = firecrackerPath
	cfg.Firecracker.KernelPath = kernelPath

	manager := snapshot.NewManager(cfg)
	ctx := context.Background()

	return manager.Create(ctx, lang, memMB, vcpus)
}

func listSnapshots(remote bool) error {
	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)

	if remote {
		// List from S3
		client, err := s3.NewClient(cfg.S3Bucket, cfg.S3Region)
		if err != nil {
			return fmt.Errorf("failed to create S3 client: %w", err)
		}

		objects, err := client.ListObjects(context.Background(), "snapshots/")
		if err != nil {
			return fmt.Errorf("failed to list S3 objects: %w", err)
		}

		// Group by language
		langMap := make(map[string]struct {
			vmstate int64
			mem     int64
		})

		for _, obj := range objects {
			parts := strings.Split(obj.Key, "/")
			if len(parts) >= 3 {
				lang := parts[1]
				entry := langMap[lang]
				if strings.HasSuffix(obj.Key, "vmstate.snapshot") {
					entry.vmstate = obj.Size
				} else if strings.HasSuffix(obj.Key, "mem.snapshot") {
					entry.mem = obj.Size
				}
				langMap[lang] = entry
			}
		}

		fmt.Fprintf(w, "LANGUAGE\tVMSTATE\tMEMORY\tS3 PATH\n")
		for lang, info := range langMap {
			fmt.Fprintf(w, "%s\t%s\t%s\ts3://%s/snapshots/%s/\n",
				lang,
				formatSize(info.vmstate),
				formatSize(info.mem),
				cfg.S3Bucket,
				lang,
			)
		}
	} else {
		// List local
		manager := snapshot.NewManager(cfg)
		snapshots, err := manager.List()
		if err != nil {
			return fmt.Errorf("failed to list local snapshots: %w", err)
		}

		fmt.Fprintf(w, "LANGUAGE\tVMSTATE\tMEMORY\tPATH\n")
		for _, snap := range snapshots {
			fmt.Fprintf(w, "%s\t%s\t%s\t%s\n",
				snap.Language,
				formatSize(snap.VmstateSize),
				formatSize(snap.MemSize),
				snap.Path,
			)
		}
	}

	w.Flush()
	return nil
}

func uploadSnapshot(lang, bucket string) error {
	client, err := s3.NewClient(cfg.S3Bucket, cfg.S3Region)
	if err != nil {
		return fmt.Errorf("failed to create S3 client: %w", err)
	}

	snapshotDir := cfg.SnapshotDir(lang)
	if _, err := os.Stat(snapshotDir); os.IsNotExist(err) {
		return fmt.Errorf("snapshot not found: %s", snapshotDir)
	}

	fmt.Printf("Uploading snapshot for %s to S3...\n", lang)
	ctx := context.Background()

	if err := client.UploadSnapshot(ctx, lang, snapshotDir); err != nil {
		return fmt.Errorf("upload failed: %w", err)
	}

	fmt.Printf("Uploaded successfully: s3://%s/snapshots/%s/\n", cfg.S3Bucket, lang)
	return nil
}

func downloadSnapshot(lang, bucket, output string) error {
	client, err := s3.NewClient(cfg.S3Bucket, cfg.S3Region)
	if err != nil {
		return fmt.Errorf("failed to create S3 client: %w", err)
	}

	if output == "" {
		output = cfg.SnapshotDir(lang)
	}

	fmt.Printf("Downloading snapshot for %s from S3...\n", lang)
	ctx := context.Background()

	start := time.Now()
	if err := client.DownloadSnapshot(ctx, lang, output); err != nil {
		return fmt.Errorf("download failed: %w", err)
	}

	elapsed := time.Since(start)
	fmt.Printf("Downloaded successfully in %v: %s\n", elapsed, output)
	return nil
}

// ========================
// EXECUTE HANDLER
// ========================

func executeCode(lang, code, file string, timeout int) error {
	if code == "" && file == "" {
		return fmt.Errorf("either --code or --file is required")
	}

	if file != "" {
		data, err := os.ReadFile(file)
		if err != nil {
			return fmt.Errorf("failed to read file: %w", err)
		}
		code = string(data)
	}

	manager := snapshot.NewManager(cfg)
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(timeout)*time.Second)
	defer cancel()

	fmt.Printf("Loading snapshot for %s...\n", lang)
	start := time.Now()

	fc, err := manager.Load(ctx, lang)
	if err != nil {
		return fmt.Errorf("failed to load snapshot: %w", err)
	}
	defer fc.Stop()

	loadTime := time.Since(start)
	fmt.Printf("Snapshot loaded in %v\n", loadTime)

	// TODO: Connect via vsock and execute code
	fmt.Printf("\nLanguage: %s\n", lang)
	fmt.Printf("\nCode to execute:\n%s\n", code)
	fmt.Println("\n[Execution via vsock not yet implemented]")

	return nil
}

// ========================
// API HANDLER
// ========================

func runAPIServer(host string, port int) error {
	fmt.Printf("Starting API server on %s:%d...\n", host, port)

	server, err := api.NewServer(cfg)
	if err != nil {
		return fmt.Errorf("failed to create server: %w", err)
	}

	fmt.Printf("\nEndpoints:\n")
	fmt.Printf("  GET  /health\n")
	fmt.Printf("  GET  /api/v1/languages\n")
	fmt.Printf("  GET  /api/v1/rootfs\n")
	fmt.Printf("  POST /api/v1/rootfs\n")
	fmt.Printf("  GET  /api/v1/snapshots\n")
	fmt.Printf("  POST /api/v1/snapshots\n")
	fmt.Printf("  POST /api/v1/execute\n")
	fmt.Printf("\n")

	return server.Run(host, port)
}

// ========================
// BENCHMARK HANDLER
// ========================

func runBenchmark(langs string, all bool) error {
	var languages []string

	if all {
		// List all snapshots locally
		manager := snapshot.NewManager(cfg)
		snapshots, err := manager.List()
		if err != nil {
			return fmt.Errorf("failed to list snapshots: %w", err)
		}
		for _, snap := range snapshots {
			languages = append(languages, snap.Language)
		}
		if len(languages) == 0 {
			return fmt.Errorf("no snapshots found locally")
		}
	} else if langs != "" {
		languages = strings.Split(langs, ",")
	} else {
		return fmt.Errorf("specify --all or --langs")
	}

	fmt.Println("==============================================")
	fmt.Println("  BENCHMARK: Snapshot Load Time")
	fmt.Println("==============================================")
	fmt.Printf("Testing %d languages...\n\n", len(languages))

	manager := snapshot.NewManager(cfg)
	ctx := context.Background()

	var totalLoad time.Duration
	passed := 0
	failed := 0

	for i, lang := range languages {
		fmt.Printf("[%d/%d] %s: ", i+1, len(languages), lang)

		start := time.Now()
		fc, err := manager.Load(ctx, lang)
		if err != nil {
			fmt.Printf("FAIL (%v)\n", err)
			failed++
			continue
		}

		loadTime := time.Since(start)
		fc.Stop()

		fmt.Printf("LOAD=%v [OK]\n", loadTime)
		totalLoad += loadTime
		passed++
	}

	fmt.Println("\n==============================================")
	fmt.Println("  BENCHMARK REPORT")
	fmt.Println("==============================================")
	fmt.Printf("\nResults: %d/%d PASSED, %d FAILED\n", passed, len(languages), failed)

	if passed > 0 {
		avgLoad := totalLoad / time.Duration(passed)
		fmt.Printf("\nAverage Load Time: %v\n", avgLoad)
		fmt.Printf("Total Load Time:   %v\n", totalLoad)
	}

	return nil
}

// ========================
// SETUP HANDLER
// ========================

func runSetup(kernelVersion, firecrackerVersion string, skipKernel, skipFirecracker, skipDocker bool) error {
	fmt.Println("Setting up Firecracker environment...")
	fmt.Println()

	// Create directories
	dirs := []string{
		"/srv/firecracker",
		"/srv/firecracker/images",
		"/dev/shm/snapshots",
	}

	fmt.Println("[1/5] Creating directories...")
	for _, dir := range dirs {
		if err := os.MkdirAll(dir, 0755); err != nil {
			return fmt.Errorf("failed to create %s: %w", dir, err)
		}
		fmt.Printf("  Created: %s\n", dir)
	}

	// Install Docker if not present
	if !skipDocker {
		fmt.Println("\n[2/5] Checking Docker...")
		if _, err := exec.LookPath("docker"); err != nil {
			fmt.Println("  Docker not found, installing...")
			if err := installDocker(); err != nil {
				return fmt.Errorf("failed to install Docker: %w", err)
			}
			fmt.Println("  Docker installed successfully!")
		} else {
			fmt.Println("  Docker already installed")
		}
	} else {
		fmt.Println("\n[2/5] Skipping Docker check")
	}

	// Download Firecracker binary
	if !skipFirecracker {
		fmt.Printf("\n[3/5] Downloading Firecracker v%s...\n", firecrackerVersion)
		fcURL := fmt.Sprintf("https://github.com/firecracker-microvm/firecracker/releases/download/v%s/firecracker-v%s-x86_64.tgz",
			firecrackerVersion, firecrackerVersion)

		if err := downloadAndExtractFirecracker(fcURL, "/usr/local/bin/firecracker"); err != nil {
			return fmt.Errorf("failed to download Firecracker: %w", err)
		}
		fmt.Println("  Installed: /usr/local/bin/firecracker")
	} else {
		fmt.Println("\n[3/5] Skipping Firecracker download")
	}

	// Download kernel
	if !skipKernel {
		fmt.Printf("\n[4/5] Downloading kernel %s...\n", kernelVersion)
		fmt.Println("  NOTE: Kernel 5.10+ is REQUIRED for Node.js support!")

		var kernelURL string
		switch kernelVersion {
		case "5.10":
			kernelURL = "https://storage.googleapis.com/fireactions/kernels/amd64/5.10/vmlinux"
		case "6.1":
			kernelURL = "https://storage.googleapis.com/fireactions/kernels/amd64/6.1/vmlinux"
		default:
			return fmt.Errorf("unsupported kernel version: %s (use 5.10 or 6.1)", kernelVersion)
		}

		if err := downloadFile(kernelURL, "/srv/firecracker/vmlinux"); err != nil {
			return fmt.Errorf("failed to download kernel: %w", err)
		}
		os.Chmod("/srv/firecracker/vmlinux", 0755)
		fmt.Println("  Installed: /srv/firecracker/vmlinux")
	} else {
		fmt.Println("\n[4/5] Skipping kernel download")
	}

	// Verify setup
	fmt.Println("\n[5/5] Verifying setup...")

	// Check Firecracker
	if _, err := os.Stat("/usr/local/bin/firecracker"); err == nil {
		fmt.Println("  Firecracker: OK")
	} else {
		fmt.Println("  Firecracker: NOT FOUND")
	}

	// Check kernel
	if _, err := os.Stat("/srv/firecracker/vmlinux"); err == nil {
		fmt.Println("  Kernel: OK")
	} else {
		fmt.Println("  Kernel: NOT FOUND")
	}

	// Check KVM
	if _, err := os.Stat("/dev/kvm"); err == nil {
		fmt.Println("  /dev/kvm: OK")
	} else {
		fmt.Println("  /dev/kvm: NOT FOUND (required for Firecracker)")
	}

	// Check Docker
	if _, err := exec.LookPath("docker"); err == nil {
		fmt.Println("  Docker: OK")
	} else {
		fmt.Println("  Docker: NOT FOUND")
	}

	fmt.Println("\nSetup complete!")
	fmt.Println("\nNext steps:")
	fmt.Println("  1. Create a rootfs:  infra.operator rootfs from-docker --name nodejs --image node:22-alpine --size 200")
	fmt.Println("  2. Create snapshot:  infra.operator snapshot create --lang nodejs --mem 512 --vcpus 1")
	fmt.Println("  3. Test execution:   infra.operator host --lang nodejs --code \"console.log('hello')\" --mem 512 --vcpus 1")

	return nil
}

// installDocker installs Docker on Ubuntu/Debian
func installDocker() error {
	// Install using the official Docker installation script
	fmt.Println("  Downloading Docker installation script...")

	// Download get-docker.sh
	resp, err := http.Get("https://get.docker.com")
	if err != nil {
		return fmt.Errorf("failed to download Docker script: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("failed to download Docker script: HTTP %d", resp.StatusCode)
	}

	// Save to temp file
	tmpFile, err := os.CreateTemp("", "get-docker-*.sh")
	if err != nil {
		return err
	}
	defer os.Remove(tmpFile.Name())

	if _, err := io.Copy(tmpFile, resp.Body); err != nil {
		return err
	}
	tmpFile.Close()

	// Make executable and run
	os.Chmod(tmpFile.Name(), 0755)

	fmt.Println("  Running Docker installation script...")
	cmd := exec.Command("sh", tmpFile.Name())
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("Docker installation failed: %w", err)
	}

	// Start Docker service
	fmt.Println("  Starting Docker service...")
	startCmd := exec.Command("systemctl", "start", "docker")
	startCmd.Run() // Ignore error, might already be running

	enableCmd := exec.Command("systemctl", "enable", "docker")
	enableCmd.Run() // Ignore error

	return nil
}

// downloadFile downloads a file from URL to destPath
func downloadFile(url, destPath string) error {
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("HTTP %d: %s", resp.StatusCode, resp.Status)
	}

	out, err := os.Create(destPath)
	if err != nil {
		return err
	}
	defer out.Close()

	_, err = io.Copy(out, resp.Body)
	return err
}

// downloadAndExtractFirecracker downloads and extracts Firecracker tarball
func downloadAndExtractFirecracker(url, destPath string) error {
	// Download to temp file
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("HTTP %d: %s", resp.StatusCode, resp.Status)
	}

	// Create temp file for tarball
	tmpFile, err := os.CreateTemp("", "firecracker-*.tgz")
	if err != nil {
		return err
	}
	defer os.Remove(tmpFile.Name())

	if _, err := io.Copy(tmpFile, resp.Body); err != nil {
		return err
	}
	tmpFile.Close()

	// Extract using tar command
	tmpDir, err := os.MkdirTemp("", "firecracker-extract-")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tmpDir)

	// Extract tarball
	cmd := exec.Command("tar", "-xzf", tmpFile.Name(), "-C", tmpDir)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("extract failed: %w", err)
	}

	// Find the firecracker binary in extracted files
	// The binary is named like: release-v1.7.0-x86_64/firecracker-v1.7.0-x86_64
	var fcBinary string
	err = filepath.Walk(tmpDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		name := info.Name()
		// Look for firecracker binary (not jailer, not debug, not .sha256)
		if !info.IsDir() &&
			strings.HasPrefix(name, "firecracker-v") &&
			!strings.Contains(name, "jailer") &&
			!strings.Contains(name, "debug") &&
			!strings.HasSuffix(name, ".sha256.txt") {
			fcBinary = path
			return filepath.SkipAll
		}
		return nil
	})

	if fcBinary == "" {
		return fmt.Errorf("firecracker binary not found in tarball")
	}

	// Copy to destination
	input, err := os.ReadFile(fcBinary)
	if err != nil {
		return err
	}

	if err := os.WriteFile(destPath, input, 0755); err != nil {
		return err
	}

	return nil
}

// ========================
// HELPERS
// ========================

func formatSize(bytes int64) string {
	const unit = 1024
	if bytes < unit {
		return fmt.Sprintf("%d B", bytes)
	}
	div, exp := int64(unit), 0
	for n := bytes / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %cB", float64(bytes)/float64(div), "KMGTPE"[exp])
}
