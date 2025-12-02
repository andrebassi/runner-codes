package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"

	"infra-operator/pkg/guest"
	"infra-operator/pkg/host"
)

var (
	version = "0.2.0"
	cfgFile string
)

// getBucket returns bucket from environment or empty string (required via flag)
func getBucket() string {
	if bucket := os.Getenv("BUCKET"); bucket != "" {
		return bucket
	}
	return ""
}

func main() {
	rootCmd := &cobra.Command{
		Use:   "infra.operator",
		Short: "Infra Operator CLI - Manage Firecracker VMs, rootfs and snapshots",
		Long: `Infra Operator CLI for managing Firecracker microVMs.

Create rootfs images, generate snapshots, upload to S3,
and execute code in isolated microVMs.

Components:
  host   - Run on the host to control Firecracker VMs
  guest  - Run inside the microVM to execute code

Examples:
  infra.operator rootfs create --lang python
  infra.operator snapshot create --lang python
  infra.operator run --lang python --code "print('hello')"
  infra.operator host --lang python --code "print('hello')"
  infra.operator guest --port 5000
  infra.operator api --port 8080`,
		Version: version,
	}

	// Global flags
	rootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/.infra-operator.yaml)")

	// Add commands
	rootCmd.AddCommand(setupCmd())
	rootCmd.AddCommand(rootfsCmd())
	rootCmd.AddCommand(snapshotCmd())
	rootCmd.AddCommand(runCmd())
	rootCmd.AddCommand(apiCmd())
	rootCmd.AddCommand(benchmarkCmd())
	rootCmd.AddCommand(hostCmd())
	rootCmd.AddCommand(guestCmd())

	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

// rootfsCmd - manage rootfs images
func rootfsCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "rootfs",
		Short: "Manage rootfs images",
		Long:  "Create, list, upload and download rootfs images for different languages",
	}

	cmd.AddCommand(rootfsCreateCmd())
	cmd.AddCommand(rootfsFromDockerCmd())
	cmd.AddCommand(rootfsListCmd())
	cmd.AddCommand(rootfsUploadCmd())
	cmd.AddCommand(rootfsDownloadCmd())

	return cmd
}

func rootfsCreateCmd() *cobra.Command {
	var name, output, packages, postInstall, base string
	var size int

	cmd := &cobra.Command{
		Use:   "create",
		Short: "Create a new rootfs image",
		Long: `Create a new rootfs image with specified packages.

All parameters are passed via command line - no hardcoded language configs.
The complete command should be documented in each language's .mdx file.

Base OS options:
  - alpine: Minimal Alpine Linux (~8MB base, uses apk, musl libc)
  - debian: Debian 12 Bookworm (~190MB base, uses apt, glibc)
  - ubuntu: Ubuntu 22.04 Jammy (~200MB base, uses apt, glibc) [default]`,
		Example: `  # Python with Alpine (smallest)
  infra.operator rootfs create --name python --size 100 --base alpine --packages "python3"

  # Python with Ubuntu (default, most compatible)
  infra.operator rootfs create --name python --size 500 --base ubuntu --packages "python3"

  # Node.js with Debian
  infra.operator rootfs create --name nodejs --size 300 --base debian --packages "nodejs,npm"

  # Go with Alpine (with post-install script)
  infra.operator rootfs create --name go --size 200 --base alpine --packages "wget" \
    --post-install "wget -qO- https://go.dev/dl/go1.21.0.linux-amd64.tar.gz | tar -C /usr/local -xzf - && ln -s /usr/local/go/bin/go /usr/bin/go"`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return createRootfsWithBase(name, size, base, packages, postInstall, output)
		},
	}

	cmd.Flags().StringVarP(&name, "name", "n", "", "Rootfs name (required, e.g., python, nodejs, go)")
	cmd.Flags().IntVarP(&size, "size", "s", 1024, "Image size in MB")
	cmd.Flags().StringVarP(&base, "base", "b", "alpine", "Base OS: alpine, debian, or ubuntu")
	cmd.Flags().StringVarP(&packages, "packages", "p", "", "Packages to install (apk for alpine, apt for debian/ubuntu)")
	cmd.Flags().StringVar(&postInstall, "post-install", "", "Post-install script (sh for alpine, bash for debian/ubuntu)")
	cmd.Flags().StringVarP(&output, "output", "o", "", "Output path (default: /srv/firecracker/images/rootfs-{name}.ext4)")
	cmd.MarkFlagRequired("name")

	return cmd
}

// rootfsFromDockerCmd creates rootfs from official Docker images (recommended)
func rootfsFromDockerCmd() *cobra.Command {
	var name, image, output string
	var size int

	cmd := &cobra.Command{
		Use:   "from-docker",
		Short: "Create rootfs from official Docker image (RECOMMENDED)",
		Long: `Create a rootfs image from an official Docker image.

This is the RECOMMENDED way to create rootfs for languages like Node.js, Python, etc.
The official Docker images are well-tested, optimized, and include all dependencies.

Popular images:
  - node:22-alpine3.20     (Node.js 22 LTS - ~180MB)
  - node:23-alpine3.20     (Node.js 23 Current - ~180MB)
  - python:3.12-alpine3.21 (Python 3.12 - ~60MB)
  - python:3.13-alpine     (Python 3.13 - ~60MB)
  - golang:1.22-alpine3.20 (Go 1.22 - ~280MB)
  - ruby:3.3-alpine3.20    (Ruby 3.3 - ~90MB)`,
		Example: `  # Node.js 22 LTS (recommended)
  infra.operator rootfs from-docker --name nodejs --image node:22-alpine3.20 --size 200

  # Python 3.12
  infra.operator rootfs from-docker --name python --image python:3.12-alpine3.21 --size 100

  # Go 1.22
  infra.operator rootfs from-docker --name go --image golang:1.22-alpine3.20 --size 300`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return createRootfsFromDocker(name, size, image, output)
		},
	}

	cmd.Flags().StringVarP(&name, "name", "n", "", "Rootfs name (required, e.g., python, nodejs, go)")
	cmd.Flags().StringVarP(&image, "image", "i", "", "Docker image (required, e.g., node:22-alpine3.20)")
	cmd.Flags().IntVarP(&size, "size", "s", 200, "Image size in MB")
	cmd.Flags().StringVarP(&output, "output", "o", "", "Output path (default: /srv/firecracker/images/rootfs-{name}.ext4)")
	cmd.MarkFlagRequired("name")
	cmd.MarkFlagRequired("image")

	return cmd
}

func rootfsListCmd() *cobra.Command {
	var remote bool

	cmd := &cobra.Command{
		Use:     "list",
		Aliases: []string{"ls"},
		Short:   "List available rootfs images",
		RunE: func(cmd *cobra.Command, args []string) error {
			return listRootfs(remote)
		},
	}

	cmd.Flags().BoolVarP(&remote, "remote", "r", false, "List images from S3")

	return cmd
}

func rootfsUploadCmd() *cobra.Command {
	var lang, bucket string

	cmd := &cobra.Command{
		Use:   "upload",
		Short: "Upload rootfs image to storage bucket",
		Example: `  # Using environment variable
  export BUCKET=llm-firecracker
  infra.operator rootfs upload --lang python

  # Using flag
  infra.operator rootfs upload --lang python --bucket llm-firecracker`,
		RunE: func(cmd *cobra.Command, args []string) error {
			if bucket == "" {
				return fmt.Errorf("bucket required: set BUCKET env var or use --bucket flag")
			}
			return uploadRootfs(lang, bucket)
		},
	}

	cmd.Flags().StringVarP(&lang, "lang", "l", "", "Language to upload (required)")
	cmd.Flags().StringVarP(&bucket, "bucket", "b", getBucket(), "Storage bucket (or set BUCKET env)")
	cmd.MarkFlagRequired("lang")

	return cmd
}

func rootfsDownloadCmd() *cobra.Command {
	var lang, bucket, output string

	cmd := &cobra.Command{
		Use:   "download",
		Short: "Download rootfs image from storage bucket",
		Example: `  # Using environment variable
  export BUCKET=llm-firecracker
  infra.operator rootfs download --lang python

  # Using flag
  infra.operator rootfs download --lang python --bucket llm-firecracker`,
		RunE: func(cmd *cobra.Command, args []string) error {
			if bucket == "" {
				return fmt.Errorf("bucket required: set BUCKET env var or use --bucket flag")
			}
			return downloadRootfs(lang, bucket, output)
		},
	}

	cmd.Flags().StringVarP(&lang, "lang", "l", "", "Language to download (required)")
	cmd.Flags().StringVarP(&bucket, "bucket", "b", getBucket(), "Storage bucket (or set BUCKET env)")
	cmd.Flags().StringVarP(&output, "output", "o", "", "Output path")
	cmd.MarkFlagRequired("lang")

	return cmd
}

// snapshotCmd - manage snapshots
func snapshotCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "snapshot",
		Short: "Manage VM snapshots",
		Long:  "Create, list, upload and download VM snapshots",
	}

	cmd.AddCommand(snapshotCreateCmd())
	cmd.AddCommand(snapshotListCmd())
	cmd.AddCommand(snapshotUploadCmd())
	cmd.AddCommand(snapshotDownloadCmd())

	return cmd
}

func snapshotCreateCmd() *cobra.Command {
	var lang, rootfs, kernelPath, firecrackerPath string
	var memMB, vcpus int

	cmd := &cobra.Command{
		Use:   "create",
		Short: "Create a new snapshot from rootfs",
		Long: `Create a new snapshot from rootfs.

IMPORTANT: Kernel 5.10+ is REQUIRED for Node.js and other modern runtimes.
If --kernel or --firecracker are not specified, defaults are used.
If they don't exist, they will be downloaded automatically.`,
		Example: `  # Create with auto-download of kernel and firecracker if missing
  infra.operator snapshot create --lang python --mem 512 --vcpus 1

  # Create with specific paths
  infra.operator snapshot create --lang nodejs --mem 1024 --vcpus 2 \
    --kernel /path/to/vmlinux --firecracker /path/to/firecracker`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return createSnapshot(lang, rootfs, kernelPath, firecrackerPath, memMB, vcpus)
		},
	}

	cmd.Flags().StringVarP(&lang, "lang", "l", "", "Language runtime (required)")
	cmd.Flags().StringVarP(&rootfs, "rootfs", "r", "", "Rootfs path (default: auto-detect)")
	cmd.Flags().StringVarP(&kernelPath, "kernel", "k", "/srv/firecracker/vmlinux", "Path to kernel (5.10+, auto-downloads if missing)")
	cmd.Flags().StringVarP(&firecrackerPath, "firecracker", "f", "/usr/local/bin/firecracker", "Path to firecracker binary (auto-downloads if missing)")
	cmd.Flags().IntVarP(&memMB, "mem", "m", 0, "Memory in MB (required)")
	cmd.Flags().IntVarP(&vcpus, "vcpus", "c", 0, "Number of vCPUs (required)")
	cmd.MarkFlagRequired("lang")
	cmd.MarkFlagRequired("mem")
	cmd.MarkFlagRequired("vcpus")

	return cmd
}

func snapshotListCmd() *cobra.Command {
	var remote bool

	cmd := &cobra.Command{
		Use:     "list",
		Aliases: []string{"ls"},
		Short:   "List available snapshots",
		RunE: func(cmd *cobra.Command, args []string) error {
			return listSnapshots(remote)
		},
	}

	cmd.Flags().BoolVarP(&remote, "remote", "r", false, "List snapshots from S3")

	return cmd
}

func snapshotUploadCmd() *cobra.Command {
	var lang, bucket string

	cmd := &cobra.Command{
		Use:   "upload",
		Short: "Upload snapshot to storage bucket",
		Example: `  # Using environment variable
  export BUCKET=llm-firecracker
  infra.operator snapshot upload --lang python

  # Using flag
  infra.operator snapshot upload --lang python --bucket llm-firecracker`,
		RunE: func(cmd *cobra.Command, args []string) error {
			if bucket == "" {
				return fmt.Errorf("bucket required: set BUCKET env var or use --bucket flag")
			}
			return uploadSnapshot(lang, bucket)
		},
	}

	cmd.Flags().StringVarP(&lang, "lang", "l", "", "Language to upload (required)")
	cmd.Flags().StringVarP(&bucket, "bucket", "b", getBucket(), "Storage bucket (or set BUCKET env)")
	cmd.MarkFlagRequired("lang")

	return cmd
}

func snapshotDownloadCmd() *cobra.Command {
	var lang, bucket, output string

	cmd := &cobra.Command{
		Use:   "download",
		Short: "Download snapshot from storage bucket",
		Example: `  # Using environment variable
  export BUCKET=llm-firecracker
  infra.operator snapshot download --lang python

  # Using flag
  infra.operator snapshot download --lang python --bucket llm-firecracker`,
		RunE: func(cmd *cobra.Command, args []string) error {
			if bucket == "" {
				return fmt.Errorf("bucket required: set BUCKET env var or use --bucket flag")
			}
			return downloadSnapshot(lang, bucket, output)
		},
	}

	cmd.Flags().StringVarP(&lang, "lang", "l", "", "Language to download (required)")
	cmd.Flags().StringVarP(&bucket, "bucket", "b", getBucket(), "Storage bucket (or set BUCKET env)")
	cmd.Flags().StringVarP(&output, "output", "o", "", "Output directory")
	cmd.MarkFlagRequired("lang")

	return cmd
}

// runCmd - execute code
func runCmd() *cobra.Command {
	var lang, code, file string
	var timeout int

	cmd := &cobra.Command{
		Use:   "run",
		Short: "Execute code in a microVM",
		Example: `  infra.operator run --lang python --code "print('hello')"
  infra.operator run --lang nodejs --file script.js
  infra.operator run --lang go --code 'package main; func main() { println("hi") }'`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return executeCode(lang, code, file, timeout)
		},
	}

	cmd.Flags().StringVarP(&lang, "lang", "l", "", "Language runtime (required)")
	cmd.Flags().StringVarP(&code, "code", "c", "", "Code to execute")
	cmd.Flags().StringVarP(&file, "file", "f", "", "File to execute")
	cmd.Flags().IntVarP(&timeout, "timeout", "t", 30, "Execution timeout in seconds")
	cmd.MarkFlagRequired("lang")

	return cmd
}

// apiCmd - start API server
func apiCmd() *cobra.Command {
	var port int
	var host string

	cmd := &cobra.Command{
		Use:   "api",
		Short: "Start the HTTP API server",
		Example: `  infra.operator api --port 8080
  infra.operator api --host 0.0.0.0 --port 3000`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return runAPIServer(host, port)
		},
	}

	cmd.Flags().IntVarP(&port, "port", "p", 8080, "API server port")
	cmd.Flags().StringVarP(&host, "host", "H", "0.0.0.0", "API server host")

	return cmd
}

// benchmarkCmd - run benchmarks
func benchmarkCmd() *cobra.Command {
	var langs string
	var all bool

	cmd := &cobra.Command{
		Use:   "benchmark",
		Short: "Run performance benchmarks",
		Example: `  infra.operator benchmark --all
  infra.operator benchmark --langs python,nodejs,go`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return runBenchmark(langs, all)
		},
	}

	cmd.Flags().StringVarP(&langs, "langs", "l", "", "Comma-separated list of languages")
	cmd.Flags().BoolVarP(&all, "all", "a", false, "Benchmark all 45 languages")

	return cmd
}

// hostCmd - run host agent (controls Firecracker VMs)
func hostCmd() *cobra.Command {
	var lang, code string
	var timeout, vcpus, memMiB int
	var vsockPort uint
	var useSnapshot bool
	var cacheDir, bucket, region string
	var kernelPath, rootfsPath string

	cmd := &cobra.Command{
		Use:   "host",
		Short: "Run host agent to control Firecracker VMs and execute code",
		Long: `Host agent controls Firecracker microVMs from the host machine.

It starts a VM (either cold boot or from snapshot), connects via vsock,
and sends code execution requests to the guest-runner inside the VM.

This replaces the standalone host-agent binary.`,
		Example: `  # Execute Python code using snapshot (warm boot - fast)
  infra.operator host --lang python --code "print('hello')" --snapshot

  # Execute code with cold boot
  infra.operator host --lang python --code "print('hello')" --kernel /srv/firecracker/vmlinux --rootfs /srv/firecracker/rootfs-python.ext4

  # Specify custom snapshot cache directory
  infra.operator host --lang nodejs --code "console.log('hi')" --snapshot --cache-dir /dev/shm/snapshots`,
		RunE: func(cmd *cobra.Command, args []string) error {
			config := host.DefaultRunnerConfig()
			config.Lang = lang
			config.Code = code
			config.Timeout = timeout
			config.VCPUs = vcpus
			config.MemMiB = memMiB
			config.VsockPort = vsockPort
			config.UseSnapshot = useSnapshot
			config.CacheDir = cacheDir
			config.S3Bucket = bucket
			config.S3Region = region
			config.KernelPath = kernelPath
			config.RootfsPath = rootfsPath

			runner := host.NewRunner(config)
			result, err := runner.Run()
			if err != nil {
				return err
			}

			host.PrintResult(result)
			return nil
		},
	}

	cmd.Flags().StringVarP(&lang, "lang", "l", "", "Language runtime (required)")
	cmd.Flags().StringVarP(&code, "code", "c", "", "Code to execute (required)")
	cmd.Flags().IntVarP(&timeout, "timeout", "t", 10, "Execution timeout in seconds")
	cmd.Flags().IntVar(&vcpus, "vcpus", 0, "Number of vCPUs (required)")
	cmd.Flags().IntVar(&memMiB, "mem", 0, "Memory in MiB (required)")
	cmd.Flags().UintVar(&vsockPort, "port", host.DefaultVsockPort, "Vsock port for guest-runner")
	cmd.Flags().BoolVar(&useSnapshot, "snapshot", true, "Use snapshot for warm boot (faster)")
	cmd.Flags().StringVar(&cacheDir, "cache-dir", host.DefaultCacheDir, "Directory for snapshot cache")
	cmd.Flags().StringVar(&bucket, "bucket", getBucket(), "Storage bucket for snapshots (or set BUCKET env)")
	cmd.Flags().StringVar(&region, "region", os.Getenv("AWS_REGION"), "Cloud region (or set AWS_REGION env)")
	cmd.Flags().StringVar(&kernelPath, "kernel", "/srv/firecracker/vmlinux", "Path to kernel image (for cold boot)")
	cmd.Flags().StringVar(&rootfsPath, "rootfs", "", "Path to rootfs image (for cold boot)")
	cmd.MarkFlagRequired("lang")
	cmd.MarkFlagRequired("code")
	cmd.MarkFlagRequired("mem")
	cmd.MarkFlagRequired("vcpus")

	return cmd
}

// guestCmd - run guest runner (executes code inside VM)
func guestCmd() *cobra.Command {
	var port uint

	cmd := &cobra.Command{
		Use:   "guest",
		Short: "Run guest runner inside a microVM to execute code",
		Long: `Guest runner runs inside the Firecracker microVM.

It listens on a vsock port for code execution requests from the host-agent,
executes the code using the appropriate language runtime, and returns results.

This replaces the standalone guest-runner binary and should be included
in the rootfs image that boots inside the microVM.`,
		Example: `  # Start guest runner on default port (5000)
  infra.operator guest

  # Start on a custom port
  infra.operator guest --port 6000`,
		RunE: func(cmd *cobra.Command, args []string) error {
			config := guest.DefaultServerConfig()
			config.Port = uint32(port)

			server := guest.NewServer(config)
			return server.Run()
		},
	}

	cmd.Flags().UintVarP(&port, "port", "p", guest.DefaultPort, "Vsock port to listen on")

	return cmd
}

// setupCmd - setup Firecracker environment on a new machine
func setupCmd() *cobra.Command {
	var kernelVersion string
	var firecrackerVersion string
	var skipKernel, skipFirecracker, skipDocker bool

	cmd := &cobra.Command{
		Use:   "setup",
		Short: "Setup Firecracker environment on a new machine",
		Long: `Setup downloads and installs all required components for Firecracker:

Components installed:
  - Docker (required for rootfs creation via from-docker)
  - Firecracker binary (from GitHub releases)
  - Linux kernel 5.10+ (REQUIRED for Node.js support)
  - Creates directory structure (/srv/firecracker/images, /dev/shm/snapshots)

IMPORTANT: Kernel 5.10+ is required for Node.js to work correctly.
           Older kernels (4.14) cause Node.js to hang due to libuv/epoll issues.`,
		Example: `  # Full setup with defaults (installs Docker, Firecracker, kernel)
  sudo infra.operator setup

  # Setup with specific versions
  sudo infra.operator setup --kernel-version 5.10 --firecracker-version 1.7.0

  # Skip Docker installation (if already installed)
  sudo infra.operator setup --skip-docker

  # Skip kernel download (if already installed)
  sudo infra.operator setup --skip-kernel`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return runSetup(kernelVersion, firecrackerVersion, skipKernel, skipFirecracker, skipDocker)
		},
	}

	cmd.Flags().StringVar(&kernelVersion, "kernel-version", "5.10", "Kernel version to download (5.10 or 6.1)")
	cmd.Flags().StringVar(&firecrackerVersion, "firecracker-version", "1.7.0", "Firecracker version to download")
	cmd.Flags().BoolVar(&skipKernel, "skip-kernel", false, "Skip kernel download")
	cmd.Flags().BoolVar(&skipFirecracker, "skip-firecracker", false, "Skip Firecracker binary download")
	cmd.Flags().BoolVar(&skipDocker, "skip-docker", false, "Skip Docker installation")

	return cmd
}
