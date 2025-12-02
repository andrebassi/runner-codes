package rootfs

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"infra-operator/internal/config"
)

// BaseType represents the base OS for rootfs
type BaseType string

const (
	BaseUbuntu BaseType = "ubuntu"
	BaseDebian BaseType = "debian"
	BaseAlpine BaseType = "alpine"
	BaseDocker BaseType = "docker"
)

// Builder creates rootfs images
type Builder struct {
	cfg         *config.Config
	guestRunner string
}

// NewBuilder creates a new rootfs builder
func NewBuilder(cfg *config.Config) *Builder {
	return &Builder{
		cfg:         cfg,
		guestRunner: cfg.GuestRunner,
	}
}

// BuildWithArgs creates a rootfs image using command line arguments - legacy compatibility
func (b *Builder) BuildWithArgs(ctx context.Context, name string, sizeMB int, packages, postInstall, outputPath string) error {
	return b.BuildWithBase(ctx, name, sizeMB, BaseUbuntu, packages, postInstall, outputPath)
}

// BuildWithBase creates a rootfs image with specified base OS (ubuntu, debian, or alpine)
func (b *Builder) BuildWithBase(ctx context.Context, name string, sizeMB int, base BaseType, packages, postInstall, outputPath string) error {
	if outputPath == "" {
		outputPath = b.cfg.RootfsPath(name)
	}

	mountPoint := fmt.Sprintf("/mnt/%s", name)
	tmpImage := fmt.Sprintf("/tmp/rootfs-%s.ext4", name)

	// Cleanup any previous attempt
	b.cleanup(mountPoint, tmpImage)

	fmt.Printf("[1/6] Creating ext4 image (%dMB)...\n", sizeMB)
	if err := b.createImage(tmpImage, sizeMB); err != nil {
		return fmt.Errorf("failed to create image: %w", err)
	}

	// Install base system based on type
	switch base {
	case BaseAlpine:
		fmt.Printf("[2/6] Installing Alpine Linux base...\n")
		if err := b.installAlpine(tmpImage, mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to install Alpine: %w", err)
		}

		if packages != "" {
			fmt.Printf("[3/6] Installing packages (apk): %s\n", packages)
			if err := b.installAlpinePackages(mountPoint, packages); err != nil {
				b.cleanup(mountPoint, tmpImage)
				return fmt.Errorf("failed to install packages: %w", err)
			}
		}

		if postInstall != "" {
			fmt.Printf("[4/6] Running post-installation...\n")
			if err := b.runAlpinePostInstall(mountPoint, postInstall); err != nil {
				fmt.Printf("WARNING: Post-install had issues: %v\n", err)
			}
		}

		fmt.Printf("[5/6] Installing guest-runner...\n")
		if err := b.installGuestRunner(mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to install guest-runner: %w", err)
		}

		fmt.Printf("[6/6] Creating OpenRC service...\n")
		if err := b.createOpenRCService(mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to create OpenRC service: %w", err)
		}

	case BaseDebian:
		fmt.Printf("[2/6] Installing Debian 12 (Bookworm) base (debootstrap)...\n")
		if err := b.debootstrapDebian(tmpImage, mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to debootstrap Debian: %w", err)
		}

		fmt.Printf("[3/6] Configuring apt sources...\n")
		if err := b.configureAptDebian(mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to configure apt: %w", err)
		}

		if packages != "" {
			fmt.Printf("[4/6] Installing packages: %s\n", packages)
			if err := b.installPackages(mountPoint, packages); err != nil {
				b.cleanup(mountPoint, tmpImage)
				return fmt.Errorf("failed to install packages: %w", err)
			}
		}

		if postInstall != "" {
			fmt.Printf("[4.5/6] Running post-installation...\n")
			if err := b.runPostInstall(mountPoint, postInstall); err != nil {
				fmt.Printf("WARNING: Post-install had issues: %v\n", err)
			}
		}

		fmt.Printf("[5/6] Installing guest-runner...\n")
		if err := b.installGuestRunner(mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to install guest-runner: %w", err)
		}

		fmt.Printf("[6/6] Creating systemd service...\n")
		if err := b.createSystemdService(mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to create systemd service: %w", err)
		}

	default: // Ubuntu
		fmt.Printf("[2/6] Installing Ubuntu 22.04 base (debootstrap)...\n")
		if err := b.debootstrap(tmpImage, mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to debootstrap: %w", err)
		}

		fmt.Printf("[3/6] Configuring apt sources...\n")
		if err := b.configureApt(mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to configure apt: %w", err)
		}

		if packages != "" {
			fmt.Printf("[4/6] Installing packages: %s\n", packages)
			if err := b.installPackages(mountPoint, packages); err != nil {
				b.cleanup(mountPoint, tmpImage)
				return fmt.Errorf("failed to install packages: %w", err)
			}
		}

		if postInstall != "" {
			fmt.Printf("[4.5/6] Running post-installation...\n")
			if err := b.runPostInstall(mountPoint, postInstall); err != nil {
				fmt.Printf("WARNING: Post-install had issues: %v\n", err)
			}
		}

		fmt.Printf("[5/6] Installing guest-runner...\n")
		if err := b.installGuestRunner(mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to install guest-runner: %w", err)
		}

		fmt.Printf("[6/6] Creating systemd service...\n")
		if err := b.createSystemdService(mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to create systemd service: %w", err)
		}
	}

	// Finalize
	b.unmount(mountPoint)

	// Move to final location
	if err := os.MkdirAll(filepath.Dir(outputPath), 0755); err != nil {
		return fmt.Errorf("failed to create output dir: %w", err)
	}

	// Use cp + rm instead of mv/rename (cross-device safe)
	fmt.Printf("Moving image to %s...\n", outputPath)
	cpCmd := exec.Command("cp", tmpImage, outputPath)
	cpCmd.Stdout = os.Stdout
	cpCmd.Stderr = os.Stderr
	if err := cpCmd.Run(); err != nil {
		return fmt.Errorf("failed to copy image: %w", err)
	}
	os.Remove(tmpImage)

	fmt.Printf("Created: %s\n", outputPath)
	return nil
}

func (b *Builder) createImage(path string, sizeMB int) error {
	// dd if=/dev/zero of=path bs=1M count=sizeMB
	cmd := exec.Command("dd", "if=/dev/zero", "of="+path, "bs=1M", fmt.Sprintf("count=%d", sizeMB), "status=progress")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}

	// mkfs.ext4
	cmd = exec.Command("mkfs.ext4", "-F", path)
	return cmd.Run()
}

func (b *Builder) debootstrap(imagePath, mountPoint string) error {
	// Mount
	if err := os.MkdirAll(mountPoint, 0755); err != nil {
		return err
	}

	cmd := exec.Command("mount", "-o", "loop", imagePath, mountPoint)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("mount failed: %w", err)
	}

	// Debootstrap Ubuntu
	cmd = exec.Command("debootstrap", "--variant=minbase", "jammy", mountPoint, "http://archive.ubuntu.com/ubuntu/")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func (b *Builder) debootstrapDebian(imagePath, mountPoint string) error {
	// Mount
	if err := os.MkdirAll(mountPoint, 0755); err != nil {
		return err
	}

	cmd := exec.Command("mount", "-o", "loop", imagePath, mountPoint)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("mount failed: %w", err)
	}

	// Debootstrap Debian Bookworm
	cmd = exec.Command("debootstrap", "--variant=minbase", "bookworm", mountPoint, "http://deb.debian.org/debian/")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func (b *Builder) installAlpine(imagePath, mountPoint string) error {
	// Mount
	if err := os.MkdirAll(mountPoint, 0755); err != nil {
		return err
	}

	cmd := exec.Command("mount", "-o", "loop", imagePath, mountPoint)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("mount failed: %w", err)
	}

	// Download and extract Alpine minirootfs
	alpineVersion := "3.19"
	alpineURL := fmt.Sprintf("https://dl-cdn.alpinelinux.org/alpine/v%s/releases/x86_64/alpine-minirootfs-%s.0-x86_64.tar.gz", alpineVersion, alpineVersion)

	fmt.Printf("  Downloading Alpine %s minirootfs...\n", alpineVersion)

	// Download
	wgetCmd := exec.Command("wget", "-q", "-O", "/tmp/alpine-minirootfs.tar.gz", alpineURL)
	wgetCmd.Stdout = os.Stdout
	wgetCmd.Stderr = os.Stderr
	if err := wgetCmd.Run(); err != nil {
		return fmt.Errorf("failed to download Alpine: %w", err)
	}

	// Extract
	tarCmd := exec.Command("tar", "-xzf", "/tmp/alpine-minirootfs.tar.gz", "-C", mountPoint)
	tarCmd.Stdout = os.Stdout
	tarCmd.Stderr = os.Stderr
	if err := tarCmd.Run(); err != nil {
		return fmt.Errorf("failed to extract Alpine: %w", err)
	}

	// Cleanup
	os.Remove("/tmp/alpine-minirootfs.tar.gz")

	// Configure Alpine repositories
	repos := `https://dl-cdn.alpinelinux.org/alpine/v3.19/main
https://dl-cdn.alpinelinux.org/alpine/v3.19/community
`
	if err := os.WriteFile(filepath.Join(mountPoint, "etc/apk/repositories"), []byte(repos), 0644); err != nil {
		return fmt.Errorf("failed to configure repositories: %w", err)
	}

	// Copy resolv.conf for DNS resolution
	exec.Command("cp", "/etc/resolv.conf", filepath.Join(mountPoint, "etc/resolv.conf")).Run()

	return nil
}

func (b *Builder) installAlpinePackages(mountPoint, packages string) error {
	// Mount /proc and /dev for apk
	exec.Command("mount", "-t", "proc", "proc", filepath.Join(mountPoint, "proc")).Run()
	exec.Command("mount", "-t", "devtmpfs", "dev", filepath.Join(mountPoint, "dev")).Run()
	defer func() {
		exec.Command("umount", filepath.Join(mountPoint, "dev")).Run()
		exec.Command("umount", filepath.Join(mountPoint, "proc")).Run()
	}()

	// apk update
	cmd := exec.Command("chroot", mountPoint, "apk", "update")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("apk update failed: %w", err)
	}

	// apk add openrc (required for service management)
	cmd = exec.Command("chroot", mountPoint, "apk", "add", "--no-cache", "openrc")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to install openrc: %w", err)
	}

	// apk add user packages
	args := []string{mountPoint, "apk", "add", "--no-cache"}
	for _, pkg := range splitPackages(packages) {
		args = append(args, pkg)
	}
	cmd = exec.Command("chroot", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("apk add failed: %w", err)
	}

	return nil
}

func (b *Builder) runAlpinePostInstall(mountPoint, script string) error {
	// Mount /proc for post-install
	exec.Command("mount", "-t", "proc", "proc", filepath.Join(mountPoint, "proc")).Run()
	exec.Command("mount", "-t", "devtmpfs", "dev", filepath.Join(mountPoint, "dev")).Run()
	defer func() {
		exec.Command("umount", filepath.Join(mountPoint, "dev")).Run()
		exec.Command("umount", filepath.Join(mountPoint, "proc")).Run()
	}()

	cmd := exec.Command("chroot", mountPoint, "/bin/sh", "-c", script)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// ensureProcMount ensures /proc is mounted VERY EARLY on Alpine systems
// This is critical for compilers like rustc that need /proc/self/exe
// We modify /etc/inittab to mount /proc as the first sysinit action
func (b *Builder) ensureProcMount(mountPoint string) {
	// 1. Add /proc mount to fstab
	fstabPath := filepath.Join(mountPoint, "etc/fstab")
	fstabContent, _ := os.ReadFile(fstabPath)
	if !strings.Contains(string(fstabContent), "proc /proc") {
		f, err := os.OpenFile(fstabPath, os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0644)
		if err == nil {
			f.WriteString("\nproc /proc proc defaults 0 0\n")
			f.Close()
		}
	}

	// 2. Modify /etc/inittab to mount /proc BEFORE OpenRC starts
	// This is the earliest possible mount point in the boot sequence
	inittabPath := filepath.Join(mountPoint, "etc/inittab")
	inittabContent, err := os.ReadFile(inittabPath)
	if err == nil {
		inittabStr := string(inittabContent)
		// Check if we already added our mount
		if !strings.Contains(inittabStr, "mount -t proc") {
			// Find the first sysinit line and add our mount before it
			// Typical Alpine inittab has: ::sysinit:/sbin/openrc sysinit
			// We want to add: ::sysinit:/bin/mount -t proc proc /proc
			if strings.Contains(inittabStr, "::sysinit:") {
				// Insert proc mount as the FIRST sysinit
				newInittab := strings.Replace(inittabStr,
					"::sysinit:",
					"::sysinit:/bin/mount -t proc proc /proc -o nosuid,nodev,noexec,relatime\n::sysinit:",
					1)
				os.WriteFile(inittabPath, []byte(newInittab), 0644)
				fmt.Printf("    Modified /etc/inittab to mount /proc early\n")
			}
		}
	} else {
		// Create inittab if it doesn't exist
		inittab := `# /etc/inittab - init configuration
::sysinit:/bin/mount -t proc proc /proc -o nosuid,nodev,noexec,relatime
::sysinit:/sbin/openrc sysinit
::sysinit:/sbin/openrc boot
::wait:/sbin/openrc default
::ctrlaltdel:/sbin/reboot
::shutdown:/sbin/openrc shutdown
`
		os.WriteFile(inittabPath, []byte(inittab), 0644)
		fmt.Printf("    Created /etc/inittab with /proc mount\n")
	}

	// 3. Create /proc directory if it doesn't exist
	procDir := filepath.Join(mountPoint, "proc")
	os.MkdirAll(procDir, 0555)
}

func (b *Builder) createOpenRCService(mountPoint string) error {
	// Create init.d directory
	initDir := filepath.Join(mountPoint, "etc/init.d")
	if err := os.MkdirAll(initDir, 0755); err != nil {
		return err
	}

	// Create OpenRC service script
	service := `#!/sbin/openrc-run

name="guest-runner"
description="LLM Firecracker Guest Runner"
command="/usr/local/bin/infra.operator"
command_args="guest --port 5000"
command_background=true
pidfile="/run/${RC_SVCNAME}.pid"

depend() {
    need localmount
    after bootmisc
}

start_pre() {
    # Mount /proc if not already mounted (required for Rust, Go compilers)
    if [ ! -d /proc/self ]; then
        mount -t proc proc /proc 2>/dev/null || true
    fi
}
`
	servicePath := filepath.Join(initDir, "guest-runner")
	if err := os.WriteFile(servicePath, []byte(service), 0755); err != nil {
		return err
	}

	// Enable service at default runlevel
	runlevelDir := filepath.Join(mountPoint, "etc/runlevels/default")
	if err := os.MkdirAll(runlevelDir, 0755); err != nil {
		return err
	}

	return os.Symlink("/etc/init.d/guest-runner", filepath.Join(runlevelDir, "guest-runner"))
}

func (b *Builder) configureApt(mountPoint string) error {
	sources := `deb http://archive.ubuntu.com/ubuntu jammy main universe
deb http://archive.ubuntu.com/ubuntu jammy-updates main universe
`
	return os.WriteFile(filepath.Join(mountPoint, "etc/apt/sources.list"), []byte(sources), 0644)
}

func (b *Builder) configureAptDebian(mountPoint string) error {
	sources := `deb http://deb.debian.org/debian bookworm main
deb http://deb.debian.org/debian bookworm-updates main
deb http://security.debian.org/debian-security bookworm-security main
`
	return os.WriteFile(filepath.Join(mountPoint, "etc/apt/sources.list"), []byte(sources), 0644)
}

func (b *Builder) installPackages(mountPoint, packages string) error {
	// Mount /proc for apt/dpkg
	exec.Command("mount", "-t", "proc", "proc", filepath.Join(mountPoint, "proc")).Run()
	exec.Command("mount", "-t", "devtmpfs", "dev", filepath.Join(mountPoint, "dev")).Run()
	defer func() {
		exec.Command("umount", filepath.Join(mountPoint, "dev")).Run()
		exec.Command("umount", filepath.Join(mountPoint, "proc")).Run()
	}()

	// apt-get update
	cmd := exec.Command("chroot", mountPoint, "apt-get", "update")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("apt-get update failed: %w", err)
	}

	// apt-get install base packages (systemd required for service management)
	args := []string{mountPoint, "apt-get", "install", "-y", "--no-install-recommends"}
	args = append(args, "ca-certificates", "systemd", "systemd-sysv")
	// Split packages string
	for _, pkg := range splitPackages(packages) {
		args = append(args, pkg)
	}
	cmd = exec.Command("chroot", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("apt-get install failed: %w", err)
	}

	// apt-get clean
	exec.Command("chroot", mountPoint, "apt-get", "clean").Run()

	return nil
}

func (b *Builder) runPostInstall(mountPoint, script string) error {
	// Mount /proc for post-install
	exec.Command("mount", "-t", "proc", "proc", filepath.Join(mountPoint, "proc")).Run()
	exec.Command("mount", "-t", "devtmpfs", "dev", filepath.Join(mountPoint, "dev")).Run()
	defer func() {
		exec.Command("umount", filepath.Join(mountPoint, "dev")).Run()
		exec.Command("umount", filepath.Join(mountPoint, "proc")).Run()
	}()

	cmd := exec.Command("chroot", mountPoint, "/bin/bash", "-c", script)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func (b *Builder) installGuestRunner(mountPoint string) error {
	dest := filepath.Join(mountPoint, "usr/local/bin/infra.operator")
	if err := os.MkdirAll(filepath.Dir(dest), 0755); err != nil {
		return err
	}

	// Copy infra.operator binary (contains guest runner)
	cmd := exec.Command("cp", b.guestRunner, dest)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to copy infra.operator: %w", err)
	}

	return os.Chmod(dest, 0755)
}

func (b *Builder) createSystemdService(mountPoint string) error {
	serviceDir := filepath.Join(mountPoint, "etc/systemd/system")
	if err := os.MkdirAll(serviceDir, 0755); err != nil {
		return err
	}

	service := `[Unit]
Description=LLM Firecracker Guest Runner
After=network.target

[Service]
Type=simple
ExecStart=/usr/local/bin/infra.operator guest --port 5000
Restart=always
RestartSec=1

[Install]
WantedBy=multi-user.target
`
	servicePath := filepath.Join(serviceDir, "guest-runner.service")
	if err := os.WriteFile(servicePath, []byte(service), 0644); err != nil {
		return err
	}

	// Enable service
	wantsDir := filepath.Join(serviceDir, "multi-user.target.wants")
	if err := os.MkdirAll(wantsDir, 0755); err != nil {
		return err
	}

	return os.Symlink("/etc/systemd/system/guest-runner.service",
		filepath.Join(wantsDir, "guest-runner.service"))
}

func (b *Builder) cleanup(mountPoint, imagePath string) {
	exec.Command("umount", filepath.Join(mountPoint, "dev")).Run()
	exec.Command("umount", filepath.Join(mountPoint, "proc")).Run()
	exec.Command("umount", mountPoint).Run()
	os.RemoveAll(mountPoint)
	os.Remove(imagePath)
}

func (b *Builder) unmount(mountPoint string) {
	exec.Command("umount", filepath.Join(mountPoint, "dev")).Run()
	exec.Command("umount", filepath.Join(mountPoint, "proc")).Run()
	exec.Command("umount", mountPoint).Run()
	os.RemoveAll(mountPoint)
}

// createBinarySymlinks creates symlinks in /usr/local/bin for binaries in non-standard locations
// This is needed because some Docker images (like golang:alpine) place binaries in paths
// that are not in the default PATH (e.g., /usr/local/go/bin/go)
func (b *Builder) createBinarySymlinks(mountPoint, name string) {
	// Map of language name to binary locations that need symlinks
	// Format: source path -> destination name in /usr/local/bin
	symlinkMap := map[string]map[string]string{
		"go": {
			"/usr/local/go/bin/go":    "go",
			"/usr/local/go/bin/gofmt": "gofmt",
		},
		"julia": {
			"/opt/julia/bin/julia": "julia",
		},
		"zig": {
			"/opt/zig/zig": "zig",
		},
		"nim": {
			"/opt/nim/bin/nim": "nim",
		},
	}

	// Ensure /usr/local/bin exists
	localBin := filepath.Join(mountPoint, "usr/local/bin")
	os.MkdirAll(localBin, 0755)

	// Special handling for Rust - rust:alpine uses rustup with symlinks
	// We need to find the actual toolchain path and symlink directly to it
	if name == "rust" {
		b.createRustSymlinks(mountPoint)
		return
	}

	binLinks, ok := symlinkMap[name]
	if !ok {
		return
	}

	for srcPath, destName := range binLinks {
		fullSrc := filepath.Join(mountPoint, srcPath)
		fullDest := filepath.Join(localBin, destName)

		// Check if source exists
		if _, err := os.Stat(fullSrc); os.IsNotExist(err) {
			continue
		}

		// Check if destination already exists
		if _, err := os.Stat(fullDest); err == nil {
			continue // Already exists, skip
		}

		// Create symlink
		if err := os.Symlink(srcPath, fullDest); err != nil {
			fmt.Printf("    Warning: failed to create symlink %s -> %s: %v\n", destName, srcPath, err)
		} else {
			fmt.Printf("    Created symlink: %s -> %s\n", destName, srcPath)
		}
	}
}

// createRustSymlinks handles rust:alpine special case with rustup
// The rust:alpine image uses rustup, so we need to find the actual toolchain
func (b *Builder) createRustSymlinks(mountPoint string) {
	localBin := filepath.Join(mountPoint, "usr/local/bin")
	toolchainsDir := filepath.Join(mountPoint, "usr/local/rustup/toolchains")

	// Find the toolchain directory (e.g., 1.91.1-x86_64-unknown-linux-musl)
	entries, err := os.ReadDir(toolchainsDir)
	if err != nil {
		fmt.Printf("    Warning: failed to read toolchains dir: %v\n", err)
		return
	}

	var toolchainName string
	for _, entry := range entries {
		if entry.IsDir() && strings.Contains(entry.Name(), "x86_64") {
			toolchainName = entry.Name()
			break
		}
	}

	if toolchainName == "" {
		fmt.Printf("    Warning: no x86_64 toolchain found\n")
		return
	}

	fmt.Printf("    Found Rust toolchain: %s\n", toolchainName)

	// Create symlinks to actual binaries in the toolchain
	binaries := map[string]string{
		"rustc": "rustc",
		"cargo": "cargo",
	}

	for binName, destName := range binaries {
		// The actual binary is in /usr/local/rustup/toolchains/VERSION/bin/
		srcPath := fmt.Sprintf("/usr/local/rustup/toolchains/%s/bin/%s", toolchainName, binName)
		fullSrc := filepath.Join(mountPoint, srcPath)
		fullDest := filepath.Join(localBin, destName)

		// Check if source exists
		if _, err := os.Stat(fullSrc); os.IsNotExist(err) {
			fmt.Printf("    Warning: %s not found at %s\n", binName, fullSrc)
			continue
		}

		// Remove existing symlink if it exists
		os.Remove(fullDest)

		// Create symlink
		if err := os.Symlink(srcPath, fullDest); err != nil {
			fmt.Printf("    Warning: failed to create symlink %s -> %s: %v\n", destName, srcPath, err)
		} else {
			fmt.Printf("    Created symlink: %s -> %s\n", destName, srcPath)
		}
	}

	// Also set RUSTUP_HOME and CARGO_HOME environment via profile.d
	profileDir := filepath.Join(mountPoint, "etc/profile.d")
	os.MkdirAll(profileDir, 0755)
	envScript := `#!/bin/sh
export RUSTUP_HOME=/usr/local/rustup
export CARGO_HOME=/usr/local/cargo
export PATH="/usr/local/rustup/toolchains/` + toolchainName + `/bin:$PATH"
`
	os.WriteFile(filepath.Join(profileDir, "rust.sh"), []byte(envScript), 0755)
}

func splitPackages(s string) []string {
	var result []string
	current := ""
	for _, c := range s {
		// Accept both comma and space as separators
		if c == ',' || c == ' ' {
			if current != "" {
				result = append(result, current)
				current = ""
			}
		} else {
			current += string(c)
		}
	}
	if current != "" {
		result = append(result, current)
	}
	return result
}

// List lists local rootfs images
func (b *Builder) List() ([]RootfsInfo, error) {
	pattern := filepath.Join(b.cfg.ImagesDir, "rootfs-*.ext4")
	matches, err := filepath.Glob(pattern)
	if err != nil {
		return nil, err
	}

	var result []RootfsInfo
	for _, path := range matches {
		info, err := os.Stat(path)
		if err != nil {
			continue
		}

		// Extract language name
		base := filepath.Base(path)
		lang := base[7 : len(base)-5] // Remove "rootfs-" prefix and ".ext4" suffix

		result = append(result, RootfsInfo{
			Language: lang,
			Path:     path,
			Size:     info.Size(),
			ModTime:  info.ModTime(),
		})
	}

	return result, nil
}

// RootfsInfo contains rootfs metadata
type RootfsInfo struct {
	Language string
	Path     string
	Size     int64
	ModTime  interface{}
}

// BuildFromDocker creates a rootfs from an official Docker image
// This is the recommended way to create rootfs for Node.js, Python, etc.
// as the official images are well-tested and optimized.
//
// Example images:
//   - node:22-alpine3.20 (Node.js 22 LTS on Alpine)
//   - python:3.12-alpine3.21 (Python 3.12 on Alpine)
//   - golang:1.22-alpine3.20 (Go 1.22 on Alpine)
func (b *Builder) BuildFromDocker(ctx context.Context, name string, sizeMB int, dockerImage string, outputPath string) error {
	if outputPath == "" {
		outputPath = b.cfg.RootfsPath(name)
	}

	mountPoint := fmt.Sprintf("/mnt/%s", name)
	tmpImage := fmt.Sprintf("/tmp/rootfs-%s.ext4", name)
	containerName := fmt.Sprintf("rootfs-export-%s", name)

	// Cleanup any previous attempt
	b.cleanup(mountPoint, tmpImage)
	exec.Command("docker", "rm", "-f", containerName).Run()

	fmt.Printf("Creating rootfs for %s from Docker image...\n", name)
	fmt.Printf("  Docker Image: %s\n", dockerImage)
	fmt.Printf("  Size: %d MB\n", sizeMB)

	// Step 1: Pull Docker image
	fmt.Printf("[1/6] Pulling Docker image: %s...\n", dockerImage)
	pullCmd := exec.Command("docker", "pull", dockerImage)
	pullCmd.Stdout = os.Stdout
	pullCmd.Stderr = os.Stderr
	if err := pullCmd.Run(); err != nil {
		return fmt.Errorf("docker pull failed: %w", err)
	}

	// Step 2: Create ext4 image
	fmt.Printf("[2/6] Creating ext4 image (%dMB)...\n", sizeMB)
	if err := b.createImage(tmpImage, sizeMB); err != nil {
		return fmt.Errorf("failed to create image: %w", err)
	}

	// Mount the image
	if err := os.MkdirAll(mountPoint, 0755); err != nil {
		return fmt.Errorf("failed to create mount point: %w", err)
	}

	mountCmd := exec.Command("mount", "-o", "loop", tmpImage, mountPoint)
	if err := mountCmd.Run(); err != nil {
		return fmt.Errorf("mount failed: %w", err)
	}

	// Step 3: Export Docker image filesystem
	fmt.Printf("[3/6] Exporting Docker filesystem...\n")

	// Create a container (don't start it)
	createCmd := exec.Command("docker", "create", "--name", containerName, dockerImage)
	if err := createCmd.Run(); err != nil {
		b.cleanup(mountPoint, tmpImage)
		return fmt.Errorf("docker create failed: %w", err)
	}

	// Export the container filesystem
	exportCmd := exec.Command("docker", "export", containerName)
	tarCmd := exec.Command("tar", "-xf", "-", "-C", mountPoint)

	// Pipe docker export to tar
	pipe, err := exportCmd.StdoutPipe()
	if err != nil {
		exec.Command("docker", "rm", "-f", containerName).Run()
		b.cleanup(mountPoint, tmpImage)
		return fmt.Errorf("failed to create pipe: %w", err)
	}
	tarCmd.Stdin = pipe
	tarCmd.Stderr = os.Stderr

	if err := exportCmd.Start(); err != nil {
		exec.Command("docker", "rm", "-f", containerName).Run()
		b.cleanup(mountPoint, tmpImage)
		return fmt.Errorf("docker export start failed: %w", err)
	}

	if err := tarCmd.Start(); err != nil {
		exportCmd.Process.Kill()
		exec.Command("docker", "rm", "-f", containerName).Run()
		b.cleanup(mountPoint, tmpImage)
		return fmt.Errorf("tar start failed: %w", err)
	}

	if err := exportCmd.Wait(); err != nil {
		tarCmd.Process.Kill()
		exec.Command("docker", "rm", "-f", containerName).Run()
		b.cleanup(mountPoint, tmpImage)
		return fmt.Errorf("docker export failed: %w", err)
	}

	if err := tarCmd.Wait(); err != nil {
		exec.Command("docker", "rm", "-f", containerName).Run()
		b.cleanup(mountPoint, tmpImage)
		return fmt.Errorf("tar extract failed: %w", err)
	}

	// Cleanup container
	exec.Command("docker", "rm", "-f", containerName).Run()

	// Step 4: Detect init system and add OpenRC if needed (Alpine uses OpenRC)
	fmt.Printf("[4/6] Configuring init system...\n")

	// Check if it's Alpine (has /etc/alpine-release)
	isAlpine := false
	if _, err := os.Stat(filepath.Join(mountPoint, "etc/alpine-release")); err == nil {
		isAlpine = true
	}

	if isAlpine {
		// Install OpenRC if not present
		if _, err := os.Stat(filepath.Join(mountPoint, "sbin/openrc")); os.IsNotExist(err) {
			fmt.Printf("  Installing OpenRC for Alpine...\n")
			// Mount /proc and /dev for apk
			exec.Command("mount", "-t", "proc", "proc", filepath.Join(mountPoint, "proc")).Run()
			exec.Command("mount", "-t", "devtmpfs", "dev", filepath.Join(mountPoint, "dev")).Run()

			// Copy resolv.conf
			exec.Command("cp", "/etc/resolv.conf", filepath.Join(mountPoint, "etc/resolv.conf")).Run()

			apkCmd := exec.Command("chroot", mountPoint, "apk", "add", "--no-cache", "openrc")
			apkCmd.Stdout = os.Stdout
			apkCmd.Stderr = os.Stderr
			apkCmd.Run() // Ignore errors, some images may not have apk

			exec.Command("umount", filepath.Join(mountPoint, "dev")).Run()
			exec.Command("umount", filepath.Join(mountPoint, "proc")).Run()
		}
	}

	// Step 4.5: Create symlinks for binaries in non-standard locations
	fmt.Printf("  Creating symlinks for binaries...\n")
	b.createBinarySymlinks(mountPoint, name)

	// Step 5: Install guest-runner
	fmt.Printf("[5/6] Installing guest-runner...\n")
	if err := b.installGuestRunner(mountPoint); err != nil {
		b.cleanup(mountPoint, tmpImage)
		return fmt.Errorf("failed to install guest-runner: %w", err)
	}

	// Step 6: Create service
	fmt.Printf("[6/6] Creating service...\n")
	if isAlpine {
		if err := b.createOpenRCService(mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to create OpenRC service: %w", err)
		}
		// Ensure /proc is mounted on boot by adding to /etc/init.d/boot
		b.ensureProcMount(mountPoint)
	} else {
		if err := b.createSystemdService(mountPoint); err != nil {
			b.cleanup(mountPoint, tmpImage)
			return fmt.Errorf("failed to create systemd service: %w", err)
		}
	}

	// Sync filesystem before unmount to prevent corruption
	fmt.Printf("Syncing filesystem...\n")
	exec.Command("sync").Run()

	// Finalize
	b.unmount(mountPoint)

	// Move to final location
	if err := os.MkdirAll(filepath.Dir(outputPath), 0755); err != nil {
		return fmt.Errorf("failed to create output dir: %w", err)
	}

	fmt.Printf("Moving image to %s...\n", outputPath)
	cpCmd := exec.Command("cp", tmpImage, outputPath)
	cpCmd.Stdout = os.Stdout
	cpCmd.Stderr = os.Stderr
	if err := cpCmd.Run(); err != nil {
		return fmt.Errorf("failed to copy image: %w", err)
	}
	os.Remove(tmpImage)

	fmt.Printf("Created: %s\n", outputPath)
	return nil
}
