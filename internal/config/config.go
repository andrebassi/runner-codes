package config

import (
	"os"
	"path/filepath"
)

// Config holds all configuration
type Config struct {
	WorkDir      string
	ImagesDir    string
	SnapshotsDir string
	S3Bucket     string
	S3Region     string
	Firecracker  FirecrackerConfig
	GuestRunner  string
}

// FirecrackerConfig holds Firecracker-specific config
type FirecrackerConfig struct {
	BinaryPath string
	KernelPath string
	DefaultMem int
	DefaultCPU int
}

// DefaultConfig returns default configuration
func DefaultConfig() *Config {
	return &Config{
		WorkDir:      "/srv/firecracker",
		ImagesDir:    "/srv/firecracker/images",
		SnapshotsDir: "/dev/shm/snapshots",
		S3Bucket:     "runner-codes",
		S3Region:     "us-east-1",
		Firecracker: FirecrackerConfig{
			BinaryPath: "/usr/local/bin/firecracker",
			KernelPath: "/srv/firecracker/vmlinux",
			DefaultMem: 512,
			DefaultCPU: 1,
		},
		GuestRunner: "/usr/local/bin/infra.operator",
	}
}

// LoadFromEnv loads config from environment variables
func (c *Config) LoadFromEnv() {
	if v := os.Getenv("FC_WORKDIR"); v != "" {
		c.WorkDir = v
		c.ImagesDir = filepath.Join(v, "images")
	}
	if v := os.Getenv("FC_IMAGES_DIR"); v != "" {
		c.ImagesDir = v
	}
	if v := os.Getenv("FC_SNAPSHOTS_DIR"); v != "" {
		c.SnapshotsDir = v
	}
	if v := os.Getenv("S3_BUCKET"); v != "" {
		c.S3Bucket = v
	}
	if v := os.Getenv("AWS_DEFAULT_REGION"); v != "" {
		c.S3Region = v
	}
	if v := os.Getenv("FC_BINARY"); v != "" {
		c.Firecracker.BinaryPath = v
	}
	if v := os.Getenv("FC_KERNEL"); v != "" {
		c.Firecracker.KernelPath = v
	}
	if v := os.Getenv("GUEST_RUNNER"); v != "" {
		c.GuestRunner = v
	}
}

// RootfsPath returns the path for a language rootfs
func (c *Config) RootfsPath(lang string) string {
	return filepath.Join(c.ImagesDir, "rootfs-"+lang+".ext4")
}

// SnapshotDir returns the directory for a language snapshot
func (c *Config) SnapshotDir(lang string) string {
	return filepath.Join(c.SnapshotsDir, lang)
}
