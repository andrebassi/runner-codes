package snapshot

import (
	"context"
	"encoding/binary"
	"encoding/json"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"time"

	"infra-operator/internal/config"
	"infra-operator/internal/firecracker"
)

// Manager handles snapshot operations
type Manager struct {
	cfg *config.Config
}

// NewManager creates a new snapshot manager
func NewManager(cfg *config.Config) *Manager {
	return &Manager{cfg: cfg}
}

// Create creates a new snapshot for a language
func (m *Manager) Create(ctx context.Context, lang string, memMB, vcpus int) error {
	rootfsPath := m.cfg.RootfsPath(lang)
	if _, err := os.Stat(rootfsPath); os.IsNotExist(err) {
		return fmt.Errorf("rootfs not found: %s", rootfsPath)
	}

	snapshotDir := m.cfg.SnapshotDir(lang)
	if err := os.MkdirAll(snapshotDir, 0755); err != nil {
		return fmt.Errorf("failed to create snapshot dir: %w", err)
	}

	// Use defaults if not specified
	if memMB == 0 {
		memMB = m.cfg.Firecracker.DefaultMem
	}
	if vcpus == 0 {
		vcpus = m.cfg.Firecracker.DefaultCPU
	}

	fmt.Printf("Creating snapshot for %s...\n", lang)
	fmt.Printf("  Rootfs: %s\n", rootfsPath)
	fmt.Printf("  Memory: %d MB\n", memMB)
	fmt.Printf("  vCPUs:  %d\n", vcpus)

	// Create Firecracker client
	fc := firecracker.NewClient(m.cfg.Firecracker.BinaryPath, m.cfg.Firecracker.KernelPath)

	socketPath := fmt.Sprintf("/tmp/fc-snapshot-%s-%d.sock", lang, time.Now().UnixNano())
	vsockPath := fmt.Sprintf("/tmp/fc-%s.vsock", lang)

	// Start Firecracker
	fmt.Println("[1/5] Starting Firecracker...")
	fcCfg := firecracker.Config{
		SocketPath: socketPath,
		KernelPath: m.cfg.Firecracker.KernelPath,
		RootfsPath: rootfsPath,
		MemSizeMB:  memMB,
		VCPUs:      vcpus,
		VsockPath:  vsockPath,
	}

	if err := fc.Start(ctx, fcCfg); err != nil {
		return fmt.Errorf("failed to start firecracker: %w", err)
	}
	defer fc.Stop()

	// Configure VM
	fmt.Println("[2/5] Configuring VM...")
	if err := fc.ConfigureVM(ctx, fcCfg); err != nil {
		return fmt.Errorf("failed to configure VM: %w", err)
	}

	// Start VM
	fmt.Println("[3/5] Starting VM...")
	if err := fc.StartVM(ctx); err != nil {
		return fmt.Errorf("failed to start VM: %w", err)
	}

	// Wait for guest-runner to be ready and sync filesystem
	fmt.Println("[3.5/5] Waiting for guest-runner...")
	time.Sleep(3 * time.Second)

	// Send sync command to guest before snapshot
	fmt.Println("[3.6/5] Syncing filesystem...")
	if err := m.sendSyncCommand(vsockPath); err != nil {
		fmt.Printf("  Warning: sync command failed: %v (continuing anyway)\n", err)
	}
	time.Sleep(500 * time.Millisecond) // Extra wait for sync to complete

	// Pause VM
	fmt.Println("[4/5] Pausing VM...")
	if err := fc.PauseVM(ctx); err != nil {
		return fmt.Errorf("failed to pause VM: %w", err)
	}

	// Create snapshot
	fmt.Println("[5/5] Creating snapshot...")
	if err := fc.CreateSnapshot(ctx, snapshotDir); err != nil {
		return fmt.Errorf("failed to create snapshot: %w", err)
	}

	// Verify snapshot files
	vmstatePath := filepath.Join(snapshotDir, "vmstate.snapshot")
	memPath := filepath.Join(snapshotDir, "mem.snapshot")

	vmstateInfo, err := os.Stat(vmstatePath)
	if err != nil {
		return fmt.Errorf("vmstate.snapshot not created: %w", err)
	}

	memInfo, err := os.Stat(memPath)
	if err != nil {
		return fmt.Errorf("mem.snapshot not created: %w", err)
	}

	fmt.Printf("\nSnapshot created successfully!\n")
	fmt.Printf("  vmstate.snapshot: %d bytes\n", vmstateInfo.Size())
	fmt.Printf("  mem.snapshot:     %d bytes (%.2f MB)\n", memInfo.Size(), float64(memInfo.Size())/(1024*1024))
	fmt.Printf("  Location: %s\n", snapshotDir)

	return nil
}

// Load loads a snapshot and returns a running Firecracker client
func (m *Manager) Load(ctx context.Context, lang string) (*firecracker.Client, error) {
	snapshotDir := m.cfg.SnapshotDir(lang)

	// Verify snapshot exists
	vmstatePath := filepath.Join(snapshotDir, "vmstate.snapshot")
	memPath := filepath.Join(snapshotDir, "mem.snapshot")

	if _, err := os.Stat(vmstatePath); os.IsNotExist(err) {
		return nil, fmt.Errorf("vmstate.snapshot not found: %s", vmstatePath)
	}
	if _, err := os.Stat(memPath); os.IsNotExist(err) {
		return nil, fmt.Errorf("mem.snapshot not found: %s", memPath)
	}

	// Create symlink for rootfs (required by snapshot)
	rootfsPath := m.cfg.RootfsPath(lang)
	symlinkPath := fmt.Sprintf("/tmp/rootfs-snap-%s.ext4", lang)
	os.Remove(symlinkPath)
	if err := os.Symlink(rootfsPath, symlinkPath); err != nil {
		return nil, fmt.Errorf("failed to create rootfs symlink: %w", err)
	}

	// Also create symlink in /dev/shm (some snapshots reference this path)
	shmSymlink := fmt.Sprintf("/dev/shm/rootfs-snap-%s.ext4", lang)
	os.Remove(shmSymlink)
	os.Symlink(rootfsPath, shmSymlink)

	// Create Firecracker client
	fc := firecracker.NewClient(m.cfg.Firecracker.BinaryPath, m.cfg.Firecracker.KernelPath)

	socketPath := fmt.Sprintf("/tmp/fc-load-%s-%d.sock", lang, time.Now().UnixNano())

	// Start Firecracker (empty, will load snapshot)
	if err := fc.Start(ctx, firecracker.Config{SocketPath: socketPath}); err != nil {
		return nil, fmt.Errorf("failed to start firecracker: %w", err)
	}

	// Load snapshot
	if err := fc.LoadSnapshot(ctx, snapshotDir); err != nil {
		fc.Stop()
		return nil, fmt.Errorf("failed to load snapshot: %w", err)
	}

	return fc, nil
}

// List lists local snapshots
func (m *Manager) List() ([]SnapshotInfo, error) {
	entries, err := os.ReadDir(m.cfg.SnapshotsDir)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, err
	}

	var result []SnapshotInfo
	for _, entry := range entries {
		if !entry.IsDir() {
			continue
		}

		lang := entry.Name()
		snapshotDir := m.cfg.SnapshotDir(lang)

		vmstatePath := filepath.Join(snapshotDir, "vmstate.snapshot")
		memPath := filepath.Join(snapshotDir, "mem.snapshot")

		vmstateInfo, err := os.Stat(vmstatePath)
		if err != nil {
			continue
		}

		memInfo, err := os.Stat(memPath)
		if err != nil {
			continue
		}

		result = append(result, SnapshotInfo{
			Language:    lang,
			Path:        snapshotDir,
			VmstateSize: vmstateInfo.Size(),
			MemSize:     memInfo.Size(),
			ModTime:     memInfo.ModTime(),
		})
	}

	return result, nil
}

// SnapshotInfo contains snapshot metadata
type SnapshotInfo struct {
	Language    string
	Path        string
	VmstateSize int64
	MemSize     int64
	ModTime     time.Time
}

// sendSyncCommand sends a sync command to the guest-runner via vsock
func (m *Manager) sendSyncCommand(vsockPath string) error {
	// Connect to vsock UDS
	conn, err := net.DialTimeout("unix", vsockPath, 5*time.Second)
	if err != nil {
		return fmt.Errorf("connect to vsock: %w", err)
	}
	defer conn.Close()

	// Send CONNECT command for port 5000
	_, err = conn.Write([]byte("CONNECT 5000\n"))
	if err != nil {
		return fmt.Errorf("send CONNECT: %w", err)
	}

	// Read response - accept any "OK" response (format varies: "OK 5000\n" or "OK <CID>\n")
	buf := make([]byte, 64)
	conn.SetReadDeadline(time.Now().Add(3 * time.Second))
	n, err := conn.Read(buf)
	if err != nil {
		return fmt.Errorf("read CONNECT response: %w", err)
	}
	response := string(buf[:n])
	if len(response) < 2 || response[:2] != "OK" {
		return fmt.Errorf("unexpected response: %s", response)
	}

	// Send sync job request
	syncJob := map[string]interface{}{
		"trace_id": "sync-before-snapshot",
		"lang":     "bash",
		"code":     "sync && echo synced",
		"timeout":  5,
	}
	jobData, _ := json.Marshal(syncJob)

	// Send length-prefixed message
	lenBuf := make([]byte, 4)
	binary.BigEndian.PutUint32(lenBuf, uint32(len(jobData)))
	conn.Write(lenBuf)
	conn.Write(jobData)

	// Read response (we don't care about result, just that it completed)
	conn.SetReadDeadline(time.Now().Add(5 * time.Second))
	conn.Read(lenBuf)

	return nil
}
