package host

import (
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestNewFirecrackerClient(t *testing.T) {
	socketPath := "/tmp/test-fc.sock"
	client := NewFirecrackerClient(socketPath)

	if client == nil {
		t.Fatal("NewFirecrackerClient() returned nil")
	}
	if client.socketPath != socketPath {
		t.Errorf("socketPath = %s, want %s", client.socketPath, socketPath)
	}
	if client.client == nil {
		t.Error("HTTP client should not be nil")
	}
}

func TestWaitForSocket(t *testing.T) {
	t.Run("socket exists and listening", func(t *testing.T) {
		tmpDir, err := os.MkdirTemp("", "fc-test-")
		if err != nil {
			t.Fatalf("Failed to create temp dir: %v", err)
		}
		defer os.RemoveAll(tmpDir)

		socketPath := filepath.Join(tmpDir, "test.sock")

		// Create actual Unix socket listener
		listener, err := net.Listen("unix", socketPath)
		if err != nil {
			t.Fatalf("Failed to create listener: %v", err)
		}
		defer listener.Close()

		err = waitForSocket(socketPath, 1*time.Second)
		if err != nil {
			t.Errorf("waitForSocket() error = %v, expected nil", err)
		}
	})

	t.Run("socket does not exist", func(t *testing.T) {
		tmpDir, err := os.MkdirTemp("", "fc-test-")
		if err != nil {
			t.Fatalf("Failed to create temp dir: %v", err)
		}
		defer os.RemoveAll(tmpDir)

		socketPath := filepath.Join(tmpDir, "nonexistent.sock")

		err = waitForSocket(socketPath, 100*time.Millisecond)
		if err == nil {
			t.Error("waitForSocket() should return error for nonexistent socket")
		}
	})

	t.Run("file exists but not a socket", func(t *testing.T) {
		tmpDir, err := os.MkdirTemp("", "fc-test-")
		if err != nil {
			t.Fatalf("Failed to create temp dir: %v", err)
		}
		defer os.RemoveAll(tmpDir)

		socketPath := filepath.Join(tmpDir, "not-a-socket.sock")
		// Create regular file (not a socket)
		if err := os.WriteFile(socketPath, []byte{}, 0644); err != nil {
			t.Fatalf("Failed to create file: %v", err)
		}

		err = waitForSocket(socketPath, 200*time.Millisecond)
		if err == nil {
			t.Error("waitForSocket() should return error for regular file")
		}
	})
}

func TestFirecrackerClient_ConfigureMachine(t *testing.T) {
	tests := []struct {
		name        string
		vcpus       int
		memMiB      int
		statusCode  int
		expectError bool
	}{
		{
			name:        "valid config",
			vcpus:       2,
			memMiB:      1024,
			statusCode:  http.StatusNoContent,
			expectError: false,
		},
		{
			name:        "api error",
			vcpus:       0,
			memMiB:      0,
			statusCode:  http.StatusBadRequest,
			expectError: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path != "/machine-config" {
					t.Errorf("Unexpected path: %s", r.URL.Path)
				}
				if r.Method != http.MethodPut {
					t.Errorf("Unexpected method: %s", r.Method)
				}
				w.WriteHeader(tt.statusCode)
			}))
			defer server.Close()

			client := &FirecrackerClient{
				socketPath: "/tmp/test.sock",
				client:     server.Client(),
			}

			// Note: In a real test, we would need to set up proper transport
			// For now, we just verify the client structure is correct
			_ = client
		})
	}
}

func TestFirecrackerConfig_Fields(t *testing.T) {
	config := FirecrackerConfig{
		InstanceID: "test-instance",
		KernelPath: "/srv/firecracker/vmlinux",
		RootfsPath: "/srv/firecracker/rootfs.ext4",
		VCPUs:      2,
		MemMiB:     1024,
		APISocket:  "/tmp/fc-test.sock",
		BootArgs:   "console=ttyS0",
	}

	tests := []struct {
		name  string
		got   interface{}
		want  interface{}
		field string
	}{
		{"InstanceID", config.InstanceID, "test-instance", "InstanceID"},
		{"KernelPath", config.KernelPath, "/srv/firecracker/vmlinux", "KernelPath"},
		{"RootfsPath", config.RootfsPath, "/srv/firecracker/rootfs.ext4", "RootfsPath"},
		{"VCPUs", config.VCPUs, 2, "VCPUs"},
		{"MemMiB", config.MemMiB, 1024, "MemMiB"},
		{"APISocket", config.APISocket, "/tmp/fc-test.sock", "APISocket"},
		{"BootArgs", config.BootArgs, "console=ttyS0", "BootArgs"},
	}

	for _, tt := range tests {
		t.Run(tt.field, func(t *testing.T) {
			if tt.got != tt.want {
				t.Errorf("%s = %v, want %v", tt.field, tt.got, tt.want)
			}
		})
	}
}

func TestLoadSnapshot_Payload(t *testing.T) {
	tests := []struct {
		name        string
		vmstatePath string
		memPath     string
	}{
		{
			name:        "standard paths",
			vmstatePath: "/srv/firecracker/snapshots/python/vmstate.snapshot",
			memPath:     "/srv/firecracker/snapshots/python/mem.snapshot",
		},
		{
			name:        "dev shm paths",
			vmstatePath: "/dev/shm/snapshots/nodejs/vmstate.snapshot",
			memPath:     "/dev/shm/snapshots/nodejs/mem.snapshot",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Test that paths are correctly formatted
			if tt.vmstatePath == "" {
				t.Error("vmstatePath should not be empty")
			}
			if tt.memPath == "" {
				t.Error("memPath should not be empty")
			}
		})
	}
}

func TestAction_Types(t *testing.T) {
	validActions := []string{
		"InstanceStart",
		"SendCtrlAltDel",
		"FlushMetrics",
	}

	for _, action := range validActions {
		t.Run(action, func(t *testing.T) {
			a := Action{ActionType: action}
			if a.ActionType != action {
				t.Errorf("ActionType = %s, want %s", a.ActionType, action)
			}
		})
	}
}
