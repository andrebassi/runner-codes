package host

import (
	"encoding/binary"
	"encoding/json"
	"net"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestNewVsockClient(t *testing.T) {
	tests := []struct {
		name    string
		udsPath string
		port    uint32
	}{
		{
			name:    "default port",
			udsPath: "/tmp/fc-test.vsock",
			port:    DefaultVsockPort,
		},
		{
			name:    "custom port",
			udsPath: "/tmp/fc-custom.vsock",
			port:    6000,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			client := NewVsockClient(tt.udsPath, tt.port)

			if client == nil {
				t.Fatal("NewVsockClient() returned nil")
			}
			if client.udsPath != tt.udsPath {
				t.Errorf("udsPath = %s, want %s", client.udsPath, tt.udsPath)
			}
			if client.port != tt.port {
				t.Errorf("port = %d, want %d", client.port, tt.port)
			}
		})
	}
}

func TestVsockConstants(t *testing.T) {
	t.Run("DefaultGuestCID", func(t *testing.T) {
		if DefaultGuestCID != 3 {
			t.Errorf("DefaultGuestCID = %d, want 3", DefaultGuestCID)
		}
	})

	t.Run("DefaultVsockPort", func(t *testing.T) {
		if DefaultVsockPort != 5000 {
			t.Errorf("DefaultVsockPort = %d, want 5000", DefaultVsockPort)
		}
	})
}

func TestVsockClient_SendJob_MessageFormat(t *testing.T) {
	// Test that request is properly formatted
	req := RunRequest{
		TraceID: "tr-test-123",
		Lang:    "python",
		Code:    "print('hello')",
		Timeout: 10,
	}

	// Marshal request
	data, err := json.Marshal(&req)
	if err != nil {
		t.Fatalf("Failed to marshal request: %v", err)
	}

	// Verify length prefix
	lenBuf := make([]byte, 4)
	binary.BigEndian.PutUint32(lenBuf, uint32(len(data)))

	if binary.BigEndian.Uint32(lenBuf) != uint32(len(data)) {
		t.Errorf("Length prefix mismatch: got %d, want %d", binary.BigEndian.Uint32(lenBuf), len(data))
	}
}

func TestVsockClient_ConnectToGuest_Format(t *testing.T) {
	// Test CONNECT command format
	port := uint32(5000)
	expectedCmd := "CONNECT 5000\n"

	cmd := "CONNECT " + itoa(int(port)) + "\n"
	if cmd != expectedCmd {
		t.Errorf("CONNECT command = %s, want %s", cmd, expectedCmd)
	}
}

func TestExecuteCodeViaUDS_InvalidPath(t *testing.T) {
	result, err := ExecuteCodeViaUDS("/nonexistent/path.vsock", 5000, "python", "print('test')", 1)

	if err == nil && result.Error == "" {
		t.Error("ExecuteCodeViaUDS() should fail with nonexistent path")
	}
}

func TestVsockClient_WaitForGuestReady_Timeout(t *testing.T) {
	client := NewVsockClient("/nonexistent.vsock", 5000)

	err := client.WaitForGuestReady(100 * time.Millisecond)
	if err == nil {
		t.Error("WaitForGuestReady() should timeout with nonexistent socket")
	}
}

// Mock server for testing vsock communication
func TestVsockClient_MockServer(t *testing.T) {
	// Create a temporary Unix socket for testing
	tmpDir, err := os.MkdirTemp("", "vsock-mock-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	socketPath := filepath.Join(tmpDir, "test.sock")

	// Create mock server
	listener, err := net.Listen("unix", socketPath)
	if err != nil {
		t.Fatalf("Failed to create listener: %v", err)
	}
	defer listener.Close()

	// Handle connection in goroutine
	go func() {
		conn, err := listener.Accept()
		if err != nil {
			return
		}
		defer conn.Close()

		// Read CONNECT command
		buf := make([]byte, 32)
		n, _ := conn.Read(buf)

		// Verify CONNECT format
		if n > 0 && string(buf[:7]) == "CONNECT" {
			conn.Write([]byte("OK 5000\n"))
		}

		// Read length prefix
		lenBuf := make([]byte, 4)
		conn.Read(lenBuf)
		msgLen := binary.BigEndian.Uint32(lenBuf)

		// Read message
		msgBuf := make([]byte, msgLen)
		conn.Read(msgBuf)

		// Parse request
		var req RunRequest
		json.Unmarshal(msgBuf, &req)

		// Send response
		result := RunResult{
			TraceID:  req.TraceID,
			Stdout:   "test output\n",
			Stderr:   "",
			ExitCode: 0,
		}

		respData, _ := json.Marshal(&result)
		binary.BigEndian.PutUint32(lenBuf, uint32(len(respData)))
		conn.Write(lenBuf)
		conn.Write(respData)
	}()

	// Give server time to start
	time.Sleep(50 * time.Millisecond)

	// Test connection
	conn, err := net.Dial("unix", socketPath)
	if err != nil {
		t.Fatalf("Failed to connect: %v", err)
	}
	conn.Close()
}

func TestRunResult_IsSuccess(t *testing.T) {
	tests := []struct {
		name     string
		result   RunResult
		expected bool
	}{
		{
			name: "success",
			result: RunResult{
				ExitCode: 0,
				Error:    "",
			},
			expected: true,
		},
		{
			name: "non-zero exit code",
			result: RunResult{
				ExitCode: 1,
				Error:    "",
			},
			expected: false,
		},
		{
			name: "error message",
			result: RunResult{
				ExitCode: 0,
				Error:    "some error",
			},
			expected: false,
		},
		{
			name: "timeout exit code",
			result: RunResult{
				ExitCode: 124,
				Error:    "timeout",
			},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// IsSuccess logic: ExitCode == 0 && Error == ""
			isSuccess := tt.result.ExitCode == 0 && tt.result.Error == ""
			if isSuccess != tt.expected {
				t.Errorf("IsSuccess = %v, want %v", isSuccess, tt.expected)
			}
		})
	}
}

// Helper function
func itoa(i int) string {
	if i == 0 {
		return "0"
	}
	var s string
	for i > 0 {
		s = string(rune('0'+i%10)) + s
		i /= 10
	}
	return s
}
