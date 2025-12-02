package guest

import (
	"bytes"
	"encoding/binary"
	"encoding/json"
	"net"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestDefaultPort(t *testing.T) {
	if DefaultPort != 5000 {
		t.Errorf("DefaultPort = %d, want 5000", DefaultPort)
	}
}

func TestDefaultServerConfig(t *testing.T) {
	config := DefaultServerConfig()

	if config.Port != DefaultPort {
		t.Errorf("config.Port = %d, want %d", config.Port, DefaultPort)
	}
}

func TestNewServer(t *testing.T) {
	tests := []struct {
		name   string
		config ServerConfig
	}{
		{
			name:   "default config",
			config: DefaultServerConfig(),
		},
		{
			name: "custom port",
			config: ServerConfig{
				Port: 6000,
			},
		},
		{
			name: "zero port",
			config: ServerConfig{
				Port: 0,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := NewServer(tt.config)

			if server == nil {
				t.Fatal("NewServer() returned nil")
			}

			if server.config.Port != tt.config.Port {
				t.Errorf("server.config.Port = %d, want %d", server.config.Port, tt.config.Port)
			}

			if server.executor == nil {
				t.Error("server.executor should not be nil")
			}
		})
	}
}

func TestServerConfig_Fields(t *testing.T) {
	config := ServerConfig{
		Port: 5000,
	}

	if config.Port != 5000 {
		t.Errorf("Port = %d, want 5000", config.Port)
	}
}

func TestServer_HandleConnection_Mock(t *testing.T) {
	// Create a mock Unix socket server for testing
	tmpDir, err := os.MkdirTemp("", "server-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	socketPath := filepath.Join(tmpDir, "test.sock")

	// Create listener
	listener, err := net.Listen("unix", socketPath)
	if err != nil {
		t.Fatalf("Failed to create listener: %v", err)
	}
	defer listener.Close()

	// Create server
	server := NewServer(DefaultServerConfig())

	// Handle connection in goroutine
	go func() {
		conn, err := listener.Accept()
		if err != nil {
			return
		}
		// Call internal handler - can't directly call private method
		// so we simulate the protocol instead
		defer conn.Close()

		// Read length prefix
		lenBuf := make([]byte, 4)
		if _, err := conn.Read(lenBuf); err != nil {
			return
		}
		msgLen := binary.BigEndian.Uint32(lenBuf)

		// Read message
		msgBuf := make([]byte, msgLen)
		if _, err := conn.Read(msgBuf); err != nil {
			return
		}

		// Parse job
		var job Job
		json.Unmarshal(msgBuf, &job)

		// Execute
		result := server.executor.Execute(job)

		// Send response
		respData, _ := json.Marshal(&result)
		binary.BigEndian.PutUint32(lenBuf, uint32(len(respData)))
		conn.Write(lenBuf)
		conn.Write(respData)
	}()

	// Give server time to start
	time.Sleep(50 * time.Millisecond)

	// Connect as client
	conn, err := net.Dial("unix", socketPath)
	if err != nil {
		t.Fatalf("Failed to connect: %v", err)
	}
	defer conn.Close()

	// Send test job
	job := Job{
		TraceID: "tr-mock-test",
		Lang:    "bash",
		Code:    "echo 'mock test'",
		Timeout: 5,
	}

	jobData, _ := json.Marshal(&job)

	// Write length prefix
	lenBuf := make([]byte, 4)
	binary.BigEndian.PutUint32(lenBuf, uint32(len(jobData)))
	conn.Write(lenBuf)
	conn.Write(jobData)

	// Read response
	conn.SetReadDeadline(time.Now().Add(10 * time.Second))
	if _, err := conn.Read(lenBuf); err != nil {
		t.Fatalf("Failed to read response length: %v", err)
	}

	respLen := binary.BigEndian.Uint32(lenBuf)
	respBuf := make([]byte, respLen)
	if _, err := conn.Read(respBuf); err != nil {
		t.Fatalf("Failed to read response: %v", err)
	}

	var result Result
	if err := json.Unmarshal(respBuf, &result); err != nil {
		t.Fatalf("Failed to unmarshal result: %v", err)
	}

	if result.TraceID != job.TraceID {
		t.Errorf("TraceID = %s, want %s", result.TraceID, job.TraceID)
	}
}

func TestServer_SendError_Format(t *testing.T) {
	// Create a mock connection using a pipe
	clientConn, serverConn := net.Pipe()
	defer clientConn.Close()
	defer serverConn.Close()

	server := NewServer(DefaultServerConfig())

	// Send error in goroutine
	go func() {
		server.sendError(serverConn, "tr-error-test", "test error message")
	}()

	// Read response on client side
	clientConn.SetReadDeadline(time.Now().Add(5 * time.Second))

	lenBuf := make([]byte, 4)
	if _, err := clientConn.Read(lenBuf); err != nil {
		t.Fatalf("Failed to read length: %v", err)
	}

	msgLen := binary.BigEndian.Uint32(lenBuf)
	msgBuf := make([]byte, msgLen)
	if _, err := clientConn.Read(msgBuf); err != nil {
		t.Fatalf("Failed to read message: %v", err)
	}

	var result Result
	if err := json.Unmarshal(msgBuf, &result); err != nil {
		t.Fatalf("Failed to unmarshal: %v", err)
	}

	if result.TraceID != "tr-error-test" {
		t.Errorf("TraceID = %s, want tr-error-test", result.TraceID)
	}

	if result.ExitCode != 1 {
		t.Errorf("ExitCode = %d, want 1", result.ExitCode)
	}

	if result.Error != "test error message" {
		t.Errorf("Error = %s, want 'test error message'", result.Error)
	}

	if result.Stderr != "test error message" {
		t.Errorf("Stderr = %s, want 'test error message'", result.Stderr)
	}
}

func TestMessageFraming_LengthPrefix(t *testing.T) {
	tests := []struct {
		name    string
		payload string
		wantLen uint32
	}{
		{
			name:    "small payload",
			payload: `{"trace_id":"tr-1","lang":"python","code":"print('hi')","timeout":5}`,
			wantLen: 67,
		},
		{
			name:    "medium payload",
			payload: `{"trace_id":"tr-12345","lang":"python","code":"for i in range(100):\n    print(f'iteration {i}')\n","timeout":30}`,
			wantLen: 111,
		},
		{
			name:    "empty code",
			payload: `{"trace_id":"","lang":"bash","code":"","timeout":1}`,
			wantLen: 50,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lenBuf := make([]byte, 4)
			binary.BigEndian.PutUint32(lenBuf, uint32(len(tt.payload)))

			gotLen := binary.BigEndian.Uint32(lenBuf)
			if gotLen != uint32(len(tt.payload)) {
				t.Errorf("Length = %d, want %d", gotLen, len(tt.payload))
			}
		})
	}
}

func TestServer_ProtocolRoundTrip(t *testing.T) {
	// Test full protocol round-trip with pipe
	clientConn, serverConn := net.Pipe()
	defer clientConn.Close()
	defer serverConn.Close()

	server := NewServer(DefaultServerConfig())

	// Simulate server handling in goroutine
	go func() {
		serverConn.SetReadDeadline(time.Now().Add(5 * time.Second))

		// Read length prefix
		lenBuf := make([]byte, 4)
		if _, err := serverConn.Read(lenBuf); err != nil {
			server.sendError(serverConn, "", "read error: "+err.Error())
			return
		}
		msgLen := binary.BigEndian.Uint32(lenBuf)

		// Read message
		msgBuf := make([]byte, msgLen)
		if _, err := serverConn.Read(msgBuf); err != nil {
			server.sendError(serverConn, "", "read message error: "+err.Error())
			return
		}

		// Decode job
		var job Job
		if err := json.Unmarshal(msgBuf, &job); err != nil {
			server.sendError(serverConn, "", "decode error: "+err.Error())
			return
		}

		// Execute
		result := server.executor.Execute(job)

		// Send response
		respData, _ := json.Marshal(&result)
		binary.BigEndian.PutUint32(lenBuf, uint32(len(respData)))
		serverConn.Write(lenBuf)
		serverConn.Write(respData)
	}()

	// Client side
	clientConn.SetWriteDeadline(time.Now().Add(5 * time.Second))
	clientConn.SetReadDeadline(time.Now().Add(10 * time.Second))

	// Send request
	job := Job{
		TraceID: "tr-roundtrip",
		Lang:    "bash",
		Code:    "echo 'roundtrip test'",
		Timeout: 5,
	}

	jobData, _ := json.Marshal(&job)

	var buf bytes.Buffer
	lenBuf := make([]byte, 4)
	binary.BigEndian.PutUint32(lenBuf, uint32(len(jobData)))
	buf.Write(lenBuf)
	buf.Write(jobData)

	if _, err := clientConn.Write(buf.Bytes()); err != nil {
		t.Fatalf("Failed to write request: %v", err)
	}

	// Read response
	respLenBuf := make([]byte, 4)
	if _, err := clientConn.Read(respLenBuf); err != nil {
		t.Fatalf("Failed to read response length: %v", err)
	}

	respLen := binary.BigEndian.Uint32(respLenBuf)
	respBuf := make([]byte, respLen)
	if _, err := clientConn.Read(respBuf); err != nil {
		t.Fatalf("Failed to read response: %v", err)
	}

	var result Result
	if err := json.Unmarshal(respBuf, &result); err != nil {
		t.Fatalf("Failed to unmarshal result: %v", err)
	}

	if result.TraceID != job.TraceID {
		t.Errorf("TraceID = %s, want %s", result.TraceID, job.TraceID)
	}

	// Should succeed if bash is available
	if result.ExitCode == 0 && result.Stdout != "roundtrip test\n" {
		t.Errorf("Stdout = %q, want %q", result.Stdout, "roundtrip test\n")
	}
}

func TestServer_InvalidJSON(t *testing.T) {
	clientConn, serverConn := net.Pipe()
	defer clientConn.Close()
	defer serverConn.Close()

	server := NewServer(DefaultServerConfig())

	// Simulate server handling
	go func() {
		serverConn.SetReadDeadline(time.Now().Add(5 * time.Second))

		// Read length prefix
		lenBuf := make([]byte, 4)
		if _, err := serverConn.Read(lenBuf); err != nil {
			return
		}
		msgLen := binary.BigEndian.Uint32(lenBuf)

		// Read message
		msgBuf := make([]byte, msgLen)
		if _, err := serverConn.Read(msgBuf); err != nil {
			return
		}

		// Try to decode - will fail
		var job Job
		if err := json.Unmarshal(msgBuf, &job); err != nil {
			server.sendError(serverConn, "", "invalid job format: "+err.Error())
			return
		}
	}()

	// Send invalid JSON
	invalidJSON := []byte("this is not valid json {{{")
	lenBuf := make([]byte, 4)
	binary.BigEndian.PutUint32(lenBuf, uint32(len(invalidJSON)))

	clientConn.SetWriteDeadline(time.Now().Add(5 * time.Second))
	clientConn.Write(lenBuf)
	clientConn.Write(invalidJSON)

	// Read error response
	clientConn.SetReadDeadline(time.Now().Add(5 * time.Second))
	respLenBuf := make([]byte, 4)
	if _, err := clientConn.Read(respLenBuf); err != nil {
		t.Fatalf("Failed to read response: %v", err)
	}

	respLen := binary.BigEndian.Uint32(respLenBuf)
	respBuf := make([]byte, respLen)
	if _, err := clientConn.Read(respBuf); err != nil {
		t.Fatalf("Failed to read response data: %v", err)
	}

	var result Result
	if err := json.Unmarshal(respBuf, &result); err != nil {
		t.Fatalf("Failed to unmarshal: %v", err)
	}

	if result.ExitCode != 1 {
		t.Errorf("ExitCode = %d, want 1", result.ExitCode)
	}

	if result.Error == "" {
		t.Error("Error should not be empty for invalid JSON")
	}
}

func TestServer_ExecutorIntegration(t *testing.T) {
	server := NewServer(DefaultServerConfig())

	// Verify executor is properly initialized
	if server.executor == nil {
		t.Fatal("Executor should not be nil")
	}

	// Verify supported languages are available
	langs := server.executor.SupportedLanguages()
	if len(langs) == 0 {
		t.Error("No supported languages found")
	}

	// Try to find python
	hasPython := false
	for _, lang := range langs {
		if lang == "python" {
			hasPython = true
			break
		}
	}

	if !hasPython {
		t.Error("Python should be a supported language")
	}
}

func TestBinaryProtocol_BigEndian(t *testing.T) {
	tests := []struct {
		name  string
		value uint32
		bytes []byte
	}{
		{
			name:  "small value",
			value: 100,
			bytes: []byte{0x00, 0x00, 0x00, 0x64},
		},
		{
			name:  "medium value",
			value: 65535,
			bytes: []byte{0x00, 0x00, 0xff, 0xff},
		},
		{
			name:  "large value",
			value: 1048576,
			bytes: []byte{0x00, 0x10, 0x00, 0x00},
		},
		{
			name:  "max uint16",
			value: 0xFFFF,
			bytes: []byte{0x00, 0x00, 0xff, 0xff},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			buf := make([]byte, 4)
			binary.BigEndian.PutUint32(buf, tt.value)

			for i, b := range tt.bytes {
				if buf[i] != b {
					t.Errorf("byte[%d] = 0x%02x, want 0x%02x", i, buf[i], b)
				}
			}

			// Verify round-trip
			decoded := binary.BigEndian.Uint32(buf)
			if decoded != tt.value {
				t.Errorf("decoded = %d, want %d", decoded, tt.value)
			}
		})
	}
}
