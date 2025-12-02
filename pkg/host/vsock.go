package host

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"net"
	"time"
)

const (
	DefaultGuestCID  = 3
	DefaultVsockPort = 5000
)

// VsockClient handles communication with the guest via Firecracker's vsock UDS
type VsockClient struct {
	udsPath string
	port    uint32
}

// NewVsockClient creates a new vsock client that connects through Firecracker's UDS
func NewVsockClient(udsPath string, port uint32) *VsockClient {
	return &VsockClient{
		udsPath: udsPath,
		port:    port,
	}
}

// connectToGuest connects to the guest through Firecracker's vsock UDS
// Firecracker vsock UDS protocol: connect to UDS, send "CONNECT <port>\n"
func (vc *VsockClient) connectToGuest() (net.Conn, error) {
	// Connect to Firecracker's vsock UDS
	conn, err := net.DialTimeout("unix", vc.udsPath, 5*time.Second)
	if err != nil {
		return nil, fmt.Errorf("dial vsock UDS %s: %w", vc.udsPath, err)
	}

	// Send CONNECT command to Firecracker
	// Format: "CONNECT <port>\n"
	connectCmd := fmt.Sprintf("CONNECT %d\n", vc.port)
	if _, err := conn.Write([]byte(connectCmd)); err != nil {
		conn.Close()
		return nil, fmt.Errorf("send CONNECT command: %w", err)
	}

	// Read response: "OK <port>\n" on success
	response := make([]byte, 32)
	n, err := conn.Read(response)
	if err != nil {
		conn.Close()
		return nil, fmt.Errorf("read CONNECT response: %w", err)
	}

	respStr := string(response[:n])
	if len(respStr) < 2 || respStr[:2] != "OK" {
		conn.Close()
		return nil, fmt.Errorf("vsock CONNECT failed: %s", respStr)
	}

	return conn, nil
}

// SendJob sends a job to the guest and waits for the result
func (vc *VsockClient) SendJob(req RunRequest, timeout time.Duration) (*RunResult, error) {
	// Connect to guest via Firecracker vsock UDS
	conn, err := vc.connectToGuest()
	if err != nil {
		return nil, fmt.Errorf("connect to guest: %w", err)
	}
	defer conn.Close()

	// Set deadline
	if timeout > 0 {
		conn.SetDeadline(time.Now().Add(timeout))
	}

	// Send request with length prefix for reliable framing
	data, err := json.Marshal(&req)
	if err != nil {
		return nil, fmt.Errorf("marshal request: %w", err)
	}

	// Write length prefix (4 bytes, big endian)
	lenBuf := make([]byte, 4)
	binary.BigEndian.PutUint32(lenBuf, uint32(len(data)))
	if _, err := conn.Write(lenBuf); err != nil {
		return nil, fmt.Errorf("write length: %w", err)
	}

	// Write JSON data
	if _, err := conn.Write(data); err != nil {
		return nil, fmt.Errorf("write request: %w", err)
	}

	// Read response length
	if _, err := conn.Read(lenBuf); err != nil {
		return nil, fmt.Errorf("read response length: %w", err)
	}
	respLen := binary.BigEndian.Uint32(lenBuf)

	// Read response data
	respData := make([]byte, respLen)
	totalRead := 0
	for totalRead < int(respLen) {
		n, err := conn.Read(respData[totalRead:])
		if err != nil {
			return nil, fmt.Errorf("read response: %w", err)
		}
		totalRead += n
	}

	// Parse response
	var result RunResult
	if err := json.Unmarshal(respData, &result); err != nil {
		return nil, fmt.Errorf("unmarshal response: %w", err)
	}

	return &result, nil
}

// WaitForGuestReady waits for the guest to be ready to accept connections
func (vc *VsockClient) WaitForGuestReady(timeout time.Duration) error {
	deadline := time.Now().Add(timeout)
	var lastErr error

	for time.Now().Before(deadline) {
		conn, err := vc.connectToGuest()
		if err == nil {
			conn.Close()
			return nil
		}
		lastErr = err
		time.Sleep(500 * time.Millisecond)
	}

	return fmt.Errorf("guest not ready within %v: %w", timeout, lastErr)
}

// ExecuteCodeViaUDS executes code on the guest using Firecracker's vsock UDS
func ExecuteCodeViaUDS(udsPath string, port uint32, lang, code string, timeout int) (*RunResult, error) {
	client := NewVsockClient(udsPath, port)

	// Wait for guest to be ready
	if err := client.WaitForGuestReady(30 * time.Second); err != nil {
		return nil, fmt.Errorf("wait for guest: %w", err)
	}

	// Create request
	req := RunRequest{
		TraceID: fmt.Sprintf("tr-%d", time.Now().UnixNano()),
		Lang:    lang,
		Code:    code,
		Timeout: timeout,
	}

	// Send job
	result, err := client.SendJob(req, time.Duration(timeout+10)*time.Second)
	if err != nil {
		return nil, fmt.Errorf("send job: %w", err)
	}

	return result, nil
}
