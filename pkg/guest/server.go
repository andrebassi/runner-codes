package guest

import (
	"encoding/binary"
	"encoding/json"
	"io"
	"log"
	"net"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/mdlayher/vsock"
)

const (
	DefaultPort = 5000
)

// ServerConfig holds configuration for the guest server
type ServerConfig struct {
	Port uint32
}

// DefaultServerConfig returns a ServerConfig with default values
func DefaultServerConfig() ServerConfig {
	return ServerConfig{
		Port: DefaultPort,
	}
}

// Server represents the guest runner server
type Server struct {
	config   ServerConfig
	executor *Executor
}

// NewServer creates a new guest server
func NewServer(config ServerConfig) *Server {
	return &Server{
		config:   config,
		executor: NewExecutor(),
	}
}

// Run starts the guest server and listens for connections
func (s *Server) Run() error {
	// Set up PATH for command execution (required when running as init)
	os.Setenv("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")
	os.Setenv("HOME", "/root")
	os.Setenv("LANG", "C.UTF-8")

	log.Printf("LLM-Firecracker Guest Runner starting...")
	log.Printf("  Vsock Port: %d", s.config.Port)
	log.Printf("  PID: %d", os.Getpid())
	log.Printf("  Supported languages: %v", s.executor.SupportedLanguages())

	// Setup signal handling
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		<-sigChan
		log.Println("Received shutdown signal, exiting...")
		os.Exit(0)
	}()

	// Start vsock listener
	listener, err := vsock.Listen(s.config.Port, nil)
	if err != nil {
		return err
	}
	defer listener.Close()

	log.Printf("Listening on vsock port %d", s.config.Port)

	// Accept connections
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Printf("Accept error: %v", err)
			time.Sleep(100 * time.Millisecond)
			continue
		}

		go s.handleConnection(conn.(*vsock.Conn))
	}
}

func (s *Server) handleConnection(conn net.Conn) {
	defer conn.Close()

	// Set read deadline
	conn.SetReadDeadline(time.Now().Add(5 * time.Minute))

	// Read message length (4 bytes, big endian)
	lenBuf := make([]byte, 4)
	if _, err := io.ReadFull(conn, lenBuf); err != nil {
		log.Printf("Failed to read message length: %v", err)
		s.sendError(conn, "", "failed to read message length: "+err.Error())
		return
	}
	msgLen := binary.BigEndian.Uint32(lenBuf)

	// Read message data
	msgData := make([]byte, msgLen)
	if _, err := io.ReadFull(conn, msgData); err != nil {
		log.Printf("Failed to read message data: %v", err)
		s.sendError(conn, "", "failed to read message data: "+err.Error())
		return
	}

	// Decode job
	var job Job
	if err := json.Unmarshal(msgData, &job); err != nil {
		log.Printf("Failed to decode job: %v", err)
		s.sendError(conn, "", "invalid job format: "+err.Error())
		return
	}

	log.Printf("Received job: trace_id=%s lang=%s code_len=%d timeout=%d",
		job.TraceID, job.Lang, len(job.Code), job.Timeout)

	// Execute
	startTime := time.Now()
	result := s.executor.Execute(job)
	duration := time.Since(startTime)

	log.Printf("Job completed: trace_id=%s exit_code=%d duration=%v",
		job.TraceID, result.ExitCode, duration)

	// Send result with length prefix
	respData, err := json.Marshal(&result)
	if err != nil {
		log.Printf("Failed to encode result: %v", err)
		return
	}

	// Write length prefix
	binary.BigEndian.PutUint32(lenBuf, uint32(len(respData)))
	if _, err := conn.Write(lenBuf); err != nil {
		log.Printf("Failed to write response length: %v", err)
		return
	}

	// Write response data
	if _, err := conn.Write(respData); err != nil {
		log.Printf("Failed to write response data: %v", err)
	}
}

func (s *Server) sendError(conn net.Conn, traceID, message string) {
	result := Result{
		TraceID:   traceID,
		Status:    "failed",
		Stdout:    "",
		Stderr:    message,
		Exception: message,
		ExitCode:  1,
		Error:     message,
	}

	respData, err := json.Marshal(&result)
	if err != nil {
		return
	}

	// Write length prefix
	lenBuf := make([]byte, 4)
	binary.BigEndian.PutUint32(lenBuf, uint32(len(respData)))
	conn.Write(lenBuf)
	conn.Write(respData)
}
