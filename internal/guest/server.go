package guest

import (
	"encoding/binary"
	"encoding/json"
	"io"
	"log"
	"net"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"

	"github.com/mdlayher/vsock"

	"runner.codes/internal/protocol"
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
	// wl é o workload da máquina persistente (no máximo um vivo).
	wl workload
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
	// Só preenche o que veio vazio: sob libkrun o root é read-only e o start.sh aponta
	// HOME (e caches como GOCACHE) para o tmpfs; sobrescrever com /root quebraria `go run`.
	setDefaultEnv("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")
	setDefaultEnv("HOME", "/root")
	setDefaultEnv("LANG", "C.UTF-8")

	log.Printf("Runner Codes Guest Runner starting...")
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

// ServeConn atende uma conexão já aberta como se tivesse chegado pelo vsock: é como o shim do
// Kubernetes testa o OpProc ponta a ponta sem VM (net.Pipe até o guest de verdade).
func (s *Server) ServeConn(conn net.Conn) { s.handleConnection(conn) }

// handleConnection lê um job (4 bytes de tamanho + JSON) e despacha pela operação; a conexão fecha
// quando a operação termina.
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

	log.Printf("Received job: trace_id=%s lang=%s code_len=%d timeout=%d stream=%v",
		job.TraceID, job.Lang, len(job.Code), job.Timeout, job.Stream)

	switch job.Op {
	case protocol.OpRun:
	case protocol.OpExec:
		if job.Stream {
			s.streamJob(conn, job, func(sink Sink) Result { return execArgv(job, sink) })
			return
		}
		writeResult(conn, execArgv(job, nil))
		return
	case protocol.OpPut:
		writeResult(conn, putChunk(job))
		return
	case protocol.OpGet:
		writeResult(conn, getChunk(job))
		return
	case protocol.OpPTY:
		ptySession(conn, job)
		return
	case protocol.OpProc:
		procSession(conn, job)
		return
	case protocol.OpWorkload:
		writeResult(conn, s.startWorkload(job))
		return
	case protocol.OpDial:
		dialSession(conn, job)
		return
	case protocol.OpMountS3:
		writeResult(conn, mountS3(job))
		return
	case protocol.OpSSHAgent:
		writeResult(conn, startSSHAgent(job))
		return
	case protocol.OpSetEnv:
		setSecretEnv(job.Env)
		writeResult(conn, Result{TraceID: job.TraceID, Status: "success"})
		return
	case protocol.OpShutdown:
		// Primeiro o workload: stop_signal e até Timeout segundos para ele salvar o que
		// precisa (um servidor gravando estado). Depois o disco, antes de responder: quando
		// o host recebe o ok, o que estava em cache já está no ext4. A saída desliga a VM.
		s.stopWorkload(job.Signal, time.Duration(job.Timeout)*time.Second)
		// Volumes s3:// saem antes do sync: o que um programa deixou aberto sobe no release.
		unmountS3()
		syncDisks()
		writeResult(conn, Result{TraceID: job.TraceID, Status: "success"})
		exitProcess(0)
		return
	default:
		s.sendError(conn, job.TraceID, "unknown op: "+job.Op)
		return
	}

	if job.Stream {
		s.streamJob(conn, job, func(sink Sink) Result { return s.executor.ExecuteStream(job, sink) })
		return
	}

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

// streamJob roda o job mandando cada pedaço de saída como frame Event e fecha com o
// Event de resultado. O mutex existe porque o exec copia stdout e stderr em goroutines
// separadas: dois frames escritos ao mesmo tempo se intercalariam no socket.
// Depois de uma escrita falhar (host fechou a conexão), os pedaços seguintes são
// descartados; o programa segue até o fim ou o timeout, e o host já matou a VM.
func (s *Server) streamJob(conn net.Conn, job Job, run func(Sink) Result) {
	var mu sync.Mutex
	broken := false
	send := func(ev protocol.Event) {
		mu.Lock()
		defer mu.Unlock()
		if broken {
			return
		}
		if err := protocol.WriteFrame(conn, ev); err != nil {
			log.Printf("Failed to write stream frame: %v", err)
			broken = true
		}
	}
	result := run(func(stream, data string) {
		send(protocol.Event{Type: stream, Data: data})
	})
	send(protocol.Event{Type: protocol.EventResult, Result: &result})
}

// writeResult manda o Result num frame; erro de escrita só vai para o log, porque o host
// que sumiu não tem a quem avisar.
func writeResult(conn net.Conn, res Result) {
	if err := protocol.WriteFrame(conn, res); err != nil {
		log.Printf("Failed to write result: %v", err)
	}
}

// exitProcess encerra o guest; trocado no teste para o processo de teste não sair.
var exitProcess = osExit

// osExit é o os.Exit de verdade, guardado para o teste restaurar depois de trocar.
var osExit = os.Exit

// sendError responde com um Result de falha quando o pedido nem chega a virar job.
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

// setDefaultEnv define a variável só quando ela não veio do ambiente.
func setDefaultEnv(key, value string) {
	if os.Getenv(key) == "" {
		os.Setenv(key, value)
	}
}
