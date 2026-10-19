// Package protocol define a mensagem trocada entre host e guest pelo vsock (porta 5000):
// um prefixo de 4 bytes big-endian com o tamanho, seguido do JSON. Host e guest importam
// este pacote para não manterem duas cópias do formato.
package protocol

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"io"
)

// DefaultPort é a porta vsock em que o guest-runner escuta dentro da VM.
const DefaultPort = 5000

// MaxFrameBytes limita o tamanho declarado no prefixo. Sem teto, um prefixo hostil
// (até 4 GiB) faria o leitor alocar tudo antes de ler um byte do corpo. 64 MiB cobre
// stdout grande com folga; acima disso a execução já seria recusada pelo host.
const MaxFrameBytes = 64 << 20

// File é um arquivo-fonte de um job com vários arquivos.
type File struct {
	Name    string `json:"name"`
	Content string `json:"content"`
}

// Job é o pedido de execução enviado ao guest.
type Job struct {
	TraceID string            `json:"trace_id"`
	Lang    string            `json:"lang"`
	Code    string            `json:"code"`
	Files   []File            `json:"files,omitempty"`
	Stdin   interface{}       `json:"stdin,omitempty"` // string, ou []string para lote
	Timeout int               `json:"timeout"`
	Meta    map[string]string `json:"meta,omitempty"`
	// Stream pede a saída em pedaços (frames Event) antes do resultado. Sem ele o guest
	// responde com um único frame Result, como sempre fez.
	Stream bool `json:"stream,omitempty"`
}

// Tipos de Event no modo streaming: pedaços de saída e o resultado que encerra a conversa.
const (
	EventStdout = "stdout"
	EventStderr = "stderr"
	EventResult = "result"
)

// Event é um frame do modo streaming. Pedaço de saída leva Data; o último frame tem
// Type EventResult e o Result completo (stdout/stderr inteiros, para o contrato não mudar).
type Event struct {
	Type   string  `json:"type"`
	Data   string  `json:"data,omitempty"`
	Result *Result `json:"result,omitempty"`
}

// Result é a resposta do guest. Stderr e Exception são null quando vazios, contrato da API.
type Result struct {
	TraceID       string      `json:"trace_id,omitempty"`
	Status        string      `json:"status"`
	Stdout        string      `json:"stdout"`
	Stderr        interface{} `json:"stderr"`
	Exception     interface{} `json:"exception"`
	ExecutionTime int64       `json:"executionTime"`
	ExitCode      int         `json:"exit_code,omitempty"`
	Error         string      `json:"error,omitempty"`
	Stdin         string      `json:"stdin,omitempty"`
}

// WriteFrame serializa v em JSON e escreve prefixo e corpo numa única chamada,
// para o guest nunca ler um prefixo sem o corpo correspondente.
func WriteFrame(w io.Writer, v interface{}) error {
	body, err := json.Marshal(v)
	if err != nil {
		return fmt.Errorf("marshal frame: %w", err)
	}
	if len(body) > MaxFrameBytes {
		return fmt.Errorf("frame of %d bytes exceeds the limit of %d", len(body), MaxFrameBytes)
	}
	buf := make([]byte, 4+len(body))
	binary.BigEndian.PutUint32(buf, uint32(len(body)))
	copy(buf[4:], body)
	_, err = w.Write(buf)
	return err
}

// ReadFrame lê um frame e decodifica o JSON em v, recusando tamanho acima de MaxFrameBytes.
func ReadFrame(r io.Reader, v interface{}) error {
	var hdr [4]byte
	if _, err := io.ReadFull(r, hdr[:]); err != nil {
		return fmt.Errorf("read frame length: %w", err)
	}
	n := binary.BigEndian.Uint32(hdr[:])
	if n > MaxFrameBytes {
		return fmt.Errorf("frame of %d bytes exceeds the limit of %d", n, MaxFrameBytes)
	}
	body := make([]byte, n)
	if _, err := io.ReadFull(r, body); err != nil {
		return fmt.Errorf("read frame body: %w", err)
	}
	if err := json.Unmarshal(body, v); err != nil {
		return fmt.Errorf("decode frame: %w", err)
	}
	return nil
}
