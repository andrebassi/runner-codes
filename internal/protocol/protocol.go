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
		return fmt.Errorf("serializar frame: %w", err)
	}
	if len(body) > MaxFrameBytes {
		return fmt.Errorf("frame de %d bytes passa do limite de %d", len(body), MaxFrameBytes)
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
		return fmt.Errorf("ler tamanho do frame: %w", err)
	}
	n := binary.BigEndian.Uint32(hdr[:])
	if n > MaxFrameBytes {
		return fmt.Errorf("frame de %d bytes passa do limite de %d", n, MaxFrameBytes)
	}
	body := make([]byte, n)
	if _, err := io.ReadFull(r, body); err != nil {
		return fmt.Errorf("ler corpo do frame: %w", err)
	}
	if err := json.Unmarshal(body, v); err != nil {
		return fmt.Errorf("decodificar frame: %w", err)
	}
	return nil
}
