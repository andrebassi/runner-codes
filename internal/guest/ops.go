package guest

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"runner.code/internal/protocol"
)

// partialSuffix marca o arquivo em cópia: ele só vira o destino no último pedaço, então
// quem lê o destino nunca vê um arquivo pela metade.
const partialSuffix = ".rc-partial"

// execArgv roda o argv do job como está, sem shell implícito (quem quer shell manda
// `sh -c ...`), com o env da máquina mais o do pedido. Timeout 0 é sem prazo: numa
// máquina o comando pode ser um build longo, e quem chama controla o prazo pelo ctx.
// Com sink, a saída também vai saindo enquanto o programa roda.
func execArgv(job Job, sink Sink) Result {
	res := Result{TraceID: job.TraceID, Status: "success"}
	if len(job.Argv) == 0 || job.Argv[0] == "" {
		return failed(res, "exec: empty argv")
	}
	ctx := context.Background()
	if job.Timeout > 0 {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, time.Duration(job.Timeout)*time.Second)
		defer cancel()
	}
	cmd := exec.CommandContext(ctx, job.Argv[0], job.Argv[1:]...)
	cmd.Env = append(os.Environ(), job.Env...)
	cmd.Dir = job.Cwd
	if cmd.Dir == "" {
		cmd.Dir = "/"
	}
	var stdout, stderr bytes.Buffer
	cmd.Stdout, cmd.Stderr = io.Writer(&stdout), io.Writer(&stderr)
	if sink != nil {
		cmd.Stdout = io.MultiWriter(&stdout, sinkWriter{protocol.EventStdout, sink})
		cmd.Stderr = io.MultiWriter(&stderr, sinkWriter{protocol.EventStderr, sink})
	}
	if s, ok := job.Stdin.(string); ok && s != "" {
		cmd.Stdin = strings.NewReader(s)
	}
	start := time.Now()
	err := cmd.Run()
	res.ExecutionTime = time.Since(start).Milliseconds()
	res.Stdout = stdout.String()
	var ee *exec.ExitError
	switch {
	case err == nil:
	case ctx.Err() == context.DeadlineExceeded:
		res.ExitCode, res.Exception = 124, "Timeout"
	case errors.As(err, &ee):
		res.ExitCode = ee.ExitCode()
	default:
		// Programa que nem começou (não existe, sem permissão): 127, como o shell faz.
		res.ExitCode = 127
		stderr.WriteString(err.Error())
	}
	if stderr.Len() > 0 {
		res.Stderr = stderr.String()
	}
	return res
}

// putChunk grava um pedaço no arquivo parcial e, no último, troca pelo destino com rename
// (atômico no mesmo sistema de arquivos). Offset 0 recomeça a cópia do zero.
func putChunk(job Job) Result {
	res := Result{TraceID: job.TraceID, Status: "success"}
	if !filepath.IsAbs(job.Path) {
		return failed(res, "put: path must be absolute: "+job.Path)
	}
	if job.Offset+int64(len(job.Data)) > protocol.MaxCopyBytes {
		return failed(res, fmt.Sprintf("put: total_size exceeds maximum of %d bytes", int64(protocol.MaxCopyBytes)))
	}
	partial := job.Path + partialSuffix
	flags := os.O_WRONLY | os.O_CREATE
	if job.Offset == 0 {
		flags |= os.O_TRUNC
		if err := os.MkdirAll(filepath.Dir(job.Path), 0o755); err != nil {
			return failed(res, "put: "+err.Error())
		}
	}
	mode := os.FileMode(job.Mode)
	if mode == 0 {
		mode = 0o644
	}
	f, err := os.OpenFile(partial, flags, mode)
	if err != nil {
		return failed(res, "put: "+err.Error())
	}
	_, werr := f.WriteAt(job.Data, job.Offset)
	cerr := f.Close()
	if werr != nil || cerr != nil {
		os.Remove(partial)
		return failed(res, fmt.Sprintf("put: %v", errors.Join(werr, cerr)))
	}
	if job.Final {
		if err := os.Rename(partial, job.Path); err != nil {
			os.Remove(partial)
			return failed(res, "put: "+err.Error())
		}
	}
	return res
}

// getChunk lê até ChunkBytes a partir do Offset e devolve junto o tamanho total, para o
// host saber quando parar e conferir o teto antes de gravar do lado dele.
func getChunk(job Job) Result {
	res := Result{TraceID: job.TraceID, Status: "success"}
	f, err := os.Open(job.Path)
	if err != nil {
		return failed(res, "get: "+err.Error())
	}
	defer f.Close()
	st, err := f.Stat()
	if err != nil || st.IsDir() {
		return failed(res, "get: not a regular file: "+job.Path)
	}
	res.Size = st.Size()
	if res.Size > protocol.MaxCopyBytes {
		return failed(res, fmt.Sprintf("get: file exceeding the byte cap of %d", int64(protocol.MaxCopyBytes)))
	}
	buf := make([]byte, protocol.ChunkBytes)
	n, err := f.ReadAt(buf, job.Offset)
	if err != nil && err != io.EOF {
		return failed(res, "get: "+err.Error())
	}
	res.Data = buf[:n]
	return res
}

// syncDisks grava no disco tudo que está em cache. É o que torna seguro desligar a
// máquina: o ext4 do disco dela fica consistente sem depender do journal.
var syncDisks = syncDisksDefault

// syncDisksDefault é o sync de verdade, guardado para o teste restaurar depois de trocar.
// Embrulhado porque syscall.Sync devolve erro no macOS e nada no Linux.
var syncDisksDefault = func() { syscall.Sync() }

// failed marca o resultado como falha de operação com a mensagem em Error.
func failed(res Result, msg string) Result {
	res.Status, res.ExitCode, res.Error = "failed", 1, msg
	return res
}
