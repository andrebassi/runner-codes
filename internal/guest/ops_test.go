package guest

import (
	"bytes"
	"crypto/rand"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"runner.code/internal/protocol"
)

// TestExecArgv cobre o exec das máquinas: argv sem shell, env e cwd do pedido, stdin,
// código de saída, programa inexistente (127) e prazo (124 + Timeout).
func TestExecArgv(t *testing.T) {
	dir := t.TempDir()
	res := execArgv(Job{Argv: []string{"sh", "-c", `echo "$FOO $(pwd)"; read l; echo "in:$l"; echo err >&2; exit 3`},
		Env: []string{"FOO=bar"}, Cwd: dir, Stdin: "hello\n"}, nil)
	real, _ := filepath.EvalSymlinks(dir)
	if res.ExitCode != 3 || !strings.Contains(res.Stdout, "bar ") || !strings.Contains(res.Stdout, "in:hello") || res.Stderr != "err\n" {
		t.Fatalf("exec: %+v", res)
	}
	if !strings.Contains(res.Stdout, real) && !strings.Contains(res.Stdout, dir) {
		t.Fatalf("cwd not applied: %q", res.Stdout)
	}
	// Argv é literal: "echo a;b" é um argumento só, não dois comandos.
	if res := execArgv(Job{Argv: []string{"echo", "a;b"}}, nil); res.Stdout != "a;b\n" {
		t.Fatalf("argv must not go through a shell: %q", res.Stdout)
	}
	if res := execArgv(Job{Argv: []string{"/does/not/exist"}}, nil); res.ExitCode != 127 || res.Stderr == nil {
		t.Fatalf("missing program: %+v", res)
	}
	if res := execArgv(Job{}, nil); res.Status != "failed" {
		t.Fatalf("empty argv must fail: %+v", res)
	}
	start := time.Now()
	if res := execArgv(Job{Argv: []string{"sleep", "5"}, Timeout: 1}, nil); res.ExitCode != 124 || res.Exception != "Timeout" || time.Since(start) > 3*time.Second {
		t.Fatalf("timeout: %+v after %v", res, time.Since(start))
	}
	var rec recorder
	if res := execArgv(Job{Argv: []string{"sh", "-c", "echo s1; echo e1 >&2"}}, rec.sink()); res.Stdout != "s1\n" ||
		rec.joined(protocol.EventStdout) != "s1\n" || rec.joined(protocol.EventStderr) != "e1\n" {
		t.Fatalf("streamed exec: %+v %+v", res, rec.chunks)
	}
}

// TestPutGetChunks copia um arquivo de 2,5 MiB em pedaços e confere: o destino só aparece
// no último pedaço (antes disso não existe), o conteúdo volta igual pelo get, e o parcial
// some.
func TestPutGetChunks(t *testing.T) {
	data := make([]byte, 2*protocol.ChunkBytes+protocol.ChunkBytes/2)
	rand.Read(data)
	dst := filepath.Join(t.TempDir(), "sub", "f.bin")
	for off := 0; off < len(data); off += protocol.ChunkBytes {
		end := min(off+protocol.ChunkBytes, len(data))
		final := end == len(data)
		if res := putChunk(Job{Path: dst, Offset: int64(off), Data: data[off:end], Final: final, Mode: 0o600}); res.Status != "success" {
			t.Fatalf("put at %d: %+v", off, res)
		}
		if _, err := os.Stat(dst); !final && err == nil {
			t.Fatal("the destination appeared before the last chunk")
		}
	}
	if _, err := os.Stat(dst + partialSuffix); err == nil {
		t.Fatal("partial file left behind")
	}
	if st, _ := os.Stat(dst); st.Mode().Perm() != 0o600 {
		t.Fatalf("mode %v", st.Mode())
	}
	var got []byte
	for off := int64(0); ; {
		res := getChunk(Job{Path: dst, Offset: off})
		if res.Status != "success" || res.Size != int64(len(data)) {
			t.Fatalf("get at %d: %+v", off, res.Error)
		}
		got = append(got, res.Data...)
		off += int64(len(res.Data))
		if off >= res.Size {
			break
		}
	}
	if !bytes.Equal(got, data) {
		t.Fatal("content differs after the round trip")
	}
}

// TestPutGetErrors: caminho relativo, arquivo que não existe, diretório e teto de 4 GiB.
func TestPutGetErrors(t *testing.T) {
	if res := putChunk(Job{Path: "rel/x", Data: []byte("a"), Final: true}); res.Status != "failed" {
		t.Fatal("relative path must be refused")
	}
	if res := putChunk(Job{Path: "/x", Offset: protocol.MaxCopyBytes, Data: []byte("a")}); !strings.Contains(res.Error, "exceeds maximum") {
		t.Fatalf("cap: %+v", res)
	}
	if res := putChunk(Job{Path: "/proc/nope/x", Data: []byte("a")}); res.Status != "failed" {
		t.Fatal("unwritable path must fail")
	}
	if res := getChunk(Job{Path: filepath.Join(t.TempDir(), "none")}); res.Status != "failed" {
		t.Fatal("missing file must fail")
	}
	if res := getChunk(Job{Path: t.TempDir()}); res.Status != "failed" {
		t.Fatal("a directory is not a file to copy")
	}
}

// TestServerOps exercita as operações pelo fio: exec, exec com stream, put/get, op
// desconhecida e shutdown (sync antes da resposta, e só então a saída do processo).
func TestServerOps(t *testing.T) {
	s := NewServer(DefaultServerConfig())
	call := func(job Job) protocol.Result {
		t.Helper()
		c, srv := net.Pipe()
		defer c.Close()
		go s.handleConnection(srv)
		c.SetDeadline(time.Now().Add(10 * time.Second))
		if err := protocol.WriteFrame(c, job); err != nil {
			t.Fatal(err)
		}
		var res protocol.Result
		if err := protocol.ReadFrame(c, &res); err != nil {
			t.Fatal(err)
		}
		return res
	}
	if res := call(Job{Op: protocol.OpExec, Argv: []string{"echo", "hi"}}); res.Stdout != "hi\n" {
		t.Fatalf("exec: %+v", res)
	}
	dst := filepath.Join(t.TempDir(), "a.txt")
	if res := call(Job{Op: protocol.OpPut, Path: dst, Data: []byte("abc"), Final: true}); res.Status != "success" {
		t.Fatalf("put: %+v", res)
	}
	if res := call(Job{Op: protocol.OpGet, Path: dst}); string(res.Data) != "abc" || res.Size != 3 {
		t.Fatalf("get: %+v", res)
	}
	if res := call(Job{Op: "format-disk"}); res.Status != "failed" || !strings.Contains(res.Error, "unknown op") {
		t.Fatalf("unknown op: %+v", res)
	}

	synced, exited := false, make(chan int, 1)
	syncDisks, exitProcess = func() { synced = true }, func(code int) { exited <- code }
	defer func() { syncDisks, exitProcess = syncDisksDefault, os.Exit }()
	if res := call(Job{Op: protocol.OpShutdown}); res.Status != "success" || !synced {
		t.Fatalf("shutdown: %+v synced=%v", res, synced)
	}
	if code := <-exited; code != 0 {
		t.Fatalf("exit code %d", code)
	}

	c, srv := net.Pipe()
	defer c.Close()
	go s.handleConnection(srv)
	c.SetDeadline(time.Now().Add(10 * time.Second))
	protocol.WriteFrame(c, Job{Op: protocol.OpExec, Argv: []string{"sh", "-c", "echo x"}, Stream: true})
	var ev protocol.Event
	if err := protocol.ReadFrame(c, &ev); err != nil || ev.Type != protocol.EventStdout || ev.Data != "x\n" {
		t.Fatalf("streamed exec event: %+v %v", ev, err)
	}
}
