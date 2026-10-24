package guest

import (
	"net"
	"strings"
	"sync"
	"testing"
	"time"

	"runner.codes/internal/protocol"
)

// chunk é um pedaço recebido pelo sink, com a hora de chegada.
type chunk struct {
	stream, data string
	at           time.Time
}

// recorder é um Sink que guarda os pedaços; o mutex existe porque o exec chama o sink de
// duas goroutines (stdout e stderr), e o -race reprovaria sem ele.
type recorder struct {
	mu     sync.Mutex
	chunks []chunk
}

// sink devolve a função que o executor chama a cada pedaço.
func (r *recorder) sink() Sink {
	return func(stream, data string) {
		r.mu.Lock()
		defer r.mu.Unlock()
		r.chunks = append(r.chunks, chunk{stream, data, time.Now()})
	}
}

// joined junta os pedaços de um fluxo, na ordem de chegada.
func (r *recorder) joined(stream string) string {
	r.mu.Lock()
	defer r.mu.Unlock()
	var b strings.Builder
	for _, c := range r.chunks {
		if c.stream == stream {
			b.WriteString(c.data)
		}
	}
	return b.String()
}

// firstAt devolve a hora do primeiro pedaço do fluxo que contém o texto.
func (r *recorder) firstAt(stream, text string) (time.Time, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	for _, c := range r.chunks {
		if c.stream == stream && strings.Contains(c.data, text) {
			return c.at, true
		}
	}
	return time.Time{}, false
}

// TestExecuteStreamDeliversWhileRunning é o comportamento que justifica o streaming: o
// pedaço escrito antes do sleep chega ANTES de o programa terminar, não junto com o fim.
// O Result continua completo, como no Execute.
func TestExecuteStreamDeliversWhileRunning(t *testing.T) {
	var rec recorder
	res := NewExecutor().ExecuteStream(Job{Lang: "sh", Code: "echo early; echo warn >&2; sleep 1; echo late", Timeout: 10}, rec.sink())
	if res.Stdout != "early\nlate\n" || res.Stderr != "warn\n" || res.ExitCode != 0 {
		t.Fatalf("result must stay complete: %+v", res)
	}
	if got := rec.joined(protocol.EventStdout); got != "early\nlate\n" {
		t.Fatalf("streamed stdout %q", got)
	}
	if got := rec.joined(protocol.EventStderr); got != "warn\n" {
		t.Fatalf("streamed stderr %q", got)
	}
	early, ok1 := rec.firstAt(protocol.EventStdout, "early")
	late, ok2 := rec.firstAt(protocol.EventStdout, "late")
	if !ok1 || !ok2 {
		t.Fatalf("missing chunks: %+v", rec.chunks)
	}
	// 1 s de sleep entre os dois; 800 ms de folga para CI lento ainda prova a entrega antecipada.
	if gap := late.Sub(early); gap < 800*time.Millisecond {
		t.Fatalf("early chunk arrived only %v before the late one: output was not streamed", gap)
	}
}

// TestStreamSetsPythonUnbuffered: só o modo streaming liga PYTHONUNBUFFERED (sem ele o
// Python acumula 8 KiB no pipe e nada chega antes do fim); o modo normal não muda.
func TestStreamSetsPythonUnbuffered(t *testing.T) {
	e := NewExecutor()
	job := Job{Lang: "sh", Code: `echo "[$PYTHONUNBUFFERED]"`, Timeout: 10}
	var rec recorder
	if got := e.ExecuteStream(job, rec.sink()).Stdout; got != "[1]\n" {
		t.Fatalf("stream mode: %q", got)
	}
	if got := e.Execute(job).Stdout; got != "[]\n" {
		t.Fatalf("normal mode must not change: %q", got)
	}
}

// TestHandleConnectionStream exercita o servidor pelo fio: pedaços antes do resultado,
// resultado completo no fim; e o job sem stream continua recebendo um frame Result só.
func TestHandleConnectionStream(t *testing.T) {
	s := NewServer(DefaultServerConfig())

	client, server := net.Pipe()
	go s.handleConnection(server)
	client.SetDeadline(time.Now().Add(10 * time.Second))
	if err := protocol.WriteFrame(client, Job{Lang: "sh", Code: "echo one; echo two", Timeout: 10, Stream: true}); err != nil {
		t.Fatal(err)
	}
	var out strings.Builder
	for {
		var ev protocol.Event
		if err := protocol.ReadFrame(client, &ev); err != nil {
			t.Fatalf("read event: %v", err)
		}
		if ev.Type == protocol.EventResult {
			if ev.Result == nil || ev.Result.Stdout != "one\ntwo\n" {
				t.Fatalf("final result: %+v", ev.Result)
			}
			break
		}
		if ev.Type != protocol.EventStdout {
			t.Fatalf("unexpected event %+v", ev)
		}
		out.WriteString(ev.Data)
	}
	client.Close()
	if out.String() != "one\ntwo\n" {
		t.Fatalf("streamed %q", out.String())
	}

	client, server = net.Pipe()
	defer client.Close()
	go s.handleConnection(server)
	client.SetDeadline(time.Now().Add(10 * time.Second))
	if err := protocol.WriteFrame(client, Job{Lang: "sh", Code: "echo plain", Timeout: 10}); err != nil {
		t.Fatal(err)
	}
	var res protocol.Result
	if err := protocol.ReadFrame(client, &res); err != nil || res.Stdout != "plain\n" || res.Status != "success" {
		t.Fatalf("non-streaming reply changed: %+v %v", res, err)
	}
}

// TestStreamJobHostGone: o host fechou a conexão no meio. O servidor não trava nem entra
// em pânico; descarta o resto e devolve quando o programa termina.
func TestStreamJobHostGone(t *testing.T) {
	client, server := net.Pipe()
	client.Close()
	done := make(chan struct{})
	go func() {
		s := NewServer(DefaultServerConfig())
		job := Job{Lang: "sh", Code: "echo a; echo b; echo c", Timeout: 10, Stream: true}
		s.streamJob(server, job, func(sink Sink) Result { return s.executor.ExecuteStream(job, sink) })
		close(done)
	}()
	select {
	case <-done:
	case <-time.After(10 * time.Second):
		t.Fatal("streamJob hung after the host went away")
	}
}
