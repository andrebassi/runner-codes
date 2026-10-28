package gcepreempt

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"sync"
	"testing"
	"time"
)

// metadataFake é o metadata server de mentira: instance/preempted começa FALSE e vira TRUE quando o teste manda; o
// long-poll (wait_for_change) segura a resposta até a mudança, como o de verdade.
type metadataFake struct {
	mu       sync.Mutex
	value    string
	etag     int
	changed  chan struct{}
	failures int // as próximas N respostas são 503
	calls    []string
}

// newMetadataFake devolve o fake com o host ainda não preemptado.
func newMetadataFake() *metadataFake {
	return &metadataFake{value: "FALSE", changed: make(chan struct{})}
}

// preempt vira o valor para TRUE e acorda o long-poll.
func (m *metadataFake) preempt() {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.value, m.etag = "TRUE", m.etag+1
	close(m.changed)
}

// ServeHTTP atende instance/preempted: sem last_etag devolve na hora; com, espera a mudança.
func (m *metadataFake) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	m.mu.Lock()
	m.calls = append(m.calls, r.URL.RawQuery)
	if r.Header.Get("Metadata-Flavor") != "Google" || r.URL.Path != "/instance/preempted" {
		m.mu.Unlock()
		w.WriteHeader(403)
		return
	}
	if m.failures > 0 {
		m.failures--
		m.mu.Unlock()
		w.WriteHeader(503)
		return
	}
	ch, etag := m.changed, m.etag
	m.mu.Unlock()
	if r.URL.Query().Get("wait_for_change") == "true" && r.URL.Query().Get("last_etag") == etagOf(etag) {
		select {
		case <-ch:
		case <-r.Context().Done():
			return
		}
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	w.Header().Set("ETag", etagOf(m.etag))
	w.Write([]byte(m.value + "\n"))
}

// etagOf é o ETag do valor na versão n.
func etagOf(n int) string { return "e" + string(rune('0'+n)) }

// TestWaitReturnsOnPreemption: a espera fica parada no long-poll (sem martelar o metadata server) e volta na hora em
// que o valor vira TRUE.
func TestWaitReturnsOnPreemption(t *testing.T) {
	m := newMetadataFake()
	srv := httptest.NewServer(m)
	defer srv.Close()
	w := &Watcher{HTTP: srv.Client(), Base: srv.URL, Retry: time.Millisecond}
	done := make(chan error, 1)
	go func() { done <- w.Wait(context.Background()) }()
	time.Sleep(50 * time.Millisecond)
	select {
	case err := <-done:
		t.Fatalf("voltou antes do aviso: %v", err)
	default:
	}
	m.mu.Lock()
	n := len(m.calls)
	m.mu.Unlock()
	if n > 3 {
		t.Fatalf("martelou o metadata server: %d chamadas em 50 ms", n)
	}
	start := time.Now()
	m.preempt()
	select {
	case err := <-done:
		if err != nil {
			t.Fatal(err)
		}
		if d := time.Since(start); d > time.Second {
			t.Fatalf("demorou %s para ver o aviso", d)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("não viu o aviso")
	}
}

// TestWaitAlreadyPreempted: host que já está em preempção quando o rc sobe vê o TRUE na primeira leitura.
func TestWaitAlreadyPreempted(t *testing.T) {
	m := newMetadataFake()
	m.preempt()
	srv := httptest.NewServer(m)
	defer srv.Close()
	if err := (&Watcher{HTTP: srv.Client(), Base: srv.URL, Retry: time.Millisecond}).Wait(context.Background()); err != nil {
		t.Fatal(err)
	}
}

// TestWaitSurvivesErrors: metadata server fora por um tempo não encerra a espera (perder o aviso custaria um failover
// lento); o aviso depois dos erros ainda é visto. E o ctx encerra a espera com o erro dele.
func TestWaitSurvivesErrors(t *testing.T) {
	m := newMetadataFake()
	m.failures = 3
	srv := httptest.NewServer(m)
	defer srv.Close()
	w := &Watcher{HTTP: srv.Client(), Base: srv.URL, Retry: time.Millisecond}
	go func() { time.Sleep(30 * time.Millisecond); m.preempt() }()
	if err := w.Wait(context.Background()); err != nil {
		t.Fatal(err)
	}
	// Transporte que falha sempre + ctx que acaba: a espera devolve o erro do ctx, não fica presa.
	dead := &Watcher{HTTP: srv.Client(), Base: "http://127.0.0.1:1", Retry: 5 * time.Millisecond}
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel()
	if err := dead.Wait(ctx); !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("ctx: %v", err)
	}
	// ctx que acaba durante o long-poll.
	m2 := newMetadataFake()
	srv2 := httptest.NewServer(m2)
	defer srv2.Close()
	ctx2, cancel2 := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel2()
	if err := (&Watcher{HTTP: srv2.Client(), Base: srv2.URL, Retry: time.Millisecond}).Wait(ctx2); !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("ctx no long-poll: %v", err)
	}
	// URL inválida: o erro de montar a requisição também não derruba a espera; só o ctx encerra.
	bad := &Watcher{HTTP: srv.Client(), Base: "http://[::1]:namedport", Retry: time.Millisecond}
	ctx3, cancel3 := context.WithTimeout(context.Background(), 20*time.Millisecond)
	defer cancel3()
	if err := bad.Wait(ctx3); err == nil {
		t.Fatal("URL inválida sem erro")
	}
}

// TestPreempted: a leitura única diz o valor de agora, sem esperar mudança; metadata fora é erro (o rc serve então
// trata o SIGTERM como restart comum).
func TestPreempted(t *testing.T) {
	m := newMetadataFake()
	srv := httptest.NewServer(m)
	defer srv.Close()
	w := &Watcher{HTTP: srv.Client(), Base: srv.URL, Retry: time.Millisecond}
	if got, err := w.Preempted(context.Background()); got || err != nil {
		t.Fatalf("antes do aviso: %v %v", got, err)
	}
	m.preempt()
	if got, err := w.Preempted(context.Background()); !got || err != nil {
		t.Fatalf("depois do aviso: %v %v", got, err)
	}
	if len(m.calls) != 2 || m.calls[0] != "" || m.calls[1] != "" {
		t.Fatalf("a leitura única esperou mudança: %q", m.calls)
	}
	m.failures = 1
	if got, err := w.Preempted(context.Background()); got || err == nil {
		t.Fatalf("metadata fora: %v %v", got, err)
	}
}

// TestNew: os padrões de produção.
func TestNew(t *testing.T) {
	w := New()
	if w.Base != "http://metadata.google.internal/computeMetadata/v1" || w.Retry != time.Second || w.HTTP == nil {
		t.Fatalf("%+v", w)
	}
}
