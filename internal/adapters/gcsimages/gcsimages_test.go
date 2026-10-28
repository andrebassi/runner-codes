package gcsimages

import (
	"bytes"
	"context"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
)

// fakeGoogle é o metadata server e o Storage de mentira: guarda os objetos subidos (comprimidos, como no bucket) e
// conta os pedidos de token. status força a resposta de uma rota ("GET" ou "POST") para testar os erros.
type fakeGoogle struct {
	mu      sync.Mutex
	objects map[string][]byte
	// created é a hora de criação de cada objeto (a listagem devolve; Find fica com o mais novo).
	created map[string]string
	tokens  int
	status  map[string]int
	badTok  bool
}

// ServeHTTP atende o token do metadata e o download/upload da API JSON do Storage.
func (f *fakeGoogle) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if strings.HasSuffix(r.URL.Path, "/service-accounts/default/token") {
		f.tokens++
		if f.badTok || r.Header.Get("Metadata-Flavor") != "Google" {
			w.WriteHeader(http.StatusForbidden)
			return
		}
		io.WriteString(w, `{"access_token":"tok","expires_in":3600}`)
		return
	}
	if r.Header.Get("Authorization") != "Bearer tok" {
		w.WriteHeader(http.StatusUnauthorized)
		return
	}
	if code := f.status[r.Method]; code != 0 {
		w.WriteHeader(code)
		io.WriteString(w, "planted failure")
		return
	}
	if r.Method == http.MethodGet && r.URL.Path == "/storage/v1/b/bkt/o" { // listagem por prefixo (Find)
		prefix := r.URL.Query().Get("prefix")
		var items []string
		for name := range f.objects {
			if strings.HasPrefix(name, prefix) {
				items = append(items, `{"name":"`+name+`","timeCreated":"`+f.created[name]+`"}`)
			}
		}
		io.WriteString(w, `{"items":[`+strings.Join(items, ",")+`]}`)
		return
	}
	switch r.Method {
	case http.MethodGet: // /storage/v1/b/<bucket>/o/<objeto>?alt=media
		name := strings.TrimPrefix(r.URL.Path, "/storage/v1/b/bkt/o/")
		body, ok := f.objects[name]
		if !ok || r.URL.Query().Get("alt") != "media" {
			w.WriteHeader(http.StatusNotFound)
			return
		}
		w.Write(body)
	case http.MethodPost: // /upload/storage/v1/b/<bucket>/o?name=<objeto>&ifGenerationMatch=0
		name := r.URL.Query().Get("name")
		if _, exists := f.objects[name]; exists && r.URL.Query().Get("ifGenerationMatch") == "0" {
			w.WriteHeader(http.StatusPreconditionFailed)
			return
		}
		body, _ := io.ReadAll(r.Body)
		f.objects[name] = body
		io.WriteString(w, `{}`)
	}
}

// rig sobe o fake e devolve o Store apontado para ele.
func rig(t *testing.T) (*Store, *fakeGoogle) {
	t.Helper()
	f := &fakeGoogle{objects: map[string][]byte{}, status: map[string]int{}, created: map[string]string{}}
	srv := httptest.NewServer(f)
	t.Cleanup(srv.Close)
	s, err := Parse("gs://bkt/rc-images")
	if err != nil {
		t.Fatal(err)
	}
	s.HTTP, s.API, s.Metadata = srv.Client(), srv.URL, srv.URL
	return s, f
}

// TestPutGetRoundTrip: o que sobe comprimido volta idêntico, byte a byte (é o que faz o restore entre hosts valer),
// no objeto <prefixo><chave>.ext4.zst; o token é pedido uma vez só para as duas operações.
func TestPutGetRoundTrip(t *testing.T) {
	s, f := rig(t)
	dir := t.TempDir()
	src := filepath.Join(dir, "a.ext4")
	data := append(bytes.Repeat([]byte{0}, 1<<20), []byte("superbloco e diretórios")...)
	os.WriteFile(src, data, 0o644)
	ctx := context.Background()
	if err := s.Put(ctx, "desk-k1", src); err != nil {
		t.Fatal(err)
	}
	stored, ok := f.objects["rc-images/desk-k1.ext4.zst"]
	if !ok || len(stored) >= len(data)/10 {
		t.Fatalf("objeto não subiu comprimido: %d bytes de %d (%v)", len(stored), len(data), ok)
	}
	dst := filepath.Join(dir, "b.ext4")
	if err := s.Get(ctx, "desk-k1", dst); err != nil {
		t.Fatal(err)
	}
	if got, _ := os.ReadFile(dst); !bytes.Equal(got, data) {
		t.Fatal("a imagem baixada não é a que subiu")
	}
	if f.tokens != 1 {
		t.Fatalf("pedidos de token: %d", f.tokens)
	}
	// Outro host subindo a mesma chave depois: o objeto já existe (412) e isso não é erro.
	if err := s.Put(ctx, "desk-k1", src); err != nil {
		t.Fatalf("segundo upload da mesma chave: %v", err)
	}
}

// TestGetFailures: chave que ninguém subiu é ErrNotFound (quem pediu constrói); erro do Storage sobe e não deixa
// arquivo nenhum no destino; conteúdo que não é zstd também não.
func TestGetFailures(t *testing.T) {
	s, f := rig(t)
	dst := filepath.Join(t.TempDir(), "x.ext4")
	ctx := context.Background()
	if err := s.Get(ctx, "nada", dst); !errors.Is(err, ErrNotFound) {
		t.Fatalf("ausente: %v", err)
	}
	f.objects["rc-images/lixo.ext4.zst"] = []byte("isto não é zstd")
	if err := s.Get(ctx, "lixo", dst); err == nil {
		t.Fatal("conteúdo inválido sem erro")
	}
	f.status["GET"] = http.StatusInternalServerError
	if err := s.Get(ctx, "nada", dst); err == nil || errors.Is(err, ErrNotFound) {
		t.Fatalf("500: %v", err)
	}
	if _, err := os.Stat(dst); !os.IsNotExist(err) {
		t.Fatal("falha deixou arquivo no destino")
	}
	if _, err := os.Stat(dst + ".download"); !os.IsNotExist(err) {
		t.Fatal("falha deixou o arquivo temporário")
	}
}

// TestPutFailures: recusa do Storage sobe com a mensagem; arquivo de origem que não existe é erro; token que o
// metadata recusa é erro antes de qualquer chamada ao Storage.
func TestPutFailures(t *testing.T) {
	s, f := rig(t)
	src := filepath.Join(t.TempDir(), "a.ext4")
	os.WriteFile(src, []byte("x"), 0o644)
	ctx := context.Background()
	f.status["POST"] = http.StatusForbidden
	if err := s.Put(ctx, "k", src); err == nil || !strings.Contains(err.Error(), "planted failure") {
		t.Fatalf("403: %v", err)
	}
	if err := s.Put(ctx, "k", src+".nada"); err == nil {
		t.Fatal("origem inexistente sem erro")
	}
	s2, f2 := rig(t)
	f2.badTok = true
	if err := s2.Put(ctx, "k", src); err == nil || !strings.Contains(err.Error(), "token") {
		t.Fatalf("token recusado: %v", err)
	}
	if err := s2.Get(ctx, "k", src+".y"); err == nil {
		t.Fatal("get sem token sem erro")
	}
}

// TestIOFailures: destino em diretório que não existe; origem que não se lê até o fim (um diretório: abre, mas a
// leitura falha no meio do upload); URL do Storage inválida; metadata server fora do ar — tudo vira erro, nada trava.
func TestIOFailures(t *testing.T) {
	s, f := rig(t)
	ctx := context.Background()
	src := filepath.Join(t.TempDir(), "a.ext4")
	os.WriteFile(src, []byte("x"), 0o644)
	if err := s.Put(ctx, "k", src); err != nil {
		t.Fatal(err)
	}
	if err := s.Get(ctx, "k", filepath.Join(t.TempDir(), "nao", "existe.ext4")); err == nil {
		t.Fatal("destino impossível sem erro")
	}
	if err := s.Put(ctx, "dir", t.TempDir()); err == nil {
		t.Fatal("origem ilegível sem erro")
	}
	// Lido com a trava do fake: o handler do upload abortado pode ainda estar gravando (o -race acusava aqui).
	f.mu.Lock()
	half := f.objects["rc-images/dir.ext4.zst"]
	f.mu.Unlock()
	if len(half) > 0 {
		t.Fatal("upload pela metade gravado como objeto completo")
	}
	bad := &Store{Bucket: s.Bucket, Prefix: s.Prefix, HTTP: s.HTTP, API: "http://[::1]:namedport", Metadata: s.Metadata}
	if err := bad.Get(ctx, "k", src+".y"); err == nil {
		t.Fatal("URL inválida sem erro")
	}
	off := &Store{Bucket: "b", HTTP: http.DefaultClient, API: "http://127.0.0.1:1", Metadata: "http://127.0.0.1:1"}
	if err := off.Get(ctx, "k", src+".z"); err == nil || !strings.Contains(err.Error(), "token") {
		t.Fatalf("metadata fora: %v", err)
	}
	offURL := &Store{Bucket: "b", HTTP: http.DefaultClient, Metadata: "http://[::1]:namedport"}
	if err := offURL.Get(ctx, "k", src+".w"); err == nil {
		t.Fatal("metadata com URL inválida sem erro")
	}
}

// TestParse: gs://bucket/prefixo vira o Store (prefixo sempre com a barra no fim); o resto é recusado.
func TestParse(t *testing.T) {
	for spec, want := range map[string]string{"gs://b/p": "p/", "gs://b/p/": "p/", "gs://b": "", "gs://b/": ""} {
		s, err := Parse(spec)
		if err != nil || s.Bucket != "b" || s.Prefix != want || s.object("k") != want+"k.ext4.zst" {
			t.Fatalf("%s: %+v %v", spec, s, err)
		}
	}
	for _, bad := range []string{"b/p", "s3://b/p", "gs:///p", ""} {
		if _, err := Parse(bad); err == nil {
			t.Fatalf("%q aceito", bad)
		}
	}
}

// TestFind: entre as chaves com o prefixo, devolve a de criação mais recente (sem o prefixo do bucket nem a extensão);
// objeto que não é imagem é ignorado; sem nenhuma é ErrNotFound; status ruim e JSON ilegível são erro.
func TestFind(t *testing.T) {
	s, f := rig(t)
	ctx := context.Background()
	f.objects["rc-images/img-a-base1-old.ext4.zst"] = []byte("x")
	f.created["rc-images/img-a-base1-old.ext4.zst"] = "2026-10-25T14:21:08Z"
	f.objects["rc-images/img-a-base1-new.ext4.zst"] = []byte("x")
	f.created["rc-images/img-a-base1-new.ext4.zst"] = "2026-10-28T04:39:00Z"
	f.objects["rc-images/img-a-base1-lixo.txt"] = []byte("x")
	f.created["rc-images/img-a-base1-lixo.txt"] = "2026-10-29T00:00:00Z"
	f.objects["rc-images/img-a-base2-x.ext4.zst"] = []byte("x")
	f.created["rc-images/img-a-base2-x.ext4.zst"] = "2026-10-30T00:00:00Z"
	if k, err := s.Find(ctx, "img-a-base1-"); err != nil || k != "img-a-base1-new" {
		t.Fatalf("mais nova: %q %v", k, err)
	}
	if _, err := s.Find(ctx, "img-b-"); !errors.Is(err, ErrNotFound) {
		t.Fatalf("sem nenhuma: %v", err)
	}
	f.status[http.MethodGet] = http.StatusForbidden
	if _, err := s.Find(ctx, "img-a-"); err == nil || !strings.Contains(err.Error(), "403") {
		t.Fatalf("status ruim: %v", err)
	}
	delete(f.status, http.MethodGet)
	f.badTok = true
	s.token = ""
	if _, err := s.Find(ctx, "img-a-"); err == nil {
		t.Fatal("sem token passou")
	}
}

// TestFindBadJSON: listagem ilegível é erro, nunca "achei vazio".
func TestFindBadJSON(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if strings.HasSuffix(r.URL.Path, "/token") {
			io.WriteString(w, `{"access_token":"tok","expires_in":3600}`)
			return
		}
		io.WriteString(w, `{nada`)
	}))
	t.Cleanup(srv.Close)
	s, _ := Parse("gs://bkt/rc-images")
	s.HTTP, s.API, s.Metadata = srv.Client(), srv.URL, srv.URL
	if _, err := s.Find(context.Background(), "x"); err == nil || errors.Is(err, ErrNotFound) {
		t.Fatalf("JSON ilegível: %v", err)
	}
}
