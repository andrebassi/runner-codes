package objectstore

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/xml"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"net/url"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"
)

// exampleKey é a chave de exemplo da documentação da AWS, partida para não casar com o
// scan de segredo (ela não é segredo: está no vetor de teste público do SigV4).
var exampleKey = "AKIA" + "IOSFODNN7EXAMPLE"

// TestSignVector: o vetor "GET Object" da documentação do SigV4 para S3 (examplebucket,
// Range bytes=0-9, 24/05/2013) dá exatamente a assinatura publicada.
func TestSignVector(t *testing.T) {
	req, _ := http.NewRequest(http.MethodGet, "https://examplebucket.s3.amazonaws.com/test.txt", nil)
	req.Header.Set("Range", "bytes=0-9")
	sign(req, emptyHash, Credentials{AccessKey: exampleKey, SecretKey: "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"},
		"us-east-1", time.Date(2013, 5, 24, 0, 0, 0, 0, time.UTC))
	want := "AWS4-HMAC-SHA256 Credential=" + exampleKey + "/20130524/us-east-1/s3/aws4_request, " +
		"SignedHeaders=host;range;x-amz-content-sha256;x-amz-date, " +
		"Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"
	if got := req.Header.Get("Authorization"); got != want {
		t.Fatalf("signature:\n got %s\nwant %s", got, want)
	}
}

// TestSignDetails: sem chave não há Authorization (bucket público); token de sessão entra
// assinado; a query é ordenada e escapada pelo SigV4 (barra e espaço viram %2F e %20).
func TestSignDetails(t *testing.T) {
	req, _ := http.NewRequest(http.MethodGet, "http://h/b?prefix=a/b c&delimiter=/", nil)
	sign(req, emptyHash, Credentials{}, "us-east-1", time.Now())
	if req.Header.Get("Authorization") != "" || req.Header.Get("X-Amz-Date") == "" {
		t.Fatal("anonymous: dated, not signed")
	}
	req.Header.Del("X-Amz-Date")
	sign(req, emptyHash, Credentials{AccessKey: "k", SecretKey: "s", SessionToken: "tok"}, "auto", time.Now())
	if a := req.Header.Get("Authorization"); !strings.Contains(a, "x-amz-security-token") || !strings.Contains(a, "/auto/s3/") {
		t.Fatalf("session token must be signed: %s", a)
	}
	if q := canonicalQuery(req); q != "delimiter=%2F&prefix=a%2Fb%20c" {
		t.Fatalf("canonical query: %s", q)
	}
	if p := encodePath(""); p != "/" {
		t.Fatalf("empty path: %s", p)
	}
	if e := uriEncode("a-b_c.d~e/f!$", false); e != "a-b_c.d~e/f%21%24" {
		t.Fatalf("encode: %s", e)
	}
}

// fakeS3 é um bucket em memória que fala o suficiente do protocolo para o Store: ListObjectsV2
// com delimitador e páginas de maxKeys, HEAD, GET com Range, PUT (recusa chunked e confere o
// hash), DELETE e CopyObject.
type fakeS3 struct {
	mu        sync.Mutex
	objs      map[string][]byte
	maxKeys   int
	copyError bool
	requests  []*http.Request
}

// ServeHTTP atende /bucket/key.
func (f *fakeS3) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.requests = append(f.requests, r)
	bucket, key, _ := strings.Cut(strings.TrimPrefix(r.URL.Path, "/"), "/")
	if bucket != "bkt" {
		w.WriteHeader(404)
		io.WriteString(w, "<Error><Code>NoSuchBucket</Code><Message>nope</Message></Error>")
		return
	}
	switch {
	case r.Method == http.MethodGet && key == "":
		f.list(w, r.URL.Query())
	case r.Method == http.MethodHead || r.Method == http.MethodGet:
		b, ok := f.objs[key]
		if !ok {
			w.WriteHeader(404)
			if r.Method == http.MethodGet {
				io.WriteString(w, "<Error><Code>NoSuchKey</Code><Message>missing</Message></Error>")
			}
			return
		}
		w.Header().Set("Last-Modified", "Wed, 21 Oct 2026 07:28:00 GMT")
		if rg := r.Header.Get("Range"); rg != "" {
			var from, to int
			if n, _ := fmt.Sscanf(rg, "bytes=%d-%d", &from, &to); n < 2 {
				to = len(b) - 1
			}
			if to >= len(b) {
				to = len(b) - 1
			}
			b = b[from : to+1]
			w.Header().Set("Content-Length", strconv.Itoa(len(b)))
			w.WriteHeader(206)
		} else {
			w.Header().Set("Content-Length", strconv.Itoa(len(b)))
		}
		if r.Method == http.MethodGet {
			w.Write(b)
		}
	case r.Method == http.MethodPut && r.Header.Get("X-Amz-Copy-Source") != "":
		src := strings.TrimPrefix(r.Header.Get("X-Amz-Copy-Source"), "/bkt/")
		src, _ = url.PathUnescape(src)
		if f.copyError {
			io.WriteString(w, "<CopyObjectResult><Error><Code>InternalError</Code><Message>mid-copy</Message></Error></CopyObjectResult>")
			return
		}
		b, ok := f.objs[src]
		if !ok {
			w.WriteHeader(404)
			io.WriteString(w, "<Error><Code>NoSuchKey</Code></Error>")
			return
		}
		f.objs[key] = append([]byte(nil), b...)
		io.WriteString(w, "<CopyObjectResult/>")
	case r.Method == http.MethodPut:
		if len(r.TransferEncoding) > 0 {
			w.WriteHeader(411)
			io.WriteString(w, "<Error><Code>MissingContentLength</Code></Error>")
			return
		}
		b, _ := io.ReadAll(r.Body)
		h := sha256.Sum256(b)
		if r.Header.Get("X-Amz-Content-Sha256") != hex.EncodeToString(h[:]) {
			w.WriteHeader(400)
			io.WriteString(w, "<Error><Code>XAmzContentSHA256Mismatch</Code></Error>")
			return
		}
		f.objs[key] = b
	case r.Method == http.MethodDelete:
		delete(f.objs, key)
		w.WriteHeader(204)
	}
}

// list responde ListObjectsV2: chaves do prefixo, agrupadas no delimitador, em páginas.
func (f *fakeS3) list(w http.ResponseWriter, q url.Values) {
	prefix, delim := q.Get("prefix"), q.Get("delimiter")
	seen := map[string]bool{}
	var items []string // "k:<chave>" ou "p:<prefixo>"
	for k := range f.objs {
		if !strings.HasPrefix(k, prefix) {
			continue
		}
		rest := strings.TrimPrefix(k, prefix)
		if i := strings.Index(rest, delim); delim != "" && i >= 0 {
			p := prefix + rest[:i+1]
			if !seen[p] {
				seen[p] = true
				items = append(items, "p:"+p)
			}
			continue
		}
		items = append(items, "k:"+k)
	}
	sort.Strings(items)
	start, _ := strconv.Atoi(q.Get("continuation-token"))
	end := len(items)
	truncated := false
	if f.maxKeys > 0 && end-start > f.maxKeys {
		end, truncated = start+f.maxKeys, true
	}
	type content struct {
		Key          string
		Size         int
		LastModified string
	}
	type cp struct{ Prefix string }
	out := struct {
		XMLName               xml.Name `xml:"ListBucketResult"`
		Contents              []content
		CommonPrefixes        []cp
		IsTruncated           bool
		NextContinuationToken string `xml:",omitempty"`
	}{IsTruncated: truncated}
	for _, it := range items[start:end] {
		if strings.HasPrefix(it, "p:") {
			out.CommonPrefixes = append(out.CommonPrefixes, cp{it[2:]})
		} else {
			out.Contents = append(out.Contents, content{it[2:], len(f.objs[it[2:]]), "2026-10-21T07:28:00.000Z"})
		}
	}
	if truncated {
		out.NextContinuationToken = strconv.Itoa(end)
	}
	xml.NewEncoder(w).Encode(out)
}

// newStore sobe o S3 falso e um Store com credenciais apontado para ele.
func newStore(t *testing.T) (*Store, *fakeS3) {
	t.Helper()
	f := &fakeS3{objs: map[string][]byte{}}
	srv := httptest.NewServer(f)
	t.Cleanup(srv.Close)
	s, err := FromEnv([]string{"AWS_ACCESS_KEY_ID=k", "AWS_SECRET_ACCESS_KEY=s", "AWS_ENDPOINT_URL=" + srv.URL + "/"}, "bkt")
	if err != nil {
		t.Fatal(err)
	}
	return s, f
}

// TestStoreOperations: gravar, listar (com paginação e subprefixos), ler inteiro e por
// faixa, HEAD, copiar, apagar; nome com espaço e acento vai e volta; arquivo vazio grava.
func TestStoreOperations(t *testing.T) {
	s, f := newStore(t)
	ctx := context.Background()
	f.maxKeys = 2
	for k, v := range map[string]string{"data/a.txt": "0123456789", "data/b c ção.txt": "x", "data/sub/d.txt": "dd", "other.txt": "o"} {
		if err := s.Put(ctx, k, strings.NewReader(v)); err != nil {
			t.Fatalf("put %s: %v", k, err)
		}
	}
	file := filepath.Join(t.TempDir(), "empty")
	os.WriteFile(file, nil, 0o600)
	if err := s.PutFile(ctx, "data/empty", file); err != nil {
		t.Fatalf("empty file: %v", err)
	}
	big := filepath.Join(t.TempDir(), "big")
	os.WriteFile(big, []byte(strings.Repeat("z", 1000)), 0o600)
	if err := s.PutFile(ctx, "data/big", big); err != nil || len(f.objs["data/big"]) != 1000 {
		t.Fatalf("file body must go with its length, not chunked: %v", err)
	}
	es, err := s.List(ctx, "data/")
	if err != nil {
		t.Fatal(err)
	}
	var got []string
	for _, e := range es {
		got = append(got, fmt.Sprintf("%s:%d:%v", e.Key, e.Size, e.IsDir))
	}
	sort.Strings(got)
	want := "data/a.txt:10:false data/b c ção.txt:1:false data/big:1000:false data/empty:0:false data/sub/:0:true"
	if strings.Join(got, " ") != want {
		t.Fatalf("list across pages:\n got %v\nwant %s", got, want)
	}
	if es[0].ModTime.IsZero() {
		t.Fatal("list must carry the modification time")
	}
	read := func(key string, off, n int64) string {
		r, err := s.Get(ctx, key, off, n)
		if err != nil {
			t.Fatalf("get %s: %v", key, err)
		}
		defer r.Close()
		b, _ := io.ReadAll(r)
		return string(b)
	}
	if read("data/a.txt", 0, 0) != "0123456789" || read("data/a.txt", 2, 3) != "234" || read("data/a.txt", 7, 0) != "789" {
		t.Fatal("ranges")
	}
	if read("data/b c ção.txt", 0, 0) != "x" {
		t.Fatal("escaped key")
	}
	if e, err := s.Head(ctx, "data/a.txt"); err != nil || e.Size != 10 || e.ModTime.IsZero() {
		t.Fatalf("head: %+v %v", e, err)
	}
	if err := s.Copy(ctx, "data/b c ção.txt", "data/moved"); err != nil || string(f.objs["data/moved"]) != "x" {
		t.Fatalf("copy: %v", err)
	}
	if err := s.Delete(ctx, "data/moved"); err != nil || f.objs["data/moved"] != nil {
		t.Fatalf("delete: %v", err)
	}
	if auth := f.requests[0].Header.Get("Authorization"); !strings.HasPrefix(auth, "AWS4-HMAC-SHA256 Credential=k/") {
		t.Fatalf("requests must be signed: %q", auth)
	}
}

// TestStoreErrors: objeto ausente casa com ErrNotFound (HEAD sem corpo e GET com XML);
// bucket errado traz o código do S3; erro dentro de um 200 de cópia é erro; falha de rede,
// de leitura do corpo e XML quebrado chegam como erro.
func TestStoreErrors(t *testing.T) {
	s, f := newStore(t)
	ctx := context.Background()
	if _, err := s.Head(ctx, "nope"); !errors.Is(err, ErrNotFound) {
		t.Fatalf("head missing: %v", err)
	}
	_, err := s.Get(ctx, "nope", 0, 0)
	var e *Error
	if !errors.Is(err, ErrNotFound) || !errors.As(err, &e) || e.Code != "NoSuchKey" || e.Error() != "s3: 404 NoSuchKey: missing" {
		t.Fatalf("get missing: %v", err)
	}
	if _, err := s.Head(ctx, "nope"); !errors.As(err, &e) || e.Code != "Not Found" {
		t.Fatalf("an empty error body keeps the status text: %v", err)
	}
	other := *s
	other.Bucket = "wrong"
	if _, err := other.List(ctx, ""); !errors.As(err, &e) || e.Code != "NoSuchBucket" {
		t.Fatalf("missing bucket: %v", err)
	}
	if err := other.Put(ctx, "x", strings.NewReader("y")); err == nil {
		t.Fatal("put to a missing bucket")
	}
	if err := other.Delete(ctx, "x"); err == nil {
		t.Fatal("delete in a missing bucket")
	}
	if err := s.Copy(ctx, "nope", "x"); !errors.Is(err, ErrNotFound) {
		t.Fatalf("copy of a missing source: %v", err)
	}
	s.Put(ctx, "a", strings.NewReader("1"))
	f.copyError = true
	if err := s.Copy(ctx, "a", "b"); !errors.As(err, &e) || e.Code != "InternalError" {
		t.Fatalf("an <Error> inside a 200 copy must fail: %v", err)
	}
	if err := s.PutFile(ctx, "x", "/nonexistent/file"); err == nil {
		t.Fatal("missing local file")
	}
	if err := s.Put(ctx, "x", failingReader{}); err == nil {
		t.Fatal("a body that fails to read")
	}
	if err := s.Put(ctx, "x", unseekable{strings.NewReader("abc")}); err == nil {
		t.Fatal("a body that fails to rewind")
	}
	dead := *s
	dead.Endpoint, _ = url.Parse("http://127.0.0.1:1")
	for name, err := range map[string]error{
		"list":   func() error { _, err := dead.List(ctx, ""); return err }(),
		"head":   func() error { _, err := dead.Head(ctx, "x"); return err }(),
		"get":    func() error { _, err := dead.Get(ctx, "x", 0, 0); return err }(),
		"put":    dead.Put(ctx, "x", strings.NewReader("y")),
		"delete": dead.Delete(ctx, "x"),
		"copy":   dead.Copy(ctx, "x", "y"),
	} {
		if err == nil {
			t.Fatalf("%s with the server down must fail", name)
		}
	}
	bad := *s
	bad.Endpoint = &url.URL{Scheme: "http", Host: "h\x7f"}
	if _, err := bad.List(ctx, ""); err == nil {
		t.Fatal("a bad endpoint must fail before the request")
	}
	junk := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) { io.WriteString(w, "<ListBucketResult><Contents>") }))
	defer junk.Close()
	j := *s
	j.Endpoint, _ = url.Parse(junk.URL)
	if _, err := j.List(ctx, ""); err == nil || !strings.Contains(err.Error(), "s3 list") {
		t.Fatalf("broken XML: %v", err)
	}
}

// TestFromEnv: padrões (região us-east-1, endpoint da AWS), anônimo sem chaves, e recusa de
// endpoint que não é http(s) e de meio par de chaves.
func TestFromEnv(t *testing.T) {
	s, err := FromEnv([]string{"PATH=/bin", "junk"}, "b")
	if err != nil || s.Region != "us-east-1" || s.Endpoint.String() != "https://s3.us-east-1.amazonaws.com" || s.Creds.AccessKey != "" {
		t.Fatalf("defaults: %+v %v", s, err)
	}
	s, _ = FromEnv([]string{"AWS_REGION=sa-east-1", "AWS_ACCESS_KEY_ID=a", "AWS_SECRET_ACCESS_KEY=b", "AWS_SESSION_TOKEN=t"}, "b")
	if s.Endpoint.Host != "s3.sa-east-1.amazonaws.com" || s.Creds != (Credentials{"a", "b", "t"}) {
		t.Fatalf("region and keys: %+v", s)
	}
	for _, env := range [][]string{{"AWS_ENDPOINT_URL=ftp://x"}, {"AWS_ENDPOINT_URL=http://"}, {"AWS_ENDPOINT_URL=::"}, {"AWS_ACCESS_KEY_ID=a"}} {
		if _, err := FromEnv(env, "b"); err == nil {
			t.Fatalf("%v must be refused", env)
		}
	}
}

// failingReader falha na leitura (corpo de PUT que quebra no hash).
type failingReader struct{}

// Read falha sempre.
func (failingReader) Read([]byte) (int, error) { return 0, errors.New("disk gone") }

// Seek nunca é alcançado: a leitura falha antes.
func (failingReader) Seek(int64, int) (int64, error) { return 0, nil }

// unseekable lê, mas não volta ao começo.
type unseekable struct{ io.Reader }

// Seek falha sempre.
func (unseekable) Seek(int64, int) (int64, error) { return 0, errors.New("pipe") }
