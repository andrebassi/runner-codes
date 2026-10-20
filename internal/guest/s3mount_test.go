package guest

import (
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"strings"
	"testing"

	"runner.code/internal/protocol"
	"runner.code/internal/s3fs"
)

// fakeMount registra o que foi montado e quantas vezes foi desmontado.
type fakeMount struct {
	dir       string
	core      *s3fs.FS
	unmounted int
}

// Unmount conta a desmontagem.
func (m *fakeMount) Unmount() error { m.unmounted++; return nil }

// TestMountS3: o guest confere o bucket (uma listagem assinada com as chaves do Env) antes
// de montar, monta no caminho com prefixo e :ro, e o shutdown desmonta; bucket que o S3
// recusa, pedido incompleto, endpoint inválido, diretório impossível e falha da montagem
// voltam como erro com o motivo.
func TestMountS3(t *testing.T) {
	var auth, query string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if strings.HasPrefix(r.URL.Path, "/missing") {
			w.WriteHeader(404)
			io.WriteString(w, "<Error><Code>NoSuchBucket</Code><Message>gone</Message></Error>")
			return
		}
		auth, query = r.Header.Get("Authorization"), r.URL.RawQuery
		io.WriteString(w, "<ListBucketResult></ListBucketResult>")
	}))
	defer srv.Close()
	var mounted *fakeMount
	mountFUSE = func(dir string, core *s3fs.FS) (unmounter, error) {
		mounted = &fakeMount{dir: dir, core: core}
		return mounted, nil
	}
	defer func() { mountFUSE = mountFUSEOS; s3Mounts = nil }()
	env := []string{"AWS_ACCESS_KEY_ID=k", "AWS_SECRET_ACCESS_KEY=s", "AWS_ENDPOINT_URL=" + srv.URL}
	dir := filepath.Join(t.TempDir(), "data")
	job := Job{TraceID: "m", Op: protocol.OpMountS3, Path: dir, Env: env,
		S3: &protocol.S3Mount{Bucket: "bkt", Prefix: "datasets", ReadOnly: true}}
	if r := mountS3(job); r.Status != "success" {
		t.Fatalf("mount: %+v", r)
	}
	if mounted.dir != dir || mounted.core.Prefix() != "datasets/" || !mounted.core.ReadOnly() {
		t.Fatalf("mounted: %+v", mounted)
	}
	if !strings.HasPrefix(auth, "AWS4-HMAC-SHA256 Credential=k/") || !strings.Contains(query, "prefix=datasets%2F") {
		t.Fatalf("the bucket check must be signed and scoped to the prefix: %q %q", auth, query)
	}
	unmountS3()
	if mounted.unmounted != 1 || len(s3Mounts) != 0 {
		t.Fatal("shutdown must unmount every s3 volume")
	}
	bad := func(j Job, want string) {
		t.Helper()
		if r := mountS3(j); r.Status == "success" || !strings.Contains(r.Error, want) {
			t.Fatalf("want %q: %+v", want, r)
		}
	}
	bad(Job{Path: dir}, "bucket and path are required")
	bad(Job{S3: &protocol.S3Mount{Bucket: "b"}}, "bucket and path are required")
	bad(Job{Path: dir, S3: &protocol.S3Mount{Bucket: "missing"}, Env: env}, "NoSuchBucket")
	bad(Job{Path: dir, S3: &protocol.S3Mount{Bucket: "b"}, Env: []string{"AWS_ENDPOINT_URL=ftp://x"}}, "AWS_ENDPOINT_URL")
	bad(Job{Path: "/dev/null/x", S3: &protocol.S3Mount{Bucket: "b"}, Env: env}, "mount-s3")
	mountFUSE = func(string, *s3fs.FS) (unmounter, error) { return nil, errors.New("no /dev/fuse") }
	bad(job, "no /dev/fuse")
	if _, err := mountFUSEOS(dir, nil); err == nil {
		t.Fatal("outside Linux the real mount refuses")
	}
}
