package flymachines

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
)

// fakeAPI imita a API de Machines o bastante para o cliente: guarda as chamadas e responde do jeito do Fly.
type fakeAPI struct {
	mu         sync.Mutex
	calls      []string
	failOn     string
	lastBody   map[string]any
	capacityIn string
	regions    []string
}

// ServeHTTP registra "MÉTODO caminho" e responde.
func (f *fakeAPI) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	f.mu.Lock()
	defer f.mu.Unlock()
	call := r.Method + " " + r.URL.Path
	if r.URL.RawQuery != "" {
		call += "?" + r.URL.RawQuery
	}
	f.calls = append(f.calls, call)
	if r.Header.Get("Authorization") != "Bearer tok" {
		w.WriteHeader(http.StatusUnauthorized)
		return
	}
	if f.failOn != "" && strings.HasPrefix(call, f.failOn) {
		w.WriteHeader(http.StatusUnprocessableEntity)
		io.WriteString(w, `{"error":"invalid image"}`)
		return
	}
	body, _ := io.ReadAll(r.Body)
	f.lastBody = nil
	json.Unmarshal(body, &f.lastBody)
	if reg, ok := f.lastBody["region"].(string); ok && r.URL.Path == "/v1/apps/vms/machines" {
		f.regions = append(f.regions, reg)
		if reg == f.capacityIn {
			w.WriteHeader(http.StatusPreconditionFailed)
			io.WriteString(w, `{"error":"failed_precondition: insufficient resources to create new machine with existing volume","status":"volume_placement_capacity"}`)
			return
		}
	}
	switch {
	case r.Method == "POST" && r.URL.Path == "/v1/apps/vms/volumes":
		io.WriteString(w, `{"id":"vol_1"}`)
	case r.Method == "POST" && r.URL.Path == "/v1/apps/vms/machines":
		io.WriteString(w, `{"id":"m1","name":"pg-abc","state":"created","private_ip":"fdaa::3","config":{"metadata":{"visitor":"pg-abc"},"mounts":[{"volume":"vol_1"}]}}`)
	case r.Method == "GET" && r.URL.Path == "/v1/apps/vms/machines":
		io.WriteString(w, `[{"id":"m1","name":"pg-abc","state":"started","private_ip":"fdaa::3","created_at":"2026-10-20T10:00:00Z","config":{"metadata":{"visitor":"pg-abc"},"mounts":[{"volume":"vol_1"}]}},{"id":"m2","name":"x","state":"stopped","config":{}}]`)
	default:
		w.WriteHeader(http.StatusOK)
	}
}

// newClient liga o cliente ao fake.
func newClient(t *testing.T) (*Client, *fakeAPI) {
	t.Helper()
	f := &fakeAPI{}
	srv := httptest.NewServer(f)
	t.Cleanup(srv.Close)
	return &Client{Base: srv.URL + "/v1", App: "vms", Token: "tok",
		Spec: Spec{Image: "registry.fly.io/vms:1", Regions: []string{"gru"}, CPUKind: "shared", CPUs: 2, MemoryMB: 2048, VolumeGB: 1, Mount: "/data"}}, f
}

// TestCreateSendsSpec: cria o volume e depois a máquina com imagem, tamanho, montagem, env e metadata.
func TestCreateSendsSpec(t *testing.T) {
	c, f := newClient(t)
	m, err := c.Create(context.Background(), "pg-abc", map[string]string{"RC_TOKEN": "x"}, map[string]string{"visitor": "pg-abc"})
	if err != nil {
		t.Fatal(err)
	}
	if m.ID != "m1" || m.Volume != "vol_1" || m.PrivateIP != "fdaa::3" || m.Metadata["visitor"] != "pg-abc" {
		t.Fatalf("máquina %+v", m)
	}
	cfg := f.lastBody["config"].(map[string]any)
	guest := cfg["guest"].(map[string]any)
	mount := cfg["mounts"].([]any)[0].(map[string]any)
	if cfg["image"] != "registry.fly.io/vms:1" || guest["memory_mb"] != float64(2048) || mount["path"] != "/data" ||
		cfg["env"].(map[string]any)["RC_TOKEN"] != "x" || cfg["restart"].(map[string]any)["policy"] != "no" {
		t.Fatalf("corpo %v", f.lastBody)
	}
	if f.calls[0] != "POST /v1/apps/vms/volumes" || f.calls[1] != "POST /v1/apps/vms/machines" {
		t.Fatalf("chamadas %v", f.calls)
	}
}

// TestCreateFailureCleansVolume: máquina que não nasce apaga o volume que acabou de criar.
func TestCreateFailureCleansVolume(t *testing.T) {
	c, f := newClient(t)
	f.failOn = "POST /v1/apps/vms/machines"
	if _, err := c.Create(context.Background(), "pg-abc", nil, nil); err == nil || !strings.Contains(err.Error(), "invalid image") {
		t.Fatalf("erro %v", err)
	}
	if last := f.calls[len(f.calls)-1]; last != "DELETE /v1/apps/vms/volumes/vol_1" {
		t.Fatalf("volume órfão: %v", f.calls)
	}
}

// TestListStartStopWaitDelete: as outras chamadas vão para o caminho certo; Delete leva o volume junto.
func TestListStartStopWaitDelete(t *testing.T) {
	c, f := newClient(t)
	ctx := context.Background()
	ms, err := c.List(ctx)
	if err != nil || len(ms) != 2 || ms[0].Metadata["visitor"] != "pg-abc" || ms[0].Volume != "vol_1" || ms[1].Volume != "" {
		t.Fatalf("list %+v %v", ms, err)
	}
	c.Start(ctx, "m1")
	c.Stop(ctx, "m1")
	c.Wait(ctx, "m1", "started", 30)
	c.Delete(ctx, ms[0])
	c.Delete(ctx, ms[1])
	want := []string{"GET /v1/apps/vms/machines", "POST /v1/apps/vms/machines/m1/start", "POST /v1/apps/vms/machines/m1/stop",
		"GET /v1/apps/vms/machines/m1/wait?state=started&timeout=30", "DELETE /v1/apps/vms/machines/m1?force=true",
		"DELETE /v1/apps/vms/volumes/vol_1", "DELETE /v1/apps/vms/machines/m2?force=true"}
	if strings.Join(f.calls, "|") != strings.Join(want, "|") {
		t.Fatalf("chamadas\n%v\nqueria\n%v", f.calls, want)
	}
}

// TestErrors: token errado e falha de rede viram erro com o caminho; Delete que falha não apaga o volume.
func TestErrors(t *testing.T) {
	c, f := newClient(t)
	c.Token = "errado"
	if _, err := c.List(context.Background()); err == nil || !strings.Contains(err.Error(), "401") {
		t.Fatalf("token errado: %v", err)
	}
	c.Token = "tok"
	f.failOn = "DELETE /v1/apps/vms/machines/m1"
	if err := c.Delete(context.Background(), Machine{ID: "m1", Volume: "vol_1"}); err == nil {
		t.Fatal("delete que falha passou")
	}
	if last := f.calls[len(f.calls)-1]; strings.Contains(last, "volumes") {
		t.Fatalf("apagou o volume de uma máquina que não saiu: %v", f.calls)
	}
	c.Base = "http://127.0.0.1:1/v1"
	if _, err := c.List(context.Background()); err == nil {
		t.Fatal("rede fora não deu erro")
	}
}

// TestCreateRegionFallback: falta de capacidade no gru passa para o iad (volume do gru apagado); outro erro não
// tenta a próxima região; sem região é erro.
func TestCreateRegionFallback(t *testing.T) {
	c, f := newClient(t)
	c.Spec.Regions = []string{"gru", "iad"}
	f.capacityIn = "gru"
	m, err := c.Create(context.Background(), "pg-abc", nil, nil)
	if err != nil || m.ID != "m1" {
		t.Fatalf("fallback: %+v %v", m, err)
	}
	got := strings.Join(f.calls, "|")
	if got != "POST /v1/apps/vms/volumes|POST /v1/apps/vms/machines|DELETE /v1/apps/vms/volumes/vol_1|POST /v1/apps/vms/volumes|POST /v1/apps/vms/machines" || f.regions[len(f.regions)-1] != "iad" {
		t.Fatalf("chamadas %s regiões %v", got, f.regions)
	}
	c2, f2 := newClient(t)
	c2.Spec.Regions = []string{"gru", "iad"}
	f2.failOn = "POST /v1/apps/vms/machines"
	c2.Create(context.Background(), "pg-abc", nil, nil)
	if strings.Count(strings.Join(f2.calls, "|"), "POST /v1/apps/vms/volumes") != 1 {
		t.Fatalf("erro sem ser de capacidade tentou outra região: %v", f2.calls)
	}
	c2.Spec.Regions = nil
	if _, err := c2.Create(context.Background(), "pg-abc", nil, nil); err == nil || !strings.Contains(err.Error(), "no region") {
		t.Fatalf("sem região: %v", err)
	}
}
