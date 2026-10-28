package gcedisk

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"os"
	"path"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

// fakeGCE é o Compute Engine de mentira: discos com donos, instâncias com status, operações que terminam na hora (ou
// com erro, quando plantado). Também é o metadata server.
type fakeGCE struct {
	mu        sync.Mutex
	disks     map[string]*fakeDisk
	instances map[string]string // nome -> status
	attached  map[string][]string
	calls     []string
	fail      map[string]int    // "METHOD sufixo" -> status HTTP a devolver
	opErr     map[string]string // "attach"/"detach"/"insert"/"delete" -> mensagem de erro da operação
	pending   bool              // operações voltam RUNNING e só terminam no /wait
	// onStatus muda o status de uma instância a cada leitura (simula a preempção andando).
	onStatus func(name, cur string) string
	// beforeInsert roda antes de criar um disco (simula outro host criando o mesmo disco no mesmo instante).
	beforeInsert func(f *fakeGCE)
	// afterAttach roda quando um disco é anexado (o teste cancela o contexto ali: o que falhar depois já é "depois de
	// criado", sem depender de relógio).
	afterAttach func()
	token        string
	// snapshots são os snapshots globais (nome -> status), e snapFrom diz de que disco cada um saiu.
	snapshots map[string]string
	snapFrom  map[string]string
}

// fakeDisk é um disco no fake.
type fakeDisk struct {
	users  []string
	labels map[string]string
	source string // sourceSnapshot do insert ("" = vazio)
}

// newFake devolve o fake com só esta instância ("me", no ar) e um token válido.
func newFake() *fakeGCE {
	return &fakeGCE{disks: map[string]*fakeDisk{}, instances: map[string]string{"me": "RUNNING"}, attached: map[string][]string{},
		fail: map[string]int{}, opErr: map[string]string{}, token: `{"access_token":"t","expires_in":3600}`,
		snapshots: map[string]string{}, snapFrom: map[string]string{}}
}

// ServeHTTP atende o metadata server (/md/...) e a API zonal (/api/projects/proj/zones/z1/...), com as falhas
// plantadas em fail valendo antes de tudo.
func (f *fakeGCE) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	f.mu.Lock()
	defer f.mu.Unlock()
	p := r.URL.Path
	f.calls = append(f.calls, r.Method+" "+p)
	for k, code := range f.fail {
		if strings.HasSuffix(r.Method+" "+p, k) {
			w.WriteHeader(code)
			return
		}
	}
	switch {
	case strings.HasPrefix(p, "/md/"):
		if r.Header.Get("Metadata-Flavor") != "Google" {
			w.WriteHeader(403)
			return
		}
		switch strings.TrimPrefix(p, "/md/") {
		case "project/project-id":
			w.Write([]byte("proj"))
		case "instance/zone":
			w.Write([]byte("projects/123/zones/z1"))
		case "instance/name":
			w.Write([]byte("me"))
		case "instance/service-accounts/default/token":
			w.Write([]byte(f.token))
		default:
			w.WriteHeader(404)
		}
		return
	case r.Header.Get("Authorization") != "Bearer t":
		w.WriteHeader(401)
		return
	}
	if name, ok := strings.CutPrefix(p, "/api/projects/proj/global/snapshots/"); ok {
		st, found := f.snapshots[name]
		if !found {
			w.WriteHeader(404)
			return
		}
		json.NewEncoder(w).Encode(map[string]any{"status": st})
		return
	}
	parts := strings.Split(strings.TrimPrefix(p, "/api/projects/proj/zones/z1/"), "/")
	switch {
	case parts[0] == "operations" && len(parts) == 3:
		json.NewEncoder(w).Encode(map[string]any{"name": parts[1], "status": "DONE"})
	case parts[0] == "disks" && len(parts) == 1 && r.Method == http.MethodPost:
		var body struct {
			Name           string            `json:"name"`
			Labels         map[string]string `json:"labels"`
			SourceSnapshot string            `json:"sourceSnapshot"`
		}
		json.NewDecoder(r.Body).Decode(&body)
		if f.beforeInsert != nil {
			f.beforeInsert(f)
		}
		if _, ok := f.disks[body.Name]; ok {
			w.WriteHeader(409)
			return
		}
		if f.opErr["insert"] == "" {
			f.disks[body.Name] = &fakeDisk{labels: body.Labels, source: body.SourceSnapshot}
		}
		f.writeOp(w, "insert")
	case parts[0] == "disks" && len(parts) == 3 && parts[2] == "createSnapshot":
		var body struct {
			Name string `json:"name"`
		}
		json.NewDecoder(r.Body).Decode(&body)
		if _, ok := f.snapshots[body.Name]; ok {
			w.WriteHeader(409)
			return
		}
		if f.opErr["snapshot"] == "" {
			f.snapshots[body.Name], f.snapFrom[body.Name] = "READY", parts[1]
		}
		f.writeOp(w, "snapshot")
	case parts[0] == "disks" && len(parts) == 2:
		d, ok := f.disks[parts[1]]
		if !ok {
			w.WriteHeader(404)
			return
		}
		if r.Method == http.MethodDelete {
			if f.opErr["delete"] == "" {
				delete(f.disks, parts[1])
			}
			f.writeOp(w, "delete")
			return
		}
		json.NewEncoder(w).Encode(map[string]any{"users": d.users, "labels": d.labels})
	case parts[0] == "instances" && len(parts) == 2:
		st, ok := f.instances[parts[1]]
		if !ok {
			w.WriteHeader(404)
			return
		}
		if f.onStatus != nil {
			st = f.onStatus(parts[1], st)
			f.instances[parts[1]] = st
		}
		var disks []map[string]string
		for _, d := range f.attached[parts[1]] {
			disks = append(disks, map[string]string{"deviceName": d})
		}
		json.NewEncoder(w).Encode(map[string]any{"status": st, "disks": disks})
	case parts[0] == "instances" && len(parts) == 3 && parts[2] == "attachDisk":
		var body struct {
			DeviceName string `json:"deviceName"`
		}
		json.NewDecoder(r.Body).Decode(&body)
		if f.opErr["attach"] == "" {
			f.disks[body.DeviceName].users = append(f.disks[body.DeviceName].users, "https://x/instances/"+parts[1])
			f.attached[parts[1]] = append(f.attached[parts[1]], body.DeviceName)
		}
		if f.afterAttach != nil {
			f.afterAttach()
		}
		f.writeOp(w, "attach")
	case parts[0] == "instances" && len(parts) == 3 && parts[2] == "detachDisk":
		name := r.URL.Query().Get("deviceName")
		if d, ok := f.disks[name]; ok && f.opErr["detach"] == "" {
			var keep []string
			for _, u := range d.users {
				if path.Base(u) != parts[1] {
					keep = append(keep, u)
				}
			}
			d.users = keep
		}
		f.writeOp(w, "detach")
	default:
		w.WriteHeader(500)
	}
}

// writeOp responde uma operação: DONE (ou RUNNING, com pending), com o erro plantado para o tipo dela.
func (f *fakeGCE) writeOp(w http.ResponseWriter, kind string) {
	op := map[string]any{"name": "op-" + kind, "status": "DONE"}
	if f.pending {
		op["status"] = "RUNNING"
	}
	if msg := f.opErr[kind]; msg != "" {
		op["error"] = map[string]any{"errors": []map[string]string{{"code": "X", "message": msg}}}
	}
	json.NewEncoder(w).Encode(op)
}

// count diz quantas chamadas registradas contêm s.
func (f *fakeGCE) count(s string) int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return strings.Count(strings.Join(f.calls, "\n"), s)
}

// rig monta o adapter contra o fake, com montar/formatar de mentira e um DevDir onde o "udev" já criou o link de
// cada disco que o teste pode anexar.
func rig(t *testing.T) (*Adapter, *fakeGCE, map[string]string, string) {
	t.Helper()
	f := newFake()
	srv := httptest.NewServer(f)
	t.Cleanup(srv.Close)
	dev := t.TempDir()
	mounts := map[string]string{} // dir -> dev
	var mu sync.Mutex
	a := &Adapter{HTTP: srv.Client(), Metadata: srv.URL + "/md", API: srv.URL + "/api", DiskType: "pd-balanced", DevDir: dev,
		FreeTimeout: 200 * time.Millisecond, Poll: 5 * time.Millisecond,
		FormatFn: func(string) error { return nil },
		MountFn: func(d, dir string) error {
			mu.Lock()
			defer mu.Unlock()
			mounts[dir] = d
			return nil
		},
		UnmountFn: func(dir string) error {
			mu.Lock()
			defer mu.Unlock()
			delete(mounts, dir)
			return nil
		},
		IsMountFn: func(dir string) bool {
			mu.Lock()
			defer mu.Unlock()
			_, ok := mounts[dir]
			return ok
		},
	}
	for _, n := range []string{"m1", "m2"} {
		// Um byte de dado: o disco "tem ext4" para o isBlank real (os testes de disco em branco gravam zeros).
		os.WriteFile(filepath.Join(dev, "google-"+DiskName(n)), []byte{0xEF}, 0o600)
	}
	return a, f, mounts, t.TempDir()
}

// TestDiskName: nome válido vira prefixo + nome; maiúscula, sublinhado e nome longo são saneados com o hash do
// original (dois nomes que se saneiam igual não colidem), e o resultado cabe nos 63 do GCE.
func TestDiskName(t *testing.T) {
	if got := DiskName("sbot-6fa2c9b8eca2"); got != "rc-pd-sbot-6fa2c9b8eca2" {
		t.Fatalf("simples: %s", got)
	}
	a, b := DiskName("Big_Name"), DiskName("big-name")
	if a == b || !strings.HasPrefix(a, "rc-pd-big-name-") || b != "rc-pd-big-name" {
		t.Fatalf("saneado: %s %s", a, b)
	}
	long := DiskName(strings.Repeat("a", 80))
	if len(long) > 63 || !strings.HasPrefix(long, "rc-pd-aaa") {
		t.Fatalf("longo (%d): %s", len(long), long)
	}
	if labelValue("ok-name_1") != "ok-name_1" || labelValue("Big") != "" || labelValue(strings.Repeat("a", 64)) != "" {
		t.Fatal("rótulo")
	}
}

// TestEnsureCreates: máquina nova → disco criado com o rótulo, anexado gravável a este host, formatado e montado;
// created=true. De novo, já montado, não chama nada.
func TestEnsureCreates(t *testing.T) {
	a, f, mounts, root := rig(t)
	ctx := context.Background()
	dir := filepath.Join(root, "m1")
	var formatted string
	a.FormatFn = func(d string) error { formatted = d; return nil }
	created, err := a.Ensure(ctx, "m1", dir, 13)
	if err != nil || !created {
		t.Fatalf("create: %v %v", created, err)
	}
	d := f.disks["rc-pd-m1"]
	if d == nil || d.labels[machineLabel] != "m1" || len(d.users) != 1 || path.Base(d.users[0]) != "me" {
		t.Fatalf("disco: %+v", d)
	}
	if mounts[dir] == "" || formatted != mounts[dir] {
		t.Fatalf("montagem: %v formatado %s", mounts, formatted)
	}
	n := f.count("/")
	if c, err := a.Ensure(ctx, "m1", dir, 13); err != nil || c || f.count("/") != n {
		t.Fatalf("já montado: %v %v", c, err)
	}
}

// TestEnsureAdopts: o disco existe solto (a máquina foi solta por outro host) → anexa e monta, SEM formatar (formatar
// apagaria a máquina); preso a este host já (reboot) → só monta, sem anexar de novo.
func TestEnsureAdopts(t *testing.T) {
	a, f, mounts, root := rig(t)
	ctx := context.Background()
	a.FormatFn = func(string) error { t.Fatal("formatou um disco que já tinha a máquina"); return nil }
	f.disks["rc-pd-m1"] = &fakeDisk{}
	if c, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err != nil || c || len(f.disks["rc-pd-m1"].users) != 1 {
		t.Fatalf("adotar solto: %v %v %+v", c, err, f.disks["rc-pd-m1"])
	}
	f.disks["rc-pd-m2"] = &fakeDisk{users: []string{"https://x/instances/me"}}
	before := f.count("attachDisk")
	if c, err := a.Ensure(ctx, "m2", filepath.Join(root, "m2"), 13); err != nil || c || mounts[filepath.Join(root, "m2")] == "" {
		t.Fatalf("já anexado aqui: %v %v", c, err)
	}
	if f.count("attachDisk") != before {
		t.Fatal("anexou de novo o que já estava aqui")
	}
}

// TestClaimOwners trava a regra dos dois donos, caso a caso: dono parado → solta dele e adota; dono apagado → espera
// o GCE soltar; dono desligando (preempção) → espera até ele parar; dono no ar → recusa na hora (ErrBusy); dono que não
// solta no prazo → ErrStillReleasing.
func TestClaimOwners(t *testing.T) {
	ctx := context.Background()
	owned := func(f *fakeGCE, owner, status string) {
		f.disks["rc-pd-m1"] = &fakeDisk{users: []string{"https://x/instances/" + owner}}
		if status != "" {
			f.instances[owner] = status
		}
	}
	t.Run("parado", func(t *testing.T) {
		a, f, _, root := rig(t)
		owned(f, "old", "TERMINATED")
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err != nil {
			t.Fatal(err)
		}
		if u := f.disks["rc-pd-m1"].users; len(u) != 1 || path.Base(u[0]) != "me" {
			t.Fatalf("donos: %v", u)
		}
	})
	// O dono preemptado ainda solta o disco (detach da drenagem em andamento): o detach daqui leva 409. Não é
	// recusa: espera a lista de donos esvaziar e adota.
	t.Run("parado com detach em andamento", func(t *testing.T) {
		a, f, _, root := rig(t)
		owned(f, "old", "TERMINATED")
		f.fail["POST /api/projects/proj/zones/z1/instances/old/detachDisk"] = 409
		go func() {
			time.Sleep(30 * time.Millisecond)
			f.mu.Lock()
			f.disks["rc-pd-m1"].users = nil
			f.mu.Unlock()
		}()
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err != nil {
			t.Fatalf("409 no detach virou erro final: %v", err)
		}
		if u := f.disks["rc-pd-m1"].users; len(u) != 1 || path.Base(u[0]) != "me" {
			t.Fatalf("donos: %v", u)
		}
	})
	// 409 que nunca libera: para no prazo com erro dizendo que o dono ainda solta, sem tomar o disco.
	t.Run("detach em andamento que não termina", func(t *testing.T) {
		a, f, mounts, root := rig(t)
		owned(f, "old", "TERMINATED")
		f.fail["POST /api/projects/proj/zones/z1/instances/old/detachDisk"] = 409
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); !errors.Is(err, ErrStillReleasing) || len(mounts) != 0 {
			t.Fatalf("esperava ErrStillReleasing sem montar: %v %v", err, mounts)
		}
	})
	t.Run("apagado", func(t *testing.T) {
		a, f, _, root := rig(t)
		owned(f, "gone", "")
		// O GCE solta o disco da instância apagada um pouco depois: o adapter espera a lista de donos esvaziar.
		go func() {
			time.Sleep(20 * time.Millisecond)
			f.mu.Lock()
			f.disks["rc-pd-m1"].users = nil
			f.mu.Unlock()
		}()
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err != nil {
			t.Fatal(err)
		}
	})
	t.Run("desligando", func(t *testing.T) {
		a, f, _, root := rig(t)
		owned(f, "spot", "STOPPING")
		n := 0
		f.onStatus = func(name, cur string) string {
			if name != "spot" {
				return cur
			}
			if n++; n >= 3 {
				return "TERMINATED" // a preempção terminou
			}
			return cur
		}
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err != nil {
			t.Fatal(err)
		}
	})
	t.Run("no ar", func(t *testing.T) {
		a, f, mounts, root := rig(t)
		owned(f, "alive", "RUNNING")
		_, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13)
		if !errors.Is(err, ErrBusy) || len(mounts) != 0 || path.Base(f.disks["rc-pd-m1"].users[0]) != "alive" {
			t.Fatalf("tomou o disco de host vivo: %v %v", err, f.disks["rc-pd-m1"].users)
		}
	})
	t.Run("não solta", func(t *testing.T) {
		a, f, _, root := rig(t)
		owned(f, "stuck", "STOPPING")
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); !errors.Is(err, ErrStillReleasing) {
			t.Fatalf("esperou para sempre: %v", err)
		}
	})
	t.Run("dono ilegível", func(t *testing.T) {
		a, f, _, root := rig(t)
		owned(f, "odd", "RUNNING")
		f.fail["GET /api/projects/proj/zones/z1/instances/odd"] = 500
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err == nil || errors.Is(err, ErrBusy) {
			t.Fatalf("erro de API virou outra coisa: %v", err)
		}
	})
	t.Run("disco some na espera", func(t *testing.T) {
		a, f, _, root := rig(t)
		owned(f, "spot", "STOPPING")
		f.onStatus = func(name, cur string) string {
			delete(f.disks, "rc-pd-m1") // alguém apagou o disco enquanto o dono desligava
			return cur
		}
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err == nil {
			t.Fatal("seguiu sem o disco")
		}
	})
	t.Run("detach do parado falha", func(t *testing.T) {
		a, f, _, root := rig(t)
		owned(f, "old", "TERMINATED")
		f.opErr["detach"] = "boom"
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err == nil || !strings.Contains(err.Error(), "boom") {
			t.Fatalf("detach falhou calado: %v", err)
		}
	})
	t.Run("ctx acaba esperando", func(t *testing.T) {
		a, f, _, root := rig(t)
		owned(f, "stuck", "STOPPING")
		a.FreeTimeout = time.Hour
		cctx, cancel := context.WithTimeout(ctx, 30*time.Millisecond)
		defer cancel()
		if _, err := a.Ensure(cctx, "m1", filepath.Join(root, "m1"), 13); err == nil {
			t.Fatal("ctx ignorado")
		}
	})
}

// TestEnsureFailures: cada passo que falha devolve erro, sem montar nada pela metade; e o 409 da corrida (outro host
// criou o mesmo disco no mesmo instante) adota em vez de falhar, sem formatar.
func TestEnsureFailures(t *testing.T) {
	ctx := context.Background()
	cases := map[string]func(a *Adapter, f *fakeGCE){
		"metadata":         func(a *Adapter, f *fakeGCE) { a.Metadata += "/nada" },
		"token":            func(a *Adapter, f *fakeGCE) { f.token = `{"oops":1}` },
		"ler disco":        func(a *Adapter, f *fakeGCE) { f.fail["GET /api/projects/proj/zones/z1/disks/rc-pd-m1"] = 500 },
		"criar":            func(a *Adapter, f *fakeGCE) { f.opErr["insert"] = "quota" },
		"criar http":       func(a *Adapter, f *fakeGCE) { f.fail["POST /api/projects/proj/zones/z1/disks"] = 403 },
		"anexar":           func(a *Adapter, f *fakeGCE) { f.opErr["attach"] = "no" },
		"formatar":         func(a *Adapter, f *fakeGCE) { a.FormatFn = func(string) error { return errors.New("mkfs") } },
		"montar":           func(a *Adapter, f *fakeGCE) { a.MountFn = func(string, string) error { return errors.New("EINVAL") } },
		"metadata recusa":  func(a *Adapter, f *fakeGCE) { f.fail["GET /md/instance/name"] = 500 },
		"transporte":       func(a *Adapter, f *fakeGCE) { a.API = "http://127.0.0.1:1" },
		"metadata offline": func(a *Adapter, f *fakeGCE) { a.Metadata = "http://127.0.0.1:1" },
		"diretório":        func(a *Adapter, f *fakeGCE) {},
	}
	for name, plant := range cases {
		t.Run(name, func(t *testing.T) {
			a, f, mounts, root := rig(t)
			plant(a, f)
			dir := filepath.Join(root, "m1")
			if name == "diretório" {
				// O pai do diretório da máquina é um arquivo: o MkdirAll falha.
				os.WriteFile(filepath.Join(root, "file"), nil, 0o600)
				dir = filepath.Join(root, "file", "m1")
			}
			if _, err := a.Ensure(ctx, "m1", dir, 13); err == nil {
				t.Fatalf("sem erro; montagens %v", mounts)
			}
			if len(mounts) != 0 {
				t.Fatalf("montou pela metade: %v", mounts)
			}
		})
	}
	t.Run("dispositivo não aparece", func(t *testing.T) {
		a, _, _, root := rig(t)
		a.DevDir = filepath.Join(a.DevDir, "vazio")
		cctx, cancel := context.WithTimeout(ctx, 30*time.Millisecond)
		defer cancel()
		if _, err := a.Ensure(cctx, "m1", filepath.Join(root, "m1"), 13); err == nil {
			t.Fatal("sem erro")
		}
	})
	t.Run("corrida no create", func(t *testing.T) {
		a, f, _, root := rig(t)
		a.FormatFn = func(string) error { t.Fatal("formatou o disco criado por outro host"); return nil }
		f.beforeInsert = func(f *fakeGCE) { f.disks["rc-pd-m1"] = &fakeDisk{} }
		if created, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err != nil || created {
			t.Fatalf("corrida: %v %v", created, err)
		}
	})
}

// TestEnsureFormatsBlankAdopted: disco que existe mas nunca foi formatado (a tentativa que o criou morreu antes do
// mkfs) é formatado ao ser adotado — sem isso a montagem falhava com EINVAL para sempre (medido em 25/10/2026). Disco
// com qualquer byte diferente de zero no começo NÃO é formatado: é dado de alguém.
func TestEnsureFormatsBlankAdopted(t *testing.T) {
	ctx := context.Background()
	for _, tc := range []struct {
		name   string
		data   []byte
		format bool
	}{
		{"todo zero", make([]byte, 4096), true},
		{"vazio", nil, true},
		{"zero e um byte no fim do 1 MiB", append(make([]byte, blankProbe-1), 1), false},
		{"byte depois do 1 MiB não conta", append(make([]byte, blankProbe), 1), true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			a, f, mounts, root := rig(t)
			os.WriteFile(filepath.Join(a.DevDir, "google-"+DiskName("m1")), tc.data, 0o600)
			f.disks["rc-pd-m1"] = &fakeDisk{}
			formatted := false
			a.FormatFn = func(string) error { formatted = true; return nil }
			created, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13)
			if err != nil || created || formatted != tc.format || mounts[filepath.Join(root, "m1")] == "" {
				t.Fatalf("created=%v err=%v formatou=%v (esperava %v)", created, err, formatted, tc.format)
			}
		})
	}
	t.Run("dispositivo ilegível", func(t *testing.T) {
		a, f, mounts, root := rig(t)
		devPath := filepath.Join(a.DevDir, "google-"+DiskName("m1"))
		os.Remove(devPath)
		os.Mkdir(devPath, 0o700) // um diretório no lugar do dispositivo: a leitura falha
		f.disks["rc-pd-m1"] = &fakeDisk{}
		a.FormatFn = func(string) error { t.Fatal("formatou sem conseguir ler"); return nil }
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err == nil || !strings.Contains(err.Error(), "read") {
			t.Fatalf("sem erro de leitura: %v", err)
		}
		if len(mounts) != 0 {
			t.Fatalf("montou: %v", mounts)
		}
	})
	t.Run("BlankFn que falha", func(t *testing.T) {
		a, f, _, root := rig(t)
		f.disks["rc-pd-m1"] = &fakeDisk{}
		a.BlankFn = func(string) (bool, error) { return false, errors.New("EIO") }
		if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err == nil || !strings.Contains(err.Error(), "EIO") {
			t.Fatalf("erro do BlankFn não apareceu: %v", err)
		}
	})
}

// TestEnsureCreatedOnLaterFailure: o disco nasceu nesta chamada e um passo seguinte falhou (anexar, dispositivo que
// não aparece, mkfs) → created=true junto com o erro, para quem chamou apagar o disco em vez de deixá-lo vazar cru.
func TestEnsureCreatedOnLaterFailure(t *testing.T) {
	ctx := context.Background()
	cases := map[string]func(a *Adapter, f *fakeGCE) context.Context{
		"anexar": func(a *Adapter, f *fakeGCE) context.Context { f.opErr["attach"] = "no"; return ctx },
		"formatar": func(a *Adapter, f *fakeGCE) context.Context {
			a.FormatFn = func(string) error { return errors.New("mkfs") }
			return ctx
		},
		"dispositivo": func(a *Adapter, f *fakeGCE) context.Context {
			// O dispositivo nunca aparece, e o contexto cai no anexar: antes era um prazo de 30 ms, que com o gate
			// rodando todos os pacotes em paralelo vencia ainda na leitura do disco (antes de criar) e o teste
			// reprovava sem defeito nenhum (medido em 25/10/2026).
			a.DevDir = filepath.Join(a.DevDir, "vazio")
			c, cancel := context.WithCancel(ctx)
			t.Cleanup(cancel)
			f.afterAttach = cancel
			return c
		},
	}
	for name, plant := range cases {
		t.Run(name, func(t *testing.T) {
			a, f, _, root := rig(t)
			c := plant(a, f)
			created, err := a.Ensure(c, "m1", filepath.Join(root, "m1"), 13)
			if err == nil || !created {
				t.Fatalf("created=%v err=%v (o disco recém-criado tem de voltar como criado)", created, err)
			}
		})
	}
}

// TestReleaseAndDestroy: Release desmonta (tentando de novo no EBUSY), solta e tira o diretório; solto de novo não é
// erro (o disco já não está aqui). Destroy apaga o disco; disco que já sumiu não é erro. E cada falha aparece.
func TestReleaseAndDestroy(t *testing.T) {
	a, f, mounts, root := rig(t)
	ctx := context.Background()
	dir := filepath.Join(root, "m1")
	if _, err := a.Ensure(ctx, "m1", dir, 13); err != nil {
		t.Fatal(err)
	}
	busy := 2
	unmount := a.UnmountFn
	a.UnmountFn = func(d string) error {
		if busy > 0 {
			busy--
			return errors.New("EBUSY")
		}
		return unmount(d)
	}
	if err := a.Release(ctx, "m1", dir); err != nil {
		t.Fatal(err)
	}
	if len(mounts) != 0 || len(f.disks["rc-pd-m1"].users) != 0 {
		t.Fatalf("não soltou: %v %v", mounts, f.disks["rc-pd-m1"].users)
	}
	if _, err := os.Stat(dir); !os.IsNotExist(err) {
		t.Fatalf("diretório ficou: %v", err)
	}
	// As duas redações do GCE para "esse disco não está anexado aqui" contam como já solto.
	for _, msg := range []string{"The disk resource 'x' is not found in instance 'me'", "No attached disk found with device name 'rc-pd-m1'"} {
		f.opErr["detach"] = msg
		if err := a.Release(ctx, "m1", dir); err != nil {
			t.Fatalf("soltar de novo (%s): %v", msg, err)
		}
	}
	delete(f.opErr, "detach")
	if err := a.Destroy(ctx, "m1", dir); err != nil || f.disks["rc-pd-m1"] != nil {
		t.Fatalf("destroy: %v %+v", err, f.disks["rc-pd-m1"])
	}
	if err := a.Destroy(ctx, "m1", dir); err != nil {
		t.Fatalf("destroy de disco que sumiu: %v", err)
	}

	t.Run("desmontar travado", func(t *testing.T) {
		a, _, _, root := rig(t)
		d := filepath.Join(root, "m1")
		a.Ensure(ctx, "m1", d, 13)
		a.UnmountFn = func(string) error { return errors.New("EBUSY") }
		if err := a.Release(ctx, "m1", d); err == nil || !strings.Contains(err.Error(), "EBUSY") {
			t.Fatalf("desmontar travado: %v", err)
		}
		cctx, cancel := context.WithCancel(ctx)
		cancel()
		if err := a.Release(cctx, "m1", d); err == nil {
			t.Fatal("ctx cancelado no unmount")
		}
	})
	t.Run("detach e delete falham", func(t *testing.T) {
		a, f, _, root := rig(t)
		d := filepath.Join(root, "m1")
		a.Ensure(ctx, "m1", d, 13)
		f.opErr["detach"] = "boom"
		if err := a.Release(ctx, "m1", d); err == nil {
			t.Fatal("detach falhou calado")
		}
		delete(f.opErr, "detach")
		f.opErr["delete"] = "in use"
		if err := a.Destroy(ctx, "m1", d); err == nil {
			t.Fatal("delete falhou calado")
		}
		if err := a.Destroy(ctx, "m1", d); err == nil || f.disks["rc-pd-m1"] == nil {
			t.Fatal("o disco sumiu com o delete falhando")
		}
	})
	t.Run("sem metadata", func(t *testing.T) {
		a, _, _, root := rig(t)
		a.Metadata = "http://127.0.0.1:1"
		if err := a.Release(ctx, "m1", filepath.Join(root, "m1")); err == nil {
			t.Fatal("release sem metadata")
		}
		if err := a.Destroy(ctx, "m1", filepath.Join(root, "m1")); err == nil {
			t.Fatal("destroy sem metadata")
		}
	})
	t.Run("diretório com arquivo", func(t *testing.T) {
		a, _, _, root := rig(t)
		d := filepath.Join(root, "m1")
		os.MkdirAll(d, 0o700)
		os.WriteFile(filepath.Join(d, "x"), nil, 0o600)
		if err := a.Release(ctx, "m1", d); err == nil {
			t.Fatal("diretório que não era só o ponto de montagem sumiu calado")
		}
	})
}

// TestRecover: depois de um reboot, remonta só os discos de máquina anexados aqui, pelo rótulo; pula o já montado, o
// que não é de máquina e o sem rótulo. Cada falha aparece.
func TestRecover(t *testing.T) {
	a, f, mounts, root := rig(t)
	ctx := context.Background()
	f.disks["rc-pd-m1"] = &fakeDisk{labels: map[string]string{machineLabel: "m1"}}
	f.disks["rc-pd-m2"] = &fakeDisk{labels: map[string]string{machineLabel: "m2"}}
	f.disks["rc-pd-anon"] = &fakeDisk{}
	f.attached["me"] = []string{"boot", "rc-pd-m1", "rc-pd-m2", "rc-pd-anon"}
	mounts[filepath.Join(root, "m2")] = "já"
	n, err := a.Recover(ctx, root)
	if err != nil || n != 1 || mounts[filepath.Join(root, "m1")] == "" {
		t.Fatalf("recover: %d %v %v", n, err, mounts)
	}
	plant := map[string]func(a *Adapter, f *fakeGCE){
		"instância ilegível": func(a *Adapter, f *fakeGCE) { f.fail["GET /api/projects/proj/zones/z1/instances/me"] = 500 },
		"disco ilegível":     func(a *Adapter, f *fakeGCE) { f.fail["GET /api/projects/proj/zones/z1/disks/rc-pd-m1"] = 500 },
		"montagem":           func(a *Adapter, f *fakeGCE) { a.MountFn = func(string, string) error { return errors.New("EINVAL") } },
		"sem metadata":       func(a *Adapter, f *fakeGCE) { a.Metadata = "http://127.0.0.1:1" },
		"diretório":          func(a *Adapter, f *fakeGCE) {},
	}
	for name, p := range plant {
		t.Run(name, func(t *testing.T) {
			a, f, _, root := rig(t)
			f.attached["me"] = []string{"rc-pd-m1"}
			f.disks["rc-pd-m1"] = &fakeDisk{labels: map[string]string{machineLabel: "m1"}}
			p(a, f)
			if name == "diretório" {
				os.WriteFile(filepath.Join(root, "m1"), nil, 0o600) // um arquivo no lugar do diretório da máquina
			}
			if _, err := a.Recover(ctx, root); err == nil {
				t.Fatal("sem erro")
			}
		})
	}
	t.Run("dispositivo ausente", func(t *testing.T) {
		a, f, _, root := rig(t)
		f.attached["me"] = []string{"rc-pd-m1"}
		f.disks["rc-pd-m1"] = &fakeDisk{labels: map[string]string{machineLabel: "m1"}}
		a.DevDir = filepath.Join(a.DevDir, "vazio")
		cctx, cancel := context.WithTimeout(ctx, 30*time.Millisecond)
		defer cancel()
		if _, err := a.Recover(cctx, root); err == nil {
			t.Fatal("sem erro")
		}
	})
}

// TestOperationWait: operação que volta RUNNING é esperada pelo /wait até DONE; /wait que falha devolve o erro.
func TestOperationWait(t *testing.T) {
	a, f, _, root := rig(t)
	ctx := context.Background()
	f.pending = true
	if _, err := a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13); err != nil {
		t.Fatal(err)
	}
	if f.count("/operations/op-insert/wait") == 0 {
		t.Fatalf("não esperou a operação: %v", f.calls)
	}
	b, fb, _, rootB := rig(t)
	fb.pending = true
	fb.fail["POST /api/projects/proj/zones/z1/operations/op-insert/wait"] = 500
	if _, err := b.Ensure(ctx, "m1", filepath.Join(rootB, "m1"), 13); err == nil {
		t.Fatal("wait falhou calado")
	}
}

// TestTokenCached: o token da conta de serviço é pedido uma vez e reaproveitado até perto de vencer.
func TestTokenCached(t *testing.T) {
	a, f, _, root := rig(t)
	ctx := context.Background()
	a.Ensure(ctx, "m1", filepath.Join(root, "m1"), 13)
	a.Ensure(ctx, "m2", filepath.Join(root, "m2"), 13)
	if n := f.count("service-accounts/default/token"); n != 1 {
		t.Fatalf("pediu o token %d vezes", n)
	}
}

// TestNewAndOS: os padrões de produção e as chamadas do SO deste sistema (fora do Linux, todas recusam).
func TestNewAndOS(t *testing.T) {
	a := New()
	if a.DiskType != "pd-balanced" || a.DevDir != "/dev/disk/by-id" || a.FreeTimeout != 90*time.Second || a.Poll != 2*time.Second {
		t.Fatalf("padrões: %+v", a)
	}
	dir := t.TempDir()
	if a.Mounted(dir) {
		t.Fatal("tempdir montado")
	}
	if err := a.format("/dev/null"); err == nil {
		t.Skip("rodando no Linux: o mkfs de verdade não roda em teste")
	}
	if a.mount("/dev/null", dir) == nil || a.unmount(dir) == nil {
		t.Fatal("fora do Linux devia recusar")
	}
}

// TestEnsureFromSnapshot: disco novo nasce do snapshot dourado (sourceSnapshot global do projeto) e é montado SEM
// formatar — formatar apagaria a máquina que veio dentro dele; disco que já existe é adotado e o snapshot é ignorado;
// sem snapshot é erro, antes de chamar o GCE.
func TestEnsureFromSnapshot(t *testing.T) {
	a, f, mounts, root := rig(t)
	ctx := context.Background()
	formatted := 0
	a.FormatFn = func(string) error { formatted++; return nil }
	dir := filepath.Join(root, "m1")
	created, err := a.EnsureFrom(ctx, "m1", dir, 13, "rc-golden-trial-abc")
	if err != nil || !created || mounts[dir] == "" || formatted != 0 {
		t.Fatalf("do snapshot: %v %v montado=%q formatado=%d", created, err, mounts[dir], formatted)
	}
	if d := f.disks["rc-pd-m1"]; d.source != "projects/proj/global/snapshots/rc-golden-trial-abc" || d.labels[machineLabel] != "m1" {
		t.Fatalf("origem: %+v", d)
	}
	f.disks["rc-pd-m2"] = &fakeDisk{}
	if c, err := a.EnsureFrom(ctx, "m2", filepath.Join(root, "m2"), 13, "rc-golden-trial-abc"); err != nil || c || f.disks["rc-pd-m2"].source != "" {
		t.Fatalf("adotado: %v %v", c, err)
	}
	n := f.count("/")
	if _, err := a.EnsureFrom(ctx, "m3", filepath.Join(root, "m3"), 13, ""); err == nil || f.count("/") != n {
		t.Fatalf("sem snapshot: %v", err)
	}
}

// TestSnapshotPublish: o snapshot sai do disco já solto (desmontado e sem anexo, para ser consistente), com o rótulo da
// máquina, e passa a existir pronto; o mesmo nome de novo é conflito; erro da operação sobe; SnapshotExists diz não ao
// que não existe e ao que ainda não está READY.
func TestSnapshotPublish(t *testing.T) {
	a, f, mounts, root := rig(t)
	ctx := context.Background()
	dir := filepath.Join(root, "m1")
	if _, err := a.Ensure(ctx, "m1", dir, 13); err != nil {
		t.Fatal(err)
	}
	if ok, err := a.SnapshotExists(ctx, "rc-golden-x"); err != nil || ok {
		t.Fatalf("antes: %v %v", ok, err)
	}
	if err := a.Snapshot(ctx, "m1", dir, "rc-golden-x"); err != nil {
		t.Fatal(err)
	}
	if _, still := mounts[dir]; still || len(f.disks["rc-pd-m1"].users) != 0 || f.snapFrom["rc-golden-x"] != "rc-pd-m1" {
		t.Fatalf("o disco não foi solto antes: montado=%v users=%v de=%q", still, f.disks["rc-pd-m1"].users, f.snapFrom["rc-golden-x"])
	}
	if ok, err := a.SnapshotExists(ctx, "rc-golden-x"); err != nil || !ok {
		t.Fatalf("depois: %v %v", ok, err)
	}
	if err := a.Snapshot(ctx, "m1", dir, "rc-golden-x"); !errors.Is(err, errConflict) {
		t.Fatalf("mesmo nome: %v", err)
	}
	f.opErr["snapshot"] = "quota"
	if err := a.Snapshot(ctx, "m1", dir, "rc-golden-y"); err == nil || !strings.Contains(err.Error(), "quota") {
		t.Fatalf("erro da operação: %v", err)
	}
	f.snapshots["rc-golden-z"] = "CREATING"
	if ok, err := a.SnapshotExists(ctx, "rc-golden-z"); err != nil || ok {
		t.Fatalf("criando: %v %v", ok, err)
	}
	f.fail["GET /api/projects/proj/global/snapshots/rc-golden-w"] = 500
	if _, err := a.SnapshotExists(ctx, "rc-golden-w"); err == nil {
		t.Fatal("erro da API engolido")
	}
}
