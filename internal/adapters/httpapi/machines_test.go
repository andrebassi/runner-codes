package httpapi

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http/httptest"
	"os"
	"strings"
	"testing"
	"time"

	"runner.code/internal/app"
	"runner.code/internal/domain"
)

// fakeMachines responde conforme err (se houver) e grava o que recebeu.
type fakeMachines struct {
	// chunks, se houver, são os pedaços de saída que o Exec manda (stream, texto).
	chunks   [][2]string
	err      error
	files    map[string][]byte
	lastExec app.ExecRequest
	stopped  time.Duration
	forced   bool
	tail     int
	source   string
	created  domain.Machine
}

// Pause devolve o erro configurado.
func (f *fakeMachines) Pause(string) error { return f.err }

// Resume devolve o erro configurado.
func (f *fakeMachines) Resume(string) error { return f.err }

// Sync devolve o erro configurado.
func (f *fakeMachines) Sync(context.Context, string) error { return f.err }

// Branch devolve a máquina nova.
func (f *fakeMachines) Branch(_ context.Context, _, to string) (domain.Machine, error) {
	return domain.Machine{Name: to}, f.err
}

// Update aplica a mudança numa máquina vazia e devolve o resultado.
func (f *fakeMachines) Update(name string, change func(*domain.Egress)) (domain.Machine, error) {
	m := domain.Machine{Name: name}
	change(&m.Network.Egress)
	return m, f.err
}

// Create devolve a máquina pedida.
func (f *fakeMachines) Create(_ context.Context, m domain.Machine) (domain.Machine, error) {
	f.created = m
	return m.WithDefaults(), f.err
}

// List devolve uma máquina ligada, ou nil (para conferir a lista vazia em JSON).
func (f *fakeMachines) List() ([]app.MachineStatus, error) {
	if f.err != nil {
		return nil, f.err
	}
	if f.files == nil {
		return nil, nil
	}
	return []app.MachineStatus{{Machine: domain.Machine{Name: "dev"}, State: app.StateRunning}}, nil
}

// Status devolve a máquina pedida ligada.
func (f *fakeMachines) Status(n string) (app.MachineStatus, error) {
	return app.MachineStatus{Machine: domain.Machine{Name: n}, State: app.StateRunning, PID: 7}, f.err
}

// Start devolve a máquina ligada.
func (f *fakeMachines) Start(_ context.Context, n string) (app.MachineStatus, error) {
	return f.Status(n)
}

// Stop grava o prazo recebido.
func (f *fakeMachines) Stop(_ context.Context, _ string, d time.Duration) error {
	f.stopped = d
	return f.err
}

// Delete grava o force.
func (f *fakeMachines) Delete(_ context.Context, _ string, force bool) error {
	f.forced = force
	return f.err
}

// Exec ecoa o argv em dois pedaços de stdout e sai com 3.
func (f *fakeMachines) Exec(_ context.Context, _ string, req app.ExecRequest, out func(string, string)) (domain.Result, error) {
	f.lastExec = req
	if f.err != nil {
		return domain.Result{}, f.err
	}
	if out != nil && f.chunks != nil {
		for _, c := range f.chunks {
			out(c[0], c[1])
		}
	} else if out != nil {
		out("stdout", "a\n")
		out("stderr", "e\n")
	}
	return domain.Result{ExitCode: 3, Stdout: strings.Join(req.Argv, " "), Duration: 9 * time.Millisecond}, nil
}

// CopyIn guarda o corpo.
func (f *fakeMachines) CopyIn(_ context.Context, _, p string, r io.Reader, _ os.FileMode) (int64, error) {
	if f.err != nil {
		return 0, f.err
	}
	b, _ := io.ReadAll(r)
	f.files[p] = b
	return int64(len(b)), nil
}

// CopyOut devolve o que foi guardado; ausente é NOT_FOUND.
func (f *fakeMachines) CopyOut(_ context.Context, _, p string, w io.Writer) (int64, error) {
	b, ok := f.files[p]
	if !ok {
		return 0, domain.MachineError{Code: domain.CodeNotFound, Msg: "no such file " + p}
	}
	n, err := w.Write(b)
	return int64(n), err
}

// Logs devolve um console fixo e grava o tail.
func (f *fakeMachines) Logs(_ string, tail int, source string) (string, error) {
	f.source = source
	f.tail = tail
	return "boot ok\n", f.err
}

// machineServer monta o servidor com o serviço falso.
func machineServer(f *fakeMachines) *Server {
	s := newServer(&runner{respond: echo})
	s.Machines = f
	return s
}

// errorCode lê o código do corpo de erro.
func errorCode(t *testing.T, body string) string {
	t.Helper()
	var e apiError
	if err := json.Unmarshal([]byte(body), &e); err != nil {
		t.Fatalf("error body is not JSON: %s", body)
	}
	return e.Error.Code
}

// TestMachineRoutesHappyPath percorre as rotas com o serviço respondendo bem.
func TestMachineRoutesHappyPath(t *testing.T) {
	f := &fakeMachines{files: map[string][]byte{}}
	s := machineServer(f)
	if code, body := do(t, s, "POST", "/api/v1/machines", `{"name":"dev","stack":"python","disk_gib":3}`); code != 201 ||
		!strings.Contains(body, `"state":"stopped"`) || !strings.Contains(body, `"disk_gib":3`) {
		t.Fatalf("create: %d %s", code, body)
	}
	// runner.code no corpo: o arquivo é a base e o campo do JSON vence.
	rc := `{"name":"web","cpus":3,"runner_code":"stack = \"python\"\ncpus = 1\ncmd = [\"srv\"]\nstop_timeout = 7"}`
	if code, body := do(t, s, "POST", "/api/v1/machines", rc); code != 201 || f.created.CPUs != 3 || f.created.Stack != "python" ||
		f.created.Workload[0] != "srv" || f.created.StopTimeout != 7 {
		t.Fatalf("runner_code create: %d %s %+v", code, body, f.created)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines", `{"name":"i","image":"alpine","env":["A=1"],"init":["true"],"workload":["sleep","9"],"stop_signal":"SIGINT","stop_on_exit":true,"workdir":"/w","gpu_vram_mib":64,"gpu":true}`); code != 201 ||
		f.created.Image != "alpine" || f.created.Env[0] != "A=1" || !f.created.StopOnExit || f.created.GPUVRAMMiB != 64 || f.created.Workdir != "/w" {
		t.Fatalf("image create: %d %+v", code, f.created)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines", `{"name":"n","stack":"python","net":true,"ports":["8080:80"],"egress":{"allow_hosts":["example.com"]},"network":"lab"}`); code != 201 ||
		f.created.Network.Name != "lab" || !f.created.Network.Enabled || f.created.Network.Ports[0].Guest != 80 || f.created.Network.Egress.AllowHosts[0] != "example.com" {
		t.Fatalf("network create: %d %+v", code, f.created.Network)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/n/pause", ""); code != 204 {
		t.Fatalf("pause: %d", code)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/n/sync", ""); code != 204 {
		t.Fatalf("sync: %d", code)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/n/resume", ""); code != 204 {
		t.Fatalf("resume: %d", code)
	}
	if code, body := do(t, s, "POST", "/api/v1/machines/n/branch", `{"name":"kid"}`); code != 201 || !strings.Contains(body, `"name":"kid"`) {
		t.Fatalf("branch: %d %s", code, body)
	}
	if code, body := do(t, s, "PUT", "/api/v1/machines/n/egress", `{"allow_hosts":["a.com"]}`); code != 200 || !strings.Contains(body, `"a.com"`) {
		t.Fatalf("egress update: %d %s", code, body)
	}
	if code, body := do(t, s, "GET", "/api/v1/machines", ""); code != 200 || !strings.Contains(body, `"name":"dev"`) {
		t.Fatalf("list: %d %s", code, body)
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/dev", ""); code != 200 || !strings.Contains(body, `"pid":7`) {
		t.Fatalf("status: %d %s", code, body)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/start", ""); code != 200 {
		t.Fatalf("start: %d", code)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/stop?timeout=3", ""); code != 204 || f.stopped != 3*time.Second {
		t.Fatalf("stop: %d %v", code, f.stopped)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/stop", ""); code != 204 || f.stopped != -1 {
		t.Fatalf("stop default: %v", f.stopped)
	}
	code, body := do(t, s, "POST", "/api/v1/machines/dev/exec", `{"command":["ls","-l"],"env":["A=1"],"workdir":"/w","stdin":"x","timeout":5}`)
	if code != 200 || !strings.Contains(body, `"exitCode":3`) || !strings.Contains(body, `"stdout":"ls -l"`) {
		t.Fatalf("exec: %d %s", code, body)
	}
	if f.lastExec.Cwd != "/w" || f.lastExec.Env[0] != "A=1" || f.lastExec.Stdin != "x" || f.lastExec.Timeout != 5*time.Second {
		t.Fatalf("exec request: %+v", f.lastExec)
	}
	code, body = do(t, s, "POST", "/api/v1/machines/dev/exec/stream", `{"command":["ls"]}`)
	evs := parseSSE(t, body)
	if code != 200 || len(evs) != 3 || evs[0] != (sseEvent{"stdout", "a"}) || evs[1].name != "stderr" ||
		evs[2].name != "exit" || !strings.Contains(evs[2].data, `"exitCode":3`) {
		t.Fatalf("stream: %d %+v", code, evs)
	}
	req := httptest.NewRequest("PUT", "/api/v1/machines/dev/files?path=/w/a.bin&mode=600", bytes.NewReader([]byte("payload")))
	w := httptest.NewRecorder()
	s.Handler().ServeHTTP(w, req)
	if w.Code != 200 || string(f.files["/w/a.bin"]) != "payload" || !strings.Contains(w.Body.String(), `"bytes":7`) {
		t.Fatalf("put: %d %s", w.Code, w.Body.String())
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/dev/files?path=/w/a.bin", ""); code != 200 || body != "payload" {
		t.Fatalf("get: %d %q", code, body)
	}
	f.files["/empty"] = nil
	if code, body := do(t, s, "GET", "/api/v1/machines/dev/files?path=/empty", ""); code != 200 || body != "" {
		t.Fatalf("empty file: %d %q", code, body)
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/dev/logs?tail=5&source=workload", ""); code != 200 || body != "boot ok\n" || f.tail != 5 || f.source != "workload" {
		t.Fatalf("logs: %d %q %d", code, body, f.tail)
	}
	if code, _ := do(t, s, "DELETE", "/api/v1/machines/dev?force=true", ""); code != 204 || !f.forced {
		t.Fatalf("delete: %d forced=%v", code, f.forced)
	}
	f.files = nil
	if _, body := do(t, s, "GET", "/api/v1/machines", ""); strings.TrimSpace(body) != "[]" {
		t.Fatalf("empty list must be []: %s", body)
	}
}

// TestMachineRoutesErrors: cada código estável vira o status HTTP certo, e pedidos
// malformados são CONFIG_ERROR (400).
func TestMachineRoutesErrors(t *testing.T) {
	cases := []struct {
		err    error
		status int
		code   string
	}{
		{domain.ErrMachineNotFound("dev"), 404, domain.CodeNotFound},
		{domain.ErrMachineExists("dev"), 409, domain.CodeConflict},
		{domain.ErrMachineState("dev", "is running"), 409, domain.CodeInvalidState},
		{domain.MachineError{Code: domain.CodeConfig, Msg: "bad"}, 400, domain.CodeConfig},
		{domain.ValidationError{Err: domain.ErrUnsupportedLanguage{Name: "cobol"}}, 400, domain.CodeConfig},
		{errors.New("no hypervisor"), 500, "RUNNER_ERROR"},
	}
	for _, c := range cases {
		s := machineServer(&fakeMachines{err: c.err, files: map[string][]byte{}})
		for _, r := range [][3]string{
			{"POST", "/api/v1/machines", `{"name":"dev","stack":"python"}`},
			{"GET", "/api/v1/machines", ""},
			{"GET", "/api/v1/machines/dev", ""},
			{"DELETE", "/api/v1/machines/dev", ""},
			{"POST", "/api/v1/machines/dev/start", ""},
			{"POST", "/api/v1/machines/dev/stop", ""},
			{"POST", "/api/v1/machines/dev/exec", `{"command":["ls"]}`},
			{"POST", "/api/v1/machines/dev/exec/stream", `{"command":["ls"]}`},
			{"PUT", "/api/v1/machines/dev/files?path=/x", "data"},
			{"GET", "/api/v1/machines/dev/logs", ""},
			{"PUT", "/api/v1/machines/dev/egress", `{}`},
			{"POST", "/api/v1/machines/dev/pause", ""},
			{"POST", "/api/v1/machines/dev/resume", ""},
			{"POST", "/api/v1/machines/dev/sync", ""},
			{"POST", "/api/v1/machines/dev/branch", `{"name":"x"}`},
		} {
			code, body := do(t, s, r[0], r[1], r[2])
			if code != c.status || errorCode(t, body) != c.code {
				t.Fatalf("%s %s with %v: %d %s", r[0], r[1], c.err, code, body)
			}
		}
	}
	s := machineServer(&fakeMachines{files: map[string][]byte{}})
	for _, r := range [][3]string{
		{"POST", "/api/v1/machines", `{"stack":"python"}`},
		{"POST", "/api/v1/machines", `{"name":"x"}`},
		{"POST", "/api/v1/machines", `{"name":"x","stack":"python","ports":["nope"]}`},
		{"PUT", "/api/v1/machines/dev/egress", `{`},
		{"POST", "/api/v1/machines/dev/branch", `{}`},
		{"POST", "/api/v1/machines", `{"name":"x","runner_code":"net = true"}`},
		{"POST", "/api/v1/machines", `{"name":"x","runner_code":"stack = \"python\"\nvolumes = [\"/:/host\"]"}`},
		{"POST", "/api/v1/machines/dev/exec", `{"command":[]}`},
		{"POST", "/api/v1/machines/dev/exec/stream", `{`},
		{"POST", "/api/v1/machines/dev/stop?timeout=999", ""},
		{"POST", "/api/v1/machines/dev/stop?timeout=x", ""},
		{"PUT", "/api/v1/machines/dev/files?path=/x&mode=999", "d"},
	} {
		if code, body := do(t, s, r[0], r[1], r[2]); code != 400 || errorCode(t, body) != domain.CodeConfig {
			t.Fatalf("%s %s: %d %s", r[0], r[1], code, body)
		}
	}
	// Volume s3:// não toca o disco do servidor: passa pela API (diretório do host não, acima).
	if code, body := do(t, s, "POST", "/api/v1/machines", `{"name":"s","runner_code":"stack = \"python\"\nnet = true\nvolumes = [\"s3://bkt/x:/data:ro\"]"}`); code != 201 {
		t.Fatalf("s3 volume over the API: %d %s", code, body)
	}
	if code, body := do(t, s, "POST", "/api/v1/machines", `{"name":"cr","stack":"python","credentials":[{"name":"gh","environment_variable":"GH","allowed_hosts":["api.github.com"]}]}`); code != 201 {
		t.Fatalf("credentials over the API: %d %s", code, body)
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/dev/files?path=/missing", ""); code != 404 || errorCode(t, body) != domain.CodeNotFound {
		t.Fatalf("missing file: %d %s", code, body)
	}
	if code, _ := do(t, newServer(&runner{respond: echo}), "GET", "/api/v1/machines", ""); code != 404 {
		t.Fatal("without a machine service the routes must not exist")
	}
}

// failAfterFirst entrega um pedaço de saída e depois falha: erro no meio do streaming.
type failAfterFirst struct{ fakeMachines }

// Exec manda um pedaço e devolve erro.
func (f *failAfterFirst) Exec(_ context.Context, _ string, _ app.ExecRequest, out func(string, string)) (domain.Result, error) {
	out("stdout", "partial")
	return domain.Result{}, errors.New("guest connection reset")
}

// TestMachineStreamErrorAfterStart: depois do primeiro evento o erro vira `event: error`.
func TestMachineStreamErrorAfterStart(t *testing.T) {
	s := newServer(&runner{respond: echo})
	s.Machines = &failAfterFirst{fakeMachines{files: map[string][]byte{}}}
	code, body := do(t, s, "POST", "/api/v1/machines/dev/exec/stream", `{"command":["ls"]}`)
	evs := parseSSE(t, body)
	if code != 200 || len(evs) != 2 || evs[1].name != "error" || !strings.Contains(evs[1].data, "connection reset") {
		t.Fatalf("%d %+v", code, evs)
	}
}

// TestAlternateBodyShapes: as formas alternativas do corpo funcionam: network booleano (o
// mesmo que --net), memoryMb, cmd, o caminho do arquivo no URL (%2F ou não) e o código do erro
// no topo do corpo. "network" em texto continua sendo a rede nomeada.
func TestAlternateBodyShapes(t *testing.T) {
	f := &fakeMachines{files: map[string][]byte{}}
	s := machineServer(f)
	if code, body := do(t, s, "POST", "/api/v1/machines", `{"name":"sv","image":"python:3.12-alpine","network":true,"memoryMb":2048,"cmd":["sh","-c","sleep 1"]}`); code != 201 ||
		!f.created.Network.Enabled || f.created.Network.Name != "" || f.created.MemoryMiB != 2048 || strings.Join(f.created.Workload, " ") != "sh -c sleep 1" {
		t.Fatalf("alternate create: %d %s %+v", code, body, f.created)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines", `{"name":"nn","stack":"python","network":"lab"}`); code != 201 || f.created.Network.Name != "lab" || f.created.Network.Enabled {
		t.Fatalf("named network: %+v", f.created.Network)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines", `{"name":"bad","stack":"python","network":3}`); code != 400 {
		t.Fatal("a network that is neither text nor boolean must be refused")
	}
	if code, _ := do(t, s, "PUT", "/api/v1/machines/dev/files/%2Ftmp%2Fabs.txt", "PAYLOAD123"); code != 200 || string(f.files["/tmp/abs.txt"]) != "PAYLOAD123" {
		t.Fatalf("PUT files/%%2F...: %d %v", code, f.files)
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/dev/files/tmp/abs.txt", ""); code != 200 || body != "PAYLOAD123" {
		t.Fatalf("GET files/<path>: %d %q", code, body)
	}
	_, body := do(t, s, "GET", "/api/v1/machines/dev/files/%2Fnope", "")
	var top struct{ Code string }
	json.Unmarshal([]byte(body), &top)
	if top.Code != domain.CodeNotFound || errorCode(t, body) != domain.CodeNotFound {
		t.Fatalf("the code must be at the top and inside error: %s", body)
	}
}

// TestStreamLines: o streaming sai uma linha por evento, texto cru: pedaço
// que corta a linha espera o resto, pedaço com várias linhas vira vários eventos, e o resto
// sem \n sai antes do exit.
func TestStreamLines(t *testing.T) {
	f := &fakeMachines{files: map[string][]byte{}, chunks: [][2]string{
		{"stdout", "li"}, {"stdout", "ne1\nline2\nli"}, {"stderr", "aviso\n"}, {"stdout", "ne3"},
	}}
	_, body := do(t, machineServer(f), "POST", "/api/v1/machines/dev/exec/stream", `{"command":["x"]}`)
	evs := parseSSE(t, body)
	var got []string
	for _, e := range evs {
		got = append(got, e.name+"="+e.data)
	}
	want := "stdout=line1 stdout=line2 stderr=aviso stdout=line3"
	if strings.Join(got[:len(got)-1], " ") != want || evs[len(evs)-1].name != "exit" {
		t.Fatalf("lines: %v", got)
	}
}

// TestStrictBodies: campo desconhecido é recusado com o nome dele, em toda rota de máquina com
// corpo. Antes era ignorado: um `allowedHost` errado criava a máquina sem a restrição pedida.
func TestStrictBodies(t *testing.T) {
	f := &fakeMachines{files: map[string][]byte{}}
	s := machineServer(f)
	for _, c := range []struct{ method, path, body string }{
		{"POST", "/api/v1/machines", `{"name":"x","stack":"python","allowedHost":["a.com"]}`},
		{"POST", "/api/v1/machines/dev/exec", `{"command":["true"],"cwd":"/x"}`},
		{"POST", "/api/v1/machines/dev/exec/stream", `{"command":["true"],"cwd":"/x"}`},
		{"PUT", "/api/v1/machines/dev/egress", `{"allowHosts":["a.com"]}`},
		{"POST", "/api/v1/machines/dev/branch", `{"name":"b","from":"x"}`},
	} {
		code, body := do(t, s, c.method, c.path, c.body)
		if code != 400 || !strings.Contains(body, "unknown field") || errorCode(t, body) != domain.CodeConfig {
			t.Errorf("%s %s: %d %s", c.method, c.path, code, body)
		}
	}
	// O corpo certo segue passando (prova de que a recusa não é de tudo).
	if code, body := do(t, s, "POST", "/api/v1/machines/dev/branch", `{"name":"b"}`); code != 201 {
		t.Fatalf("branch: %d %s", code, body)
	}
}

// TestEgressAliases: allowedHosts e allowedCidrs (nomes alternativos) viram a política de saída e
// ligam a rede, somando-se ao egress do rc.
func TestEgressAliases(t *testing.T) {
	f := &fakeMachines{files: map[string][]byte{}}
	s := machineServer(f)
	code, body := do(t, s, "POST", "/api/v1/machines", `{"name":"eg","stack":"python","allowedHosts":["api.example.com"],"allowedCidrs":["10.0.0.0/8"],"egress":{"allow_hosts":["b.example.com"]}}`)
	n := f.created.Network
	if code != 201 || !n.Enabled || strings.Join(n.Egress.AllowHosts, ",") != "b.example.com,api.example.com" || strings.Join(n.Egress.AllowCIDRs, ",") != "10.0.0.0/8" {
		t.Fatalf("aliases: %d %s %+v", code, body, n)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines", `{"name":"off","stack":"python"}`); code != 201 || f.created.Network.Enabled {
		t.Fatal("without aliases the network stays off")
	}
}

// TestNestedOverAPI: nested (e o nome alternativo nestedVirt) é recusado por padrão e passa só com o
// servidor em --allow-nested-virt.
func TestNestedOverAPI(t *testing.T) {
	f := &fakeMachines{files: map[string][]byte{}}
	s := machineServer(f)
	for _, k := range []string{"nested", "nestedVirt"} {
		code, body := do(t, s, "POST", "/api/v1/machines", `{"name":"n","stack":"python","`+k+`":true}`)
		if code != 400 || !strings.Contains(body, "--allow-nested-virt") || f.created.Nested {
			t.Fatalf("%s refused by default: %d %s", k, code, body)
		}
	}
	s.AllowNested = true
	for _, k := range []string{"nested", "nestedVirt"} {
		f.created = domain.Machine{}
		if code, body := do(t, s, "POST", "/api/v1/machines", `{"name":"n","stack":"python","`+k+`":true}`); code != 201 || !f.created.Nested {
			t.Fatalf("%s with the server allowing it: %d %s", k, code, body)
		}
	}
}
