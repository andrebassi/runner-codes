package httpapi

import (
	"context"
	"strings"
	"testing"

	"runner.codes/internal/domain"
)

// androidMachines é o fakeMachines com as operações de Android.
type androidMachines struct {
	*fakeMachines
	ui, png []byte
	input   domain.AndroidInput
	shell   string
	err     error
}

// AndroidUI devolve o XML configurado.
func (a *androidMachines) AndroidUI(context.Context, string) ([]byte, error) { return a.ui, a.err }

// AndroidInput guarda a ação recebida.
func (a *androidMachines) AndroidInput(_ context.Context, _ string, in domain.AndroidInput) error {
	a.input = in
	return a.err
}

// AndroidScreenshot devolve o PNG configurado.
func (a *androidMachines) AndroidScreenshot(context.Context, string) ([]byte, error) {
	return a.png, a.err
}

// AndroidShell guarda a linha recebida e devolve uma saída fixa.
func (a *androidMachines) AndroidShell(_ context.Context, _, cmd string) (string, error) {
	a.shell = cmd
	return "uid=0(root)\n", a.err
}

// TestAndroidRoutes trava as três rotas: o XML da árvore, a ação em JSON (204), o PNG, os erros de máquina com o
// status do código estável e o 501 num servidor sem Android.
func TestAndroidRoutes(t *testing.T) {
	a := &androidMachines{fakeMachines: &fakeMachines{files: map[string][]byte{}}, ui: []byte("<hierarchy/>"), png: []byte("\x89PNG")}
	s := &Server{Machines: a}
	if code, body := do(t, s, "GET", "/api/v1/machines/phone/android/ui", ""); code != 200 || body != "<hierarchy/>" {
		t.Fatalf("ui: %d %q", code, body)
	}
	if code, body := do(t, s, "POST", "/api/v1/machines/phone/android/input", `{"kind":"tap","x":3,"y":4}`); code != 204 || a.input.Kind != "tap" || a.input.X != 3 {
		t.Fatalf("input: %d %q %+v", code, body, a.input)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/phone/android/input", `{"kind":"tap","bogus":1}`); code != 400 {
		t.Fatalf("campo desconhecido: %d", code)
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/phone/android/screenshot", ""); code != 200 || body != "\x89PNG" {
		t.Fatalf("screenshot: %d %q", code, body)
	}
	if code, body := do(t, s, "POST", "/api/v1/machines/phone/android/shell", `{"command":"id"}`); code != 200 || a.shell != "id" || body != `{"output":"uid=0(root)\n"}` {
		t.Fatalf("shell: %d %q (%q)", code, body, a.shell)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/phone/android/shell", `{"cmd":"id"}`); code != 400 {
		t.Fatalf("shell campo errado: %d", code)
	}
	a.err = domain.ErrMachineState("phone", "is not running")
	if code, _ := do(t, s, "POST", "/api/v1/machines/phone/android/shell", `{"command":"id"}`); code != 409 {
		t.Errorf("shell parada: %d", code)
	}
	for _, path := range []string{"/android/ui", "/android/screenshot"} {
		if code, _ := do(t, s, "GET", "/api/v1/machines/phone"+path, ""); code != 409 {
			t.Errorf("%s parada: %d", path, code)
		}
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/phone/android/input", `{"kind":"key","key":"HOME"}`); code != 409 {
		t.Errorf("input parada: %d", code)
	}
	plain := &Server{Machines: &fakeMachines{files: map[string][]byte{}}}
	for _, r := range [][2]string{{"GET", "/android/ui"}, {"POST", "/android/input"}, {"GET", "/android/screenshot"}, {"POST", "/android/shell"}} {
		if code, _ := do(t, plain, r[0], "/api/v1/machines/phone"+r[1], `{}`); code != 501 {
			t.Errorf("%s sem android: %d", r[1], code)
		}
	}
}

// androidHost é o fakeMachines que diz se liga Android.
type androidHost struct {
	*fakeMachines
	ready bool
}

// AndroidReady devolve o plantado.
func (a androidHost) AndroidReady() bool { return a.ready }

// TestHostAndroid: o host diz android=true só com o serviço que liga Android; quem agenda manda Android só para ele.
func TestHostAndroid(t *testing.T) {
	measured := func() (domain.Host, error) { return domain.Host{KVM: true}, nil }
	for _, c := range []struct {
		m    MachineService
		want string
	}{
		{androidHost{&fakeMachines{files: map[string][]byte{}}, true}, `"android":true`},
		{androidHost{&fakeMachines{files: map[string][]byte{}}, false}, `"android":false`},
		{&fakeMachines{files: map[string][]byte{}}, `"android":false`},
	} {
		if code, body := do(t, &Server{Host: measured, Machines: c.m}, "GET", "/api/v1/host", ""); code != 200 || !strings.Contains(body, c.want) {
			t.Errorf("host: %d %s, queria %s", code, body, c.want)
		}
	}
}

// TestCreateMachineAndroid: o corpo com android vira máquina Android com a tela pedida; tela fora dos limites é 400 de
// config, como o --android da CLI.
func TestCreateMachineAndroid(t *testing.T) {
	m, err := CreateMachineRequest{Name: "phone", Image: "redroid/redroid:14", Android: &domain.Android{Width: 720, Height: 1280, DPI: 320}}.machine()
	if err != nil || !m.IsAndroid() || m.Android.Width != 720 || m.Android.DPI != 320 {
		t.Fatalf("android: %+v %v", m.Android, err)
	}
	if _, err := (CreateMachineRequest{Name: "phone", Image: "redroid/redroid:14", Android: &domain.Android{Width: 10, Height: 10, DPI: 320}}).machine(); err == nil ||
		!strings.Contains(err.Error(), "android screen") {
		t.Fatalf("tela pequena aceita: %v", err)
	}
	if m, err := (CreateMachineRequest{Name: "dev", Image: "debian"}).machine(); err != nil || m.IsAndroid() {
		t.Fatalf("sem android virou Android: %+v %v", m.Android, err)
	}
	// Pela rota: o corpo com android cria a máquina com a tela.
	f := &fakeMachines{files: map[string][]byte{}}
	if code, body := do(t, machineServer(f), "POST", "/api/v1/machines",
		`{"name":"phone","image":"redroid/redroid:14","android":{"width":720,"height":1280,"dpi":320}}`); code != 201 {
		t.Fatalf("criar android: %d %s", code, body)
	}
}
