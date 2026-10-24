package httpapi

import (
	"bytes"
	"encoding/json"
	"image"
	"image/jpeg"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"

	"runner.codes/internal/adapters/screenctl"
	"runner.codes/internal/adapters/viewer"
	"runner.codes/internal/domain"
)

// screenViewer imita o visualizador da tela: GET /frame devolve um JPEG de 200x100 (ou o status forçado) e
// POST /input guarda os pacotes.
type screenViewer struct {
	mu      sync.Mutex
	status  int
	packets [][]viewer.Event
}

// ServeHTTP atende as duas rotas do visualizador.
func (v *screenViewer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	v.mu.Lock()
	defer v.mu.Unlock()
	if v.status != 0 {
		w.WriteHeader(v.status)
		return
	}
	if r.URL.Path == "/frame" {
		var buf bytes.Buffer
		jpeg.Encode(&buf, image.NewRGBA(image.Rect(0, 0, 200, 100)), nil)
		w.Write(buf.Bytes())
		return
	}
	var evs []viewer.Event
	json.NewDecoder(r.Body).Decode(&evs)
	v.packets = append(v.packets, evs)
	w.WriteHeader(http.StatusNoContent)
}

// screenServer monta o servidor da API com o serviço falso e o controle de tela apontado para o visualizador falso.
func screenServer(t *testing.T) (*Server, *fakeMachines, *screenViewer) {
	v := &screenViewer{}
	srv := httptest.NewServer(v)
	t.Cleanup(srv.Close)
	f := &fakeMachines{}
	s := machineServer(f)
	s.ScreenFor = func(port uint16) screenctl.Screen {
		if port != 6080 {
			t.Errorf("porta %d", port)
		}
		return screenctl.Screen{Base: srv.URL}
	}
	return s, f, v
}

// TestScreenshot: a imagem volta em JPEG com o tamanho nos cabeçalhos; máquina sem tela é 400 (CONFIG_ERROR), visualizador
// fora do ar é 502 (SCREEN_UNAVAILABLE).
func TestScreenshot(t *testing.T) {
	s, f, v := screenServer(t)
	req := httptest.NewRequest("GET", "/api/v1/machines/desk/screen", nil)
	w := httptest.NewRecorder()
	s.Handler().ServeHTTP(w, req)
	if w.Code != 200 || w.Header().Get("Content-Type") != "image/jpeg" || w.Header().Get("X-Screen-Width") != "200" ||
		w.Header().Get("X-Screen-Height") != "100" || f.displayName != "desk" {
		t.Fatalf("%d %v", w.Code, w.Header())
	}
	if _, err := jpeg.Decode(w.Body); err != nil {
		t.Fatal(err)
	}
	v.status = 500
	if code, body := do(t, s, "GET", "/api/v1/machines/desk/screen", ""); code != 502 || errorCode(t, body) != "SCREEN_UNAVAILABLE" {
		t.Fatalf("visualizador: %d %s", code, body)
	}
	f.err = domain.MachineError{Code: domain.CodeConfig, Msg: "machine desk has no display"}
	if code, body := do(t, s, "GET", "/api/v1/machines/desk/screen", ""); code != 400 || errorCode(t, body) != domain.CodeConfig {
		t.Fatalf("sem tela: %d %s", code, body)
	}
}

// TestScreenActions: as ações viram pacotes no visualizador e a resposta é 204; ação inválida é 400 sem nenhum
// pacote; corpo com campo desconhecido, lote grande demais e máquina desligada são recusados antes do visualizador.
func TestScreenActions(t *testing.T) {
	s, f, v := screenServer(t)
	code, body := do(t, s, "POST", "/api/v1/machines/desk/screen/actions",
		`{"actions":[{"action":"click","x":199,"y":99},{"action":"type","text":"ok"},{"action":"key","keys":"Return"}]}`)
	if code != 204 || len(v.packets) != 3+4+2 || v.packets[0][0].Value != 32767 {
		t.Fatalf("%d %s pacotes=%d", code, body, len(v.packets))
	}
	v.packets = nil
	for name, req := range map[string]string{
		"ação inválida":      `{"actions":[{"action":"type","text":"ok"},{"action":"fly"}]}`,
		"fora da tela":       `{"actions":[{"action":"click","x":500,"y":1}]}`,
		"campo desconhecido": `{"actions":[{"action":"click","x":1,"y":1,"botton":"left"}]}`,
		"lista vazia":        `{"actions":[]}`,
		"lote grande demais": `{"actions":[` + strings.Repeat(`{"action":"wait"},`, maxScreenActions) + `{"action":"wait"}]}`,
	} {
		if code, body := do(t, s, "POST", "/api/v1/machines/desk/screen/actions", req); code != 400 || errorCode(t, body) != domain.CodeConfig {
			t.Fatalf("%s: %d %s", name, code, body)
		}
	}
	if len(v.packets) != 0 {
		t.Fatalf("pedido recusado mandou %d pacotes", len(v.packets))
	}
	v.status = 503
	if code, body := do(t, s, "POST", "/api/v1/machines/desk/screen/actions", `{"actions":[{"action":"key","keys":"a"}]}`); code != 502 {
		t.Fatalf("visualizador: %d %s", code, body)
	}
	f.err = domain.ErrMachineState("desk", "is not running; start it first")
	if code, body := do(t, s, "POST", "/api/v1/machines/desk/screen/actions", `{"actions":[{"action":"key","keys":"a"}]}`); code != 409 || errorCode(t, body) != "INVALID_STATE" {
		t.Fatalf("desligada: %d %s", code, body)
	}
}

// TestScreenDefault: sem ScreenFor, o controle aponta para o visualizador no loopback, na porta da máquina.
func TestScreenDefault(t *testing.T) {
	if got := (&Server{}).screen(6123).Base; got != "http://127.0.0.1:6123" {
		t.Fatal(got)
	}
}
