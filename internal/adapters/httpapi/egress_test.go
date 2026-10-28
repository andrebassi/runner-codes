package httpapi

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

// TestEgressEvents: a rota repassa só o cursor e o tamanho (validados) à API do proxy de saída e devolve a resposta
// como veio; host sem proxy responde 501, parâmetro que não é inteiro é 400, e proxy fora do ar é 502.
func TestEgressEvents(t *testing.T) {
	var asked string
	proxy := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		asked = r.URL.String()
		w.Header().Set("Content-Type", "application/json")
		w.Write([]byte(`{"epoch":1,"next":7,"lost":false,"events":[]}`))
	}))
	defer proxy.Close()
	s := &Server{EgressAPI: proxy.URL + "/"}
	code, body := do(t, s, "GET", "/api/v1/egress/events?after=5&limit=10&x=../../etc", "")
	if code != 200 || !strings.Contains(body, `"next":7`) {
		t.Fatalf("events: %d %s", code, body)
	}
	if asked != "/events?after=5&limit=10" {
		t.Fatalf("the proxy got %q", asked)
	}
	if code, _ := do(t, s, "GET", "/api/v1/egress/events", ""); code != 200 || asked != "/events?" {
		t.Fatalf("no cursor: %d %q", code, asked)
	}
	if code, body := do(t, s, "GET", "/api/v1/egress/events?after=-1", ""); code != 400 || !strings.Contains(body, "after must be") {
		t.Fatalf("bad cursor: %d %s", code, body)
	}
	if code, _ := do(t, &Server{}, "GET", "/api/v1/egress/events", ""); code != 501 {
		t.Fatalf("no proxy: %d", code)
	}
	proxy.Close()
	if code, body := do(t, s, "GET", "/api/v1/egress/events", ""); code != 502 || !strings.Contains(body, "egress proxy") {
		t.Fatalf("proxy down: %d %s", code, body)
	}
	if code, _ := do(t, &Server{EgressAPI: "http://bad host"}, "GET", "/api/v1/egress/events", ""); code != 500 {
		t.Fatalf("bad base URL: %d", code)
	}
}
