package viewer

import (
	"encoding/json"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/adapters/screen"
)

// kinds devolve os tipos dos eventos, na ordem.
func kinds(evs []PointerEvent) string {
	var k []string
	for _, e := range evs {
		k = append(k, e.Kind)
	}
	return strings.Join(k, ",")
}

// TestPointerFeed: posição acumula até o SYN e vira move (escalada de 0 a 1, presa nos limites); botão apertado é
// click, tecla apertada é key, roda é scroll; soltar não conta; lote sem SYN no fim ainda publica o move.
func TestPointerFeed(t *testing.T) {
	f := newPointerFeed()
	f.publish([]Event{{evAbsType, absXCode, 32767}, {evAbsType, absYCode, 0}, {evSynType, 0, 0},
		{evKeyType, 0x110, 1}, {evSynType, 0, 0}, {evKeyType, 0x110, 0}, {evSynType, 0, 0},
		{evKeyType, 30, 1}, {evKeyType, 30, 0}, {evRelType, relWheel, -1}})
	evs, seq := f.since(0, time.Second)
	if kinds(evs) != "move,click,key,scroll" || seq != 4 || evs[0].X != 1 || evs[0].Y != 0 || evs[1].X != 1 {
		t.Fatalf("%s %d %+v", kinds(evs), seq, evs)
	}
	f.publish([]Event{{evAbsType, absXCode, 99999}, {evAbsType, absYCode, -5}})
	evs, _ = f.since(4, time.Second)
	if kinds(evs) != "move" || evs[0].X != 1 || evs[0].Y != 0 {
		t.Fatalf("sem SYN e fora do eixo: %+v", evs)
	}
	if evs, seq := f.since(5, 20*time.Millisecond); evs != nil || seq != 5 {
		t.Fatalf("sem novidade: %+v %d", evs, seq)
	}
}

// TestPointerFeedWakesAndTrims: quem espera acorda com o evento novo; o feed guarda só os feedKeep mais recentes.
func TestPointerFeedWakesAndTrims(t *testing.T) {
	f := newPointerFeed()
	go func() {
		time.Sleep(20 * time.Millisecond)
		f.publish([]Event{{evKeyType, 0x111, 1}})
	}()
	if evs, seq := f.since(0, 5*time.Second); len(evs) != 1 || evs[0].Kind != "click" || seq != 1 {
		t.Fatalf("não acordou: %+v %d", evs, seq)
	}
	for i := 0; i < feedKeep+10; i++ {
		f.publish([]Event{{evKeyType, 30, 1}})
	}
	if evs, _ := f.since(0, time.Second); len(evs) != feedKeep || evs[len(evs)-1].Seq != uint64(feedKeep+11) {
		t.Fatalf("guardou %d", len(evs))
	}
}

// TestPointerRoute: só o POST /input marcado como de agente chega ao /pointer; o da pessoa passa ao guest e não
// desenha seta.
func TestPointerRoute(t *testing.T) {
	in := &fakeInput{}
	h := Handler(screen.New(), in, nil, 100, 100)
	body := `[{"type":3,"code":0,"value":16384},{"type":3,"code":1,"value":16384},{"type":0,"code":0,"value":0}]`
	do(h, "POST", "/input", body) // a pessoa
	req := httptest.NewRequest("POST", "/input", strings.NewReader(`[{"type":1,"code":272,"value":1}]`))
	req.Header.Set(AgentHeader, "1")
	rec := httptest.NewRecorder()
	h.ServeHTTP(rec, req)
	if rec.Code != 204 || len(in.evs) != 4 {
		t.Fatalf("input: %d %d", rec.Code, len(in.evs))
	}
	r := do(h, "GET", "/pointer?after=0", "")
	var got struct {
		Seq    uint64
		Events []PointerEvent
	}
	if err := json.Unmarshal(r.Body.Bytes(), &got); err != nil || got.Seq != 1 || kinds(got.Events) != "click" ||
		r.Header().Get("Content-Type") != "application/json" {
		t.Fatalf("pointer: %s %v", r.Body, err)
	}
	if !strings.Contains(do(h, "GET", "/", "").Body.String(), `fetch("pointer?after="`) {
		t.Fatal("a página não lê o /pointer")
	}
}

// TestClamp01 cobre os três lados.
func TestClamp01(t *testing.T) {
	if clamp01(-1) != 0 || clamp01(2) != 1 || clamp01(0.25) != 0.25 {
		t.Fatal("clamp01")
	}
}
