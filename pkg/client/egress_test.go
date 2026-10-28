package client

import (
	"context"
	"net/http"
	"testing"
)

// TestEgressEvents: decodifica a página do registro (campos opcionais nulos viram nil) e o 501 do host sem proxy sobe
// como erro.
func TestEgressEvents(t *testing.T) {
	c, _ := newFake(t, map[string]func(http.ResponseWriter){
		"GET /api/v1/egress/events": reply(200, `{"epoch":9,"next":2,"lost":true,"events":[
			{"seq":2,"atMs":1700000000000,"machine":"sbot-a","kind":"https","verdict":"allowed","host":"github.com","port":443,
			 "method":"GET","target":"/x?code=…","status":200,"bytesUp":0,"bytesDown":10,"durationMs":3,"note":null}]}`),
	})
	p, err := c.EgressEvents(context.Background(), 1, 50)
	if err != nil || p.Epoch != 9 || p.Next != 2 || !p.Lost || len(p.Events) != 1 {
		t.Fatalf("page: %+v %v", p, err)
	}
	e := p.Events[0]
	if e.Host != "github.com" || *e.Method != "GET" || *e.Status != 200 || e.Note != nil || e.AtMs != 1700000000000 {
		t.Fatalf("event: %+v", e)
	}
	c, _ = newFake(t, map[string]func(http.ResponseWriter){
		"GET /api/v1/egress/events": reply(501, `{"error":"this host has no egress proxy (RC_EGRESS_API)"}`),
	})
	if _, err := c.EgressEvents(context.Background(), 0, 0); err == nil {
		t.Fatal("501 must be an error")
	}
}
