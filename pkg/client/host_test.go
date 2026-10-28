package client

import (
	"context"
	"net/http"
	"strings"
	"testing"
	"time"
)

// TestHost: decodifica a medição do host; erro da API sobe.
func TestHost(t *testing.T) {
	c, _ := newFake(t, map[string]func(http.ResponseWriter){
		"GET /api/v1/host": reply(200, `{"mem_total_mib":7859,"mem_available_mib":4096,"disk_total_mib":50000,"disk_free_mib":36000,"kvm":true,"machines":2,"running":1}`),
	})
	h, err := c.Host(context.Background())
	if err != nil || h.MemAvailableMiB != 4096 || h.DiskFreeMiB != 36000 || !h.KVM || h.Running != 1 || h.Machines != 2 {
		t.Fatalf("host: %+v %v", h, err)
	}
	if _, err := New("http://127.0.0.1:1").Host(context.Background()); err == nil {
		t.Fatal("servidor fora sem erro")
	}
}

// TestHostPreemptionAndRelease: o host em preempção traz quem estava ligada; ReleaseMachine chama a rota de soltar
// (204 é sucesso; o 501 do host sem disco próprio sobe como erro).
func TestHostPreemptionAndRelease(t *testing.T) {
	c, _ := newFake(t, map[string]func(http.ResponseWriter){
		"GET /api/v1/host":                    reply(200, `{"persistent_disks":true,"preempting":true,"drained":["a"]}`),
		"POST /api/v1/machines/dev/release":   reply(204, ``),
		"POST /api/v1/machines/local/release": reply(501, `{"error":"persistent disks are not enabled on this host"}`),
		"POST /api/v1/machines/dev/adopt":     reply(200, `{"name":"dev","state":"stopped"}`),
	})
	h, err := c.Host(context.Background())
	if err != nil || !h.PersistentDisks || !h.Preempting || len(h.Drained) != 1 || h.Drained[0] != "a" {
		t.Fatalf("host: %+v %v", h, err)
	}
	if err := c.ReleaseMachine(context.Background(), "dev"); err != nil {
		t.Fatalf("soltar: %v", err)
	}
	if err := c.ReleaseMachine(context.Background(), "local"); err == nil {
		t.Fatal("501 sem erro")
	}
	if m, err := c.AdoptMachine(context.Background(), "dev"); err != nil || m.Name != "dev" {
		t.Fatalf("adotar: %+v %v", m, err)
	}
}

// TestSuspendMachine: o cliente pede o suspend com e sem prazo (?timeout em segundos), e o 501 do host sem suporte
// vira erro.
func TestSuspendMachine(t *testing.T) {
	c, f := newFake(t, map[string]func(http.ResponseWriter){
		"POST /api/v1/machines/desk/suspend": reply(204, ``),
		"POST /api/v1/machines/old/suspend":  reply(501, `{"error":"suspend is not available on this host"}`),
	})
	ctx := context.Background()
	if err := c.SuspendMachine(ctx, "desk", 15*time.Second); err != nil {
		t.Fatalf("com prazo: %v", err)
	}
	if err := c.SuspendMachine(ctx, "desk", 0); err != nil {
		t.Fatalf("sem prazo: %v", err)
	}
	if len(f.calls) != 2 || f.calls[0].uri != "/api/v1/machines/desk/suspend?timeout=15" || f.calls[1].uri != "/api/v1/machines/desk/suspend" {
		t.Fatalf("chamadas: %+v", f.calls)
	}
	if err := c.SuspendMachine(ctx, "old", 0); err == nil {
		t.Fatal("501 sem erro")
	}
}

// TestGolden: o create com Golden vai com ?golden= na URL (o corpo é a máquina, sem o campo), e o PublishGolden manda o
// nome do snapshot; o 501 do host sem o recurso vira erro.
func TestGolden(t *testing.T) {
	c, f := newFake(t, map[string]func(http.ResponseWriter){
		"POST /api/v1/machines":             reply(201, `{"name":"desk"}`),
		"POST /api/v1/machines/mold/golden": reply(204, ``),
		"POST /api/v1/machines/old/golden":  reply(501, `{"error":"golden snapshots are not available on this host"}`),
	})
	ctx := context.Background()
	if m, err := c.CreateMachine(ctx, MachineSpec{Name: "desk", Golden: "sbot-golden-trial-abc"}); err != nil || m.Name != "desk" {
		t.Fatalf("create do dourado: %+v %v", m, err)
	}
	if f.calls[0].uri != "/api/v1/machines?golden=sbot-golden-trial-abc" || strings.Contains(f.calls[0].body, "golden") {
		t.Fatalf("pedido: %+v", f.calls[0])
	}
	if _, err := c.CreateMachine(ctx, MachineSpec{Name: "desk"}); err != nil || f.calls[1].uri != "/api/v1/machines" {
		t.Fatalf("create normal: %v %+v", err, f.calls[1])
	}
	if err := c.PublishGolden(ctx, "mold", "sbot-golden-desk-abc"); err != nil || !strings.Contains(f.calls[2].body, `"snapshot":"sbot-golden-desk-abc"`) {
		t.Fatalf("publicar: %v %+v", err, f.calls[2])
	}
	if err := c.PublishGolden(ctx, "old", "x"); err == nil {
		t.Fatal("501 sem erro")
	}
}
