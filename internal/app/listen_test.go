package app

import (
	"context"
	"errors"
	"io"
	"reflect"
	"testing"

	"runner.code/internal/domain"
)

// TestListeningPortsAndDial: com a máquina parada, os dois recusam; ligada, a porta em LISTEN do guest aparece e o
// DialPort chega até ela pelo guest; porta fechada no guest volta o erro.
func TestListeningPortsAndDial(t *testing.T) {
	m, _, _, _, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "a", Stack: "python"})
	if _, err := m.ListeningPorts(ctx, "a"); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("parada, ListeningPorts: %v", err)
	}
	if _, err := m.DialPort(ctx, "a", 8000); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("parada, DialPort: %v", err)
	}
	m.Start(ctx, "a")
	ports, err := m.ListeningPorts(ctx, "a")
	if err != nil || !reflect.DeepEqual(ports, []uint16{8000}) {
		t.Fatalf("portas %v %v", ports, err)
	}
	conn, err := m.DialPort(ctx, "a", 8000)
	if err != nil {
		t.Fatal(err)
	}
	got, _ := io.ReadAll(conn)
	conn.Close()
	if string(got) != "porta 8000" || g.dialed[len(g.dialed)-1] != 8000 {
		t.Fatalf("dial: %q %v", got, g.dialed)
	}
	g.dialErr = errors.New("connection refused")
	if _, err := m.DialPort(ctx, "a", 9999); err == nil {
		t.Fatal("porta fechada no guest não deu erro")
	}
	g.execErr = errors.New("guest caiu")
	if _, err := m.ListeningPorts(ctx, "a"); err == nil {
		t.Fatal("exec que falha não deu erro")
	}
}
