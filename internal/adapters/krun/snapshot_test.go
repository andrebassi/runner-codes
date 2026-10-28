package krun

import (
	"os"
	"strings"
	"testing"
	"time"
)

// snapAPI é o fakeAPI com a parte de snapshot da libkrun (o fork do superbot): grava cada pedido de snapshot e de
// restore, e devolve restoreRC no restore.
type snapAPI struct {
	fakeAPI
	snapshots chan string
	restored  string
	restoreRC int32
}

// SnapshotRequest anota o diretório pedido.
func (f *snapAPI) SnapshotRequest(_ uint32, path string, _ uint32) int32 {
	f.snapshots <- path
	return 0
}

// Restore anota o diretório e devolve o rc plantado.
func (f *snapAPI) Restore(_ uint32, path string, _ uint32) int32 {
	f.restored = path
	return f.restoreRC
}

// vmmSpec é a spec mínima de uma VM de máquina (sem sandbox: tem rede).
func vmmSpec() Spec {
	return Spec{CPUs: 1, MemoryMiB: 64, RootDir: "/r", Exec: "/x", Network: true}
}

// TestRunVMMRestore: com RestoreFrom, a VM volta pelo krun_restore e nunca chama o krun_start_enter (um boot do zero
// por cima do snapshot apagaria o estado). Como o krun_start_enter, o krun_restore não volta no sucesso (a libkrun
// encerra o processo quando a VM desliga): voltar é erro, com o nome da chamada. Libkrun sem snapshot recusa com a
// explicação em vez de dar boot calado.
func TestRunVMMRestore(t *testing.T) {
	s := vmmSpec()
	s.RestoreFrom = "/m/snapshot"
	f := &snapAPI{snapshots: make(chan string, 1)}
	if err := RunVMM(f, s, nil); err == nil || !strings.Contains(err.Error(), "krun_restore") {
		t.Fatalf("restore que volta tem de ser erro: %v", err)
	}
	if f.restored != "/m/snapshot" || strings.Contains(strings.Join(f.calls, ","), "krun_start_enter") {
		t.Fatalf("restore %q, chamadas %v", f.restored, f.calls)
	}
	f = &snapAPI{snapshots: make(chan string, 1), restoreRC: -22}
	if err := RunVMM(f, s, nil); err == nil || !strings.Contains(err.Error(), "krun_restore") {
		t.Fatalf("restore falhando: %v", err)
	}
	if err := RunVMM(&fakeAPI{}, s, nil); err == nil || !strings.Contains(err.Error(), "cannot restore") {
		t.Fatalf("libkrun sem snapshot: %v", err)
	}
}

// TestRunVMMSuspendSignal: com SnapshotDir, o sinal de suspender vira um krun_snapshot_request para aquele diretório,
// feito por outra goroutine enquanto a principal segue no start. Sem SnapshotDir, ou numa libkrun sem snapshot, nada
// é armado (o sinal não teria quem atender).
func TestRunVMMSuspendSignal(t *testing.T) {
	var armed chan<- os.Signal
	old := notifySuspend
	notifySuspend = func(c chan<- os.Signal) { armed = c }
	t.Cleanup(func() { notifySuspend = old })

	s := vmmSpec()
	s.SnapshotDir = "/m/snapshot"
	f := &snapAPI{snapshots: make(chan string, 1)}
	RunVMM(f, s, nil) // o krun_start_enter falso volta na hora: o erro é o do contrato, não deste teste
	if armed == nil {
		t.Fatal("o sinal de suspender não foi armado")
	}
	armed <- SuspendSignal
	select {
	case got := <-f.snapshots:
		if got != "/m/snapshot" {
			t.Fatalf("snapshot em %q", got)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("o sinal não virou pedido de snapshot")
	}

	armed = nil
	RunVMM(&snapAPI{snapshots: make(chan string, 1)}, vmmSpec(), nil)
	if armed != nil {
		t.Fatal("sem SnapshotDir armou")
	}
	RunVMM(&fakeAPI{}, s, nil)
	if armed != nil {
		t.Fatal("libkrun sem snapshot armou")
	}
}

// TestSnapshotFailureKeepsRunning: pedido de snapshot que a libkrun recusa só vai para o stderr — a goroutine não
// derruba o processo, e a VM segue (o rc vê que o snapshot não apareceu e desliga pelo caminho normal).
func TestSnapshotFailureKeepsRunning(t *testing.T) {
	var armed chan<- os.Signal
	old := notifySuspend
	notifySuspend = func(c chan<- os.Signal) { armed = c }
	t.Cleanup(func() { notifySuspend = old })
	done := make(chan struct{})
	armSuspend(failingSnap{done}, 1, "/m/snapshot")
	armed <- SuspendSignal
	select {
	case <-done:
	case <-time.After(2 * time.Second):
		t.Fatal("o pedido não foi feito")
	}
}

// failingSnap recusa o snapshot (rc negativo) e avisa que foi chamado.
type failingSnap struct{ done chan struct{} }

// SnapshotRequest recusa.
func (f failingSnap) SnapshotRequest(uint32, string, uint32) int32 { close(f.done); return -95 }

// Restore não é usado aqui.
func (failingSnap) Restore(uint32, string, uint32) int32 { return 0 }
