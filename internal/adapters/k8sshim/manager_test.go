package k8sshim

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	bootapi "github.com/containerd/containerd/api/runtime/bootstrap/v1"
	"github.com/containerd/containerd/v2/pkg/namespaces"
	specs "github.com/opencontainers/runtime-spec/specs-go"
)

// shortDir devolve um diretório de caminho curto: o socket Unix tem teto de ~104 bytes no macOS, e o
// TempDir do teste já passa disso com o hash de 64 caracteres do endereço.
func shortDir(t *testing.T) string {
	t.Helper()
	d := fmt.Sprintf("/tmp/rcs%d", os.Getpid())
	os.MkdirAll(d, 0o700)
	t.Cleanup(func() { os.RemoveAll(d) })
	return d
}

// ptr devolve o endereço do valor (o BootstrapParams usa *string nos campos opcionais).
func ptr(s string) *string { return &s }

// inBundle troca o diretório atual para um bundle com o config.json do pod p1 (o start roda nele).
func inBundle(t *testing.T) {
	t.Helper()
	dir := t.TempDir()
	b, _ := json.Marshal(specs.Spec{Process: &specs.Process{Args: []string{"sh"}}, Annotations: map[string]string{annSandboxID: "p1"}})
	os.WriteFile(filepath.Join(dir, "config.json"), b, 0o644)
	old, _ := os.Getwd()
	os.Chdir(dir)
	t.Cleanup(func() { os.Chdir(old) })
}

// TestManagerStart: o primeiro container do pod lança o shim com o socket herdado; o segundo do mesmo
// pod recebe o endereço do shim que já escuta, sem lançar outro.
func TestManagerStart(t *testing.T) {
	inBundle(t)
	sockDir := shortDir(t)
	var launched []*exec.Cmd
	orig := startShim
	startShim = func(cmd *exec.Cmd) error {
		launched = append(launched, cmd)
		cmd.Path, cmd.Args = "/usr/bin/true", []string{"true"}
		return cmd.Start()
	}
	defer func() { startShim = orig }()
	ctx := namespaces.WithNamespace(context.Background(), "k8s.io")
	params := &bootapi.BootstrapParams{InstanceID: "c1", SocketDir: &sockDir, ContainerdGrpcAddress: "/run/containerd.sock"}
	res, err := Manager{}.Start(ctx, params)
	if err != nil || res.Version != 3 || !strings.HasPrefix(res.Address, "unix://"+sockDir) || len(launched) != 1 {
		t.Fatalf("1º start: %+v %v launched=%d", res, err, len(launched))
	}
	if a := launched[0].Args; len(a) == 0 {
		t.Fatal("sem argumentos")
	}
	again, err := Manager{}.Start(ctx, &bootapi.BootstrapParams{InstanceID: "c2", SocketDir: &sockDir, ContainerdGrpcAddress: "/run/containerd.sock"})
	if err != nil || again.Address != res.Address || len(launched) != 1 {
		t.Fatalf("2º do mesmo pod: %+v %v launched=%d", again, err, len(launched))
	}
}

// TestManagerStartErrors: sem config.json, sem namespace e com o lançamento falhando, o start recusa.
func TestManagerStartErrors(t *testing.T) {
	old, _ := os.Getwd()
	os.Chdir(t.TempDir())
	_, err := Manager{}.Start(context.Background(), &bootapi.BootstrapParams{InstanceID: "x"})
	os.Chdir(old)
	if err == nil {
		t.Fatal("sem config.json")
	}
	inBundle(t)
	if _, err := (Manager{}).Start(context.Background(), &bootapi.BootstrapParams{InstanceID: "x", SocketDir: ptr(shortDir(t))}); err == nil {
		t.Fatal("sem namespace")
	}
	orig := startShim
	startShim = func(*exec.Cmd) error { return errors.New("exec boom") }
	defer func() { startShim = orig }()
	ctx := namespaces.WithNamespace(context.Background(), "k8s.io")
	if _, err := (Manager{}).Start(ctx, &bootapi.BootstrapParams{InstanceID: "x", SocketDir: ptr(shortDir(t))}); err == nil {
		t.Fatal("lançamento falhou e passou")
	}
}

// TestNewShimSocketStale: socket de um shim morto (arquivo sem ninguém escutando) é recriado.
func TestNewShimSocketStale(t *testing.T) {
	ctx := namespaces.WithNamespace(context.Background(), "k8s.io")
	dir := shortDir(t)
	s, err := newShimSocket(ctx, dir, "/c.sock", "g")
	if err != nil {
		t.Fatal(err)
	}
	addr := s.addr
	s.l.SetUnlinkOnClose(false)
	s.l.Close()
	s.f.Close()
	s2, err := newShimSocket(ctx, dir, "/c.sock", "g")
	if err != nil || s2.addr != addr {
		t.Fatalf("recriar: %v", err)
	}
	s2.Close()
	if _, err := newShimSocket(context.Background(), dir, "/c.sock", "g"); err == nil {
		t.Fatal("sem namespace")
	}
}

// TestManagerStopInfo: o delete de limpeza apaga o diretório do pod e informa SIGKILL; o info diz o nome.
func TestManagerStopInfo(t *testing.T) {
	state := t.TempDir()
	os.MkdirAll(filepath.Join(state, "pods", "p1"), 0o700)
	st, err := Manager{StateDir: state}.Stop(context.Background(), "p1")
	if err != nil || st.ExitStatus != 137 {
		t.Fatal(st, err)
	}
	if _, err := os.Stat(filepath.Join(state, "pods", "p1")); err == nil {
		t.Fatal("diretório do pod ficou")
	}
	if info, _ := (Manager{}).Info(context.Background(), nil); info.Name != RuntimeName || (Manager{}).Name() != RuntimeName {
		t.Fatal("nome")
	}
}
