package k8sshim

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"syscall"
	"time"

	bootapi "github.com/containerd/containerd/api/runtime/bootstrap/v1"
	"github.com/containerd/containerd/api/types"
	"github.com/containerd/containerd/v2/pkg/namespaces"
	"github.com/containerd/containerd/v2/pkg/shim"
	"github.com/containerd/errdefs"
	specs "github.com/opencontainers/runtime-spec/specs-go"
)

// RuntimeName é o runtime_type que o containerd usa para achar este binário
// (containerd-shim-rc-v2 no PATH).
const RuntimeName = "io.containerd.rc.v2"

// Manager cuida do ciclo de vida do processo do shim (o start, o delete de limpeza e o info).
type Manager struct {
	// StateDir é o state_dir da Config: o delete de limpeza apaga o pod que um shim morto deixou.
	StateDir string
}

// Name é o nome do runtime.
func (Manager) Name() string { return RuntimeName }

// shimSocket é o socket TTRPC de um pod: o endereço que o containerd recebe e o fd que o processo
// do shim herda.
type shimSocket struct {
	addr string
	l    *net.UnixListener
	f    *os.File
}

// Close fecha e apaga o socket.
func (s *shimSocket) Close() {
	if s.l != nil {
		s.l.Close()
	}
	if s.f != nil {
		s.f.Close()
	}
	shim.RemoveSocket(s.addr)
}

// newShimSocket cria (ou reconhece) o socket do grupo. Se um shim vivo já escuta nele, é o shim do
// pod: o containerd deve falar com ele (ErrAlreadyExists com o endereço).
func newShimSocket(ctx context.Context, root, path, group string) (*shimSocket, error) {
	addr, err := shim.CreateSocketAddress(ctx, root, path, group, false)
	if err != nil {
		return nil, err
	}
	l, err := shim.NewSocket(addr)
	if err != nil {
		if !shim.SocketEaddrinuse(err) {
			return nil, fmt.Errorf("create shim socket: %w", err)
		}
		if shim.CanConnect(addr) {
			return &shimSocket{addr: addr}, errdefs.ErrAlreadyExists
		}
		// Socket de um shim que morreu: apaga e cria de novo.
		if err := shim.RemoveSocket(addr); err != nil {
			return nil, err
		}
		if l, err = shim.NewSocket(addr); err != nil {
			return nil, err
		}
	}
	s := &shimSocket{addr: addr, l: l}
	if s.f, err = l.File(); err != nil {
		s.Close()
		return nil, err
	}
	return s, nil
}

// readSpec lê o config.json do bundle no diretório atual (o containerd chama o start nele).
func readSpec() (*specs.Spec, error) {
	b, err := os.ReadFile("config.json")
	if err != nil {
		return nil, err
	}
	var s specs.Spec
	return &s, json.Unmarshal(b, &s)
}

// startShim lança o processo do shim; os testes trocam para não reexecutar o binário de teste.
var startShim = func(cmd *exec.Cmd) error { return cmd.Start() }

// Start abre (ou reusa) o shim do pod: o primeiro container de um sandbox lança o processo do shim
// com o socket herdado; os seguintes recebem o endereço do que já existe.
func (m Manager) Start(ctx context.Context, opts *bootapi.BootstrapParams) (*bootapi.BootstrapResult, error) {
	res := &bootapi.BootstrapResult{Version: 3, Protocol: "ttrpc"}
	id := opts.GetInstanceID()
	spec, err := readSpec()
	if err != nil {
		return nil, err
	}
	dir := opts.GetSocketDir()
	if dir == "" {
		dir = "/run/containerd/s"
	}
	sock, err := newShimSocket(ctx, dir, opts.GetContainerdGrpcAddress(), GroupID(spec, id))
	if err != nil {
		if errdefs.IsAlreadyExists(err) {
			res.Address = sock.addr
			return res, nil
		}
		return nil, err
	}
	ns, err := namespaces.NamespaceRequired(ctx)
	if err != nil {
		sock.Close()
		return nil, err
	}
	self, err := os.Executable()
	if err != nil {
		sock.Close()
		return nil, err
	}
	cwd, _ := os.Getwd()
	cmd := exec.Command(self, "-namespace", ns, "-id", id, "-address", opts.GetContainerdGrpcAddress())
	cmd.Dir = cwd
	cmd.Env = append(os.Environ(), "GOMAXPROCS=4")
	// Grupo próprio: o SIGINT de quem chamou o start não derruba o shim (nem as VMs dele).
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	cmd.ExtraFiles = []*os.File{sock.f}
	if err := startShim(cmd); err != nil {
		sock.Close()
		return nil, err
	}
	go cmd.Wait()
	shim.AdjustOOMScore(cmd.Process.Pid)
	res.Address = sock.addr
	return res, nil
}

// Stop é o delete de limpeza, chamado quando o shim do container morreu sem limpar: apaga o
// diretório do pod (a VM já morreu junto com o shim, pelo pipe de vigia) e informa SIGKILL.
func (m Manager) Stop(ctx context.Context, id string) (shim.StopStatus, error) {
	if m.StateDir != "" {
		os.RemoveAll(filepath.Join(m.StateDir, "pods", id))
	}
	return shim.StopStatus{ExitedAt: time.Now(), ExitStatus: 128 + int(syscall.SIGKILL)}, nil
}

// Info descreve o runtime.
func (m Manager) Info(ctx context.Context, _ io.Reader) (*types.RuntimeInfo, error) {
	return &types.RuntimeInfo{Name: RuntimeName}, nil
}
