// Command containerd-shim-rc-v2 é o runtime do rc para Kubernetes (runtime_type io.containerd.rc.v2):
// o containerd o acha no PATH pelo nome. Cada pod vira uma microVM da libkrun; o binário rc faz o
// processo de cada VM (subcomando vmm). Só liga as peças: a lógica está em internal/adapters/k8sshim.
package main

import (
	"context"
	"encoding/json"
	"io"
	"os"

	"github.com/containerd/containerd/v2/pkg/shim"
	"github.com/containerd/containerd/v2/pkg/shutdown"
	"github.com/containerd/containerd/v2/plugins"
	"github.com/containerd/plugin"
	"github.com/containerd/plugin/registry"
	"github.com/containerd/ttrpc"

	taskAPI "github.com/containerd/containerd/api/runtime/task/v3"

	"runner.code/internal/adapters/k8sshim"
)

// configPath é onde o instalador grava onde estão o rc, a libkrun e a imagem do pod.
// RC_SHIM_CONFIG troca (os testes e um nó com outro layout).
const configPath = "/etc/rc/shim.json"

// loadConfig lê a Config; sem arquivo, os caminhos padrão do instalador (/opt/rc).
func loadConfig() k8sshim.Config {
	cfg := k8sshim.Config{RC: "/opt/rc/bin/rc", LibDir: "/opt/rc/lib", LibPath: "/opt/rc/lib/libkrun.so.1",
		Image: "/opt/rc/pod.ext4", StateDir: "/var/lib/rc-shim"}
	p := configPath
	if v := os.Getenv("RC_SHIM_CONFIG"); v != "" {
		p = v
	}
	if b, err := os.ReadFile(p); err == nil {
		json.Unmarshal(b, &cfg)
	}
	return cfg
}

// taskService embrulha o Service para o pkg/shim registrar no servidor TTRPC.
type taskService struct{ *k8sshim.Service }

// RegisterTTRPC registra o serviço de task v3.
func (t taskService) RegisterTTRPC(s *ttrpc.Server) error {
	taskAPI.RegisterTTRPCTaskService(s, t.Service)
	return nil
}

// init registra o serviço de task como plugin TTRPC do shim, com o publisher de eventos e o
// shutdown que o pkg/shim fornece.
func init() {
	registry.Register(&plugin.Registration{
		Type:     plugins.TTRPCPlugin,
		ID:       "task",
		Requires: []plugin.Type{plugins.EventPlugin, plugins.InternalPlugin},
		InitFn: func(ic *plugin.InitContext) (any, error) {
			pp, err := ic.GetByID(plugins.EventPlugin, "publisher")
			if err != nil {
				return nil, err
			}
			ss, err := ic.GetByID(plugins.InternalPlugin, "shutdown")
			if err != nil {
				return nil, err
			}
			sd := ss.(shutdown.Service)
			svc := k8sshim.NewService(k8sshim.NewPodBooter(loadConfig()), pp.(shim.Publisher), k8sshim.OpenFifos, k8sshim.MountRootfs, sd.Shutdown)
			return taskService{svc}, nil
		},
	})
}

// flagValue acha o valor de uma flag "-nome valor" nos argumentos (o pkg/shim só as lê dentro do RunShim).
func flagValue(args []string, name string) string {
	for i := 0; i+1 < len(args); i++ {
		if args[i] == name || args[i] == "-"+name {
			return args[i+1]
		}
	}
	return ""
}

// main entrega o processo ao pkg/shim, que trata start, delete e o servidor. No "start" vindo de um
// containerd anterior ao 2.3 (o do microk8s), a resposta é traduzida para o JSON que ele lê
// (k8sshim.FormatStartResult): o stdin é lido aqui e devolvido ao pkg/shim por um pipe, e o stdout
// passa por outro pipe até a tradução.
func main() {
	mgr := k8sshim.Manager{StateDir: loadConfig().StateDir}
	args := os.Args[1:]
	if len(args) == 0 || args[len(args)-1] != "start" {
		shim.RunShim(context.Background(), mgr)
		return
	}
	input, _ := io.ReadAll(io.LimitReader(os.Stdin, 10<<20))
	legacy := k8sshim.IsLegacyStart(input, flagValue(args, "-id"), flagValue(args, "-namespace"))
	inR, inW, _ := os.Pipe()
	go func() { inW.Write(input); inW.Close() }()
	outR, outW, _ := os.Pipe()
	realOut := os.Stdout
	os.Stdin, os.Stdout = inR, outW
	done := make(chan []byte)
	go func() { b, _ := io.ReadAll(outR); done <- b }()
	shim.RunShim(context.Background(), mgr)
	outW.Close()
	realOut.Write(k8sshim.FormatStartResult(<-done, legacy))
}
