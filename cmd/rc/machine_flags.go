package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/pflag"

	"runner.codes/internal/adapters/oci"
	"runner.codes/internal/adapters/runfile"
	"runner.codes/internal/domain"
	"runner.codes/internal/pack"
)

// errCUDA é a recusa do --cuda: o rc não faz remoting de CUDA (que exige GPU
// NVIDIA no host); o --gpu daqui é Vulkan.
var errCUDA = errors.New("--cuda is not supported: rc has no CUDA remoting (it needs a Linux host with an NVIDIA GPU); --gpu gives the guest Vulkan, not CUDA")

// machineFlags são as flags que declaram uma máquina, comuns ao `create` e ao `run`.
type machineFlags struct {
	mc     domain.Machine
	cpus   uint
	file   string
	mounts []string
	ports  []string
	creds  []string
	secEnv []string
	secFil []string
	from   string
	// diskFrom é o disco de partida do pack (--from de uma máquina empacotada).
	diskFrom string
	display  string
	dport    uint16
	dtitle   string
	// exposeSock e mountSock são as pontes de socket (--expose-socket, --mount-socket).
	exposeSock []string
	mountSock  []string
	// cuda existe só para recusar com o motivo: quem vem de outra ferramenta digita --cuda, e
	// "unknown flag" não diz que o recurso não existe aqui nem qual é a alternativa.
	cuda bool
}

// addMachineFlags registra as flags de declaração em f; defName é o nome padrão.
func addMachineFlags(f *pflag.FlagSet, defName string) *machineFlags {
	mf := &machineFlags{}
	mc := &mf.mc
	f.StringVarP(&mf.file, "file", "f", "", "runner.code file (default ./runner.code when present)")
	// -s/--spec: atalho oculto de --file, para comandos escritos com -s seguirem funcionando.
	f.StringVarP(&mf.file, "spec", "s", "", "alias of --file")
	f.MarkHidden("spec")
	f.StringVar(&mf.from, "from", "", "start from a pack (an artifact or its "+pack.SidecarExt+" payload): its image, settings and disk")
	f.StringVar(&mc.Name, "name", defName, "machine name")
	f.StringVar(&mc.Stack, "stack", "", "stack whose image is the machine base (python, go, ...)")
	f.StringVar(&mc.Image, "image", "", "any OCI image as the machine base (built on first use)")
	f.UintVar(&mf.cpus, "cpus", 0, "vCPUs (default 4)")
	f.Uint32Var(&mc.MemoryMiB, "memory", 0, "memory ceiling in MiB (default 8192; elastic: the host commits only what the guest uses)")
	f.Uint32Var(&mc.MemoryMiB, "mem", 0, "alias of --memory")
	f.IntVar(&mc.DiskGiB, "disk-gib", 0, "writable disk in GiB (default 10, sparse)")
	f.IntVar(&mc.DiskGiB, "storage", 0, "alias of --disk-gib")
	f.BoolVar(&mc.GPU, "gpu", false, "give the machine the host GPU through Vulkan (the stack must declare gpu)")
	f.Uint32Var(&mc.GPUVRAMMiB, "gpu-vram", 0, "GPU shared memory window in MiB (default: the stack's)")
	f.StringArrayVarP(&mc.Env, "env", "e", nil, "environment variable K=V for init, workload, exec and shell (repeatable)")
	f.StringVarP(&mc.Workdir, "workdir", "w", "", "working directory for init, workload, exec and shell")
	f.BoolVar(&mc.SSHAgent, "ssh-agent", false, "forward the host SSH agent (SSH_AUTH_SOCK at start): keys stay on the host")
	f.BoolVar(&mc.DockerSocket, "docker-socket", false, "expose the guest's /var/run/docker.sock as docker.sock in the machine directory (rc machine data-dir)")
	f.StringVarP(&mc.User, "user", "u", "", "who runs the workload, exec and shell (passwd name or uid[:gid]); init runs as root")
	f.StringArrayVar(&mc.Init, "init", nil, "command run once, on the first start (repeatable, in order)")
	f.StringVar(&mc.StopSignal, "stop-signal", "", "signal sent to the workload on stop (default SIGTERM)")
	f.IntVar(&mc.StopTimeout, "stop-timeout", 0, "seconds the workload gets to exit on stop (default 10)")
	f.BoolVar(&mc.StopOnExit, "stop-on-exit", false, "stop the machine when the workload exits")
	f.StringArrayVarP(&mf.mounts, "volume", "v", nil, "host directory HOST:GUEST[:ro], mounted on every start (repeatable)")
	f.BoolVar(&mc.Network.Enabled, "net", false, "outbound network through the host gateway (off by default)")
	f.StringArrayVarP(&mf.ports, "port", "p", nil, "publish a guest port on 127.0.0.1: PORT, HOST:GUEST, or with /udp (repeatable)")
	egressFlags(f, &mc.Network.Egress)
	f.StringArrayVar(&mf.secEnv, "secret-env", nil, "GUEST_VAR=HOST_VAR: the host variable's value, read at start, in every command's environment (repeatable; never stored)")
	f.StringArrayVar(&mf.secFil, "secret-file", nil, "GUEST_VAR=/abs/file: the file's content, read at start, in every command's environment (repeatable; never stored)")
	f.StringArrayVar(&mf.creds, "credential", nil, "credential the machine uses without reading it: NAME=ENV_VAR@HOST[,HOST...][:body][:set-header=HEADER] (repeatable; the value comes from ENV_VAR when the machine starts)")
	f.StringVar(&mc.Network.Name, "network", "", "named network: machines in it reach each other by IP and <name>.<network>.internal")
	f.StringVar(&mf.display, "display", "", "screen WIDTHxHEIGHT, viewed in the browser (needs --gpu)")
	f.Uint16Var(&mf.dport, "display-port", domain.DefaultDisplayPort, "port of the screen viewer on 127.0.0.1")
	f.StringVar(&mf.dtitle, "display-title", "", "name shown on the screen viewer's bar and tab (default runner.codes)")
	f.StringArrayVar(&mf.exposeSock, "expose-socket", nil, "GUEST[:HOST]: a unix socket the workload listens on, reachable on the host at HOST (default: its name in rc machine data-dir) (repeatable)")
	f.StringArrayVar(&mf.mountSock, "mount-socket", nil, "HOST:GUEST: a host unix socket, reachable in the machine at GUEST (repeatable)")
	f.BoolVar(&mc.Nested, "nested", false, "give the guest the host's virtualization extensions (/dev/kvm inside; needs nested KVM on Linux or an M3+ Mac on macOS 15+; slow)")
	f.BoolVar(&mf.cuda, "cuda", false, "not supported")
	f.MarkHidden("cuda")
	return mf
}

// build junta arquivo e flags na máquina declarada (flag > arquivo > padrão) e devolve o
// arquivo usado. workload é o que veio depois do -- no create.
func (mf *machineFlags) build(flags *pflag.FlagSet, workload []string) (domain.Machine, string, error) {
	mc := mf.mc
	if mf.cuda {
		return mc, "", errCUDA
	}
	// O lado do host pode vir relativo na linha de comando; vira absoluto aqui, porque o vmm
	// roda em outro diretório e a máquina é ligada depois, de qualquer lugar.
	abs := func(spec string, hostFirst bool) string {
		a, b, ok := strings.Cut(spec, ":")
		if hostFirst && ok && !filepath.IsAbs(a) {
			if p, err := filepath.Abs(a); err == nil {
				return p + ":" + b
			}
		}
		if !hostFirst && ok && b != "" && !filepath.IsAbs(b) {
			if p, err := filepath.Abs(b); err == nil {
				return a + ":" + p
			}
		}
		return spec
	}
	for _, s := range mf.exposeSock {
		b, err := domain.ParseExposeSocket(abs(s, false))
		if err != nil {
			return mc, "", err
		}
		mc.ExposeSockets = append(mc.ExposeSockets, b)
	}
	for _, s := range mf.mountSock {
		b, err := domain.ParseMountSocket(abs(s, true))
		if err != nil {
			return mc, "", err
		}
		mc.MountSockets = append(mc.MountSockets, b)
	}
	mc.CPUs = uint8(mf.cpus)
	mc.Workload = workload
	for _, s := range mf.mounts {
		mt, err := domain.ParseMount(s)
		if err != nil {
			return mc, "", err
		}
		mc.Mounts = append(mc.Mounts, mt)
	}
	for i, list := range [][]string{mf.secEnv, mf.secFil} {
		for _, s := range list {
			sec, err := domain.ParseSecret(s, i == 1)
			if err != nil {
				return mc, "", err
			}
			mc.Secrets = append(mc.Secrets, sec)
		}
	}
	for _, s := range mf.creds {
		c, err := domain.ParseCredential(s)
		if err != nil {
			return mc, "", err
		}
		mc.Network.Credentials = append(mc.Network.Credentials, c)
	}
	for _, s := range mf.ports {
		ps, err := domain.ParsePorts(s)
		if err != nil {
			return mc, "", err
		}
		mc.Network.Ports = append(mc.Network.Ports, ps...)
	}
	if mf.display != "" {
		d, err := domain.ParseDisplay(mf.display)
		if err != nil {
			return mc, "", err
		}
		d.Port, d.Title = mf.dport, mf.dtitle
		mc.Display = &d
	} else if mf.dtitle != "" {
		return mc, "", errors.New("--display-title needs --display")
	}
	cwd, err := os.Getwd()
	if err != nil {
		return mc, "", err
	}
	// Na linha de comando, "./" é relativo ao diretório de onde se chamou.
	if mc.Mounts, err = runfile.ResolveMounts(mc.Mounts, cwd); err != nil {
		return mc, "", err
	}
	file := mf.file
	if file == "" {
		if _, err := os.Stat(runfile.DefaultName); err == nil {
			file = runfile.DefaultName
		}
	}
	base := domain.Machine{}
	if mf.from != "" {
		if mf.file != "" {
			return mc, "", errors.New("--from and --file are exclusive")
		}
		imp, err := importPack(mf.from)
		if err != nil {
			return mc, "", err
		}
		base, mf.diskFrom, file = imp.machine, imp.disk, ""
	} else if file != "" {
		if base, err = runfile.Load(file); err != nil {
			return mc, "", err
		}
	}
	// As flags sinônimas contam como a principal na precedência.
	changed := func(k string) bool {
		alias := map[string]string{"memory": "mem", "disk-gib": "storage"}
		return flags.Changed(k) || (alias[k] != "" && flags.Changed(alias[k]))
	}
	mc = runfile.Override(base, mc, changed)
	// --image -: a imagem chega pelo stdin (docker save | rc machine run --image - ...).
	if mc.Image == "-" {
		p, err := oci.ImportStream(os.Stdin, filepath.Join(cacheDir(), "imports"))
		if err != nil {
			return mc, "", err
		}
		mc.Image = p
	}
	// Caminho local fica absoluto no registro: o create lê o arquivo, e o start não depende
	// de onde o comando foi chamado.
	if oci.IsLocalRef(mc.Image) {
		mc.Image, _ = filepath.Abs(mc.Image)
	}
	if mc.Stack == "" && mc.Image == "" {
		return mc, "", errors.New("set --stack or --image (or write them in a runner.code)")
	}
	if mc.Image != "" && !strings.HasPrefix(mc.Image, packRef) {
		fmt.Fprintf(os.Stderr, "preparing image %s (built on first use)...\n", mc.Image)
	}
	return mc, file, nil
}
