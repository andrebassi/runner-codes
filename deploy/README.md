# Runtime de Kubernetes do rc (k3s e microk8s)

Cada pod com `runtimeClassName: rc` roda numa **microVM da libkrun com kernel próprio**, em vez de dividir o kernel do
nó. O containerd chama o `containerd-shim-rc-v2`; o container pause do pod liga a VM; os containers da app rodam dentro
dela; a VM assume o IP que o CNI deu ao pod. Para o resto do cluster é um pod comum: `kubectl exec`, `kubectl logs`,
Service, DNS e tráfego pod a pod funcionam.

Medido em 20/10/2026 no k3s 1.36 e no microk8s 1.35, com a prova `deploy/k8s/e2e.sh` (19 checagens, 0 falhas nos dois,
duas rodadas seguidas). Documentação de uso: [docs › Kubernetes](../docs/docusaurus/docs/advanced/kubernetes.md).
Resultados e defeitos achados: [relatório de testes](../docs/docusaurus/docs/advanced/test-report.md#linux-com-kvm-e-kubernetes).
Passo a passo conferido: [runbook, seção 8x](../docs/runbook.md#8x-runtime-de-kubernetes-k3s-e-microk8s).

## Sumário

1. [Requisitos](#requisitos)
2. [Instalar](#instalar)
3. [Usar](#usar)
4. [Como funciona](#como-funciona)
5. [Decisões e o porquê](#decisões-e-o-porquê)
6. [Defeitos achados no nó real](#defeitos-achados-no-nó-real)
7. [Como foi testado](#como-foi-testado)
8. [Reproduzir o teste](#reproduzir-o-teste)
9. [Quando algo der errado](#quando-algo-der-errado)
10. [Limites](#limites)
11. [Mapa do código](#mapa-do-código)

## Requisitos

| Item | Por quê |
|---|---|
| nó **Linux** x86_64 com **`/dev/kvm`** | a libkrun usa o KVM; sem ele não há VM (VPS comum costuma não expor: meça com `ls -l /dev/kvm`) |
| k3s **ou** microk8s (medidos: k3s 1.36 com containerd 2.3.4 e microk8s 1.35 com containerd 2.1.6) | o shim fala o serviço de task v3 e os dois protocolos de start; outras versões não foram medidas |
| Nix no nó, `task krun:install` e `task build` feitos no repositório | a libkrun vem do nixpkgs (com virtio-blk), e o build gera `bin/containerd-shim-rc-v2-linux-<arch>` |
| root para instalar | o instalador escreve em `/opt/rc`, `/usr/local/bin`, `/etc/rc` e na config do containerd |

## Instalar

```bash
sudo deploy/k3s/install.sh        # k3s
sudo deploy/microk8s/install.sh   # microk8s
```

Os dois chamam primeiro o [`install-node.sh`](install-node.sh), comum a qualquer distribuição:

| O que | Onde |
|---|---|
| `rc` (o processo de cada VM é o `rc vmm`) e o guest | `/opt/rc/bin/` |
| libkrun e libkrunfw (cópias dos links do perfil Nix) | `/opt/rc/lib/` |
| imagem da VM do pod (Alpine do catálogo, stack `awk`, com o guest do rc) | `/opt/rc/pod.ext4` |
| o shim, no PATH do containerd | `/usr/local/bin/containerd-shim-rc-v2` |
| onde estão as peças | `/etc/rc/shim.json` |
| estado de cada pod (spec, console, socket do vsock, share) | `/var/lib/rc-shim/pods/<id do sandbox>/` |

Depois cada instalador liga o runtime ao containerd da distribuição:

- **k3s** ([`k3s/install.sh`](k3s/install.sh)): o `config-v3.toml` que o k3s gera importa
  `config-v3.toml.d/*.toml`, então o runtime entra em `config-v3.toml.d/rc.toml`, sem tocar no template (onde moram
  mirrors e auth de registry). O cabeçalho da tabela do runtime sai da `version` que o próprio
  config declara (`rc_cri_table`, em [`lib.sh`](lib.sh)): o nome do plugin CRI muda entre as versões do formato.
  Reinicia o k3s e espera o `/readyz`.
- **microk8s** ([`microk8s/install.sh`](microk8s/install.sh)): acrescenta um bloco entre marcadores
  (`# rc:begin` … `# rc:end`) no fim do `containerd-template.toml` do snap; reexecutar troca o bloco em vez de
  duplicar. Reinicia o `microk8s.daemon-containerd` e espera o serviço voltar (veja
  [Quando algo der errado](#quando-algo-der-errado)).

O bloco é o mesmo nos dois, só o cabeçalho muda:

```toml
[plugins.'io.containerd.cri.v1.runtime'.containerd.runtimes.rc]   # k3s (containerd 2.3, config v3)
  runtime_type = "io.containerd.rc.v2"
  pod_annotations = ["runner.codes/*"]
```

Sem a tabela `[options]`: as opções do runc (`BinaryName`, `SystemdCgroup`) não valem para este shim. Por fim o nó
ganha o rótulo `runner.codes/runtime=true` e a RuntimeClass `rc` (`rc_runtime_class`, em [`lib.sh`](lib.sh)) é aplicada. Reexecutar é
seguro; depois de um upgrade da distribuição, rodar de novo.

## Usar

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: rc-hello
  annotations:
    runner.codes/cpus: "1"          # opcional: vCPUs da VM (1 a 16, padrão 2)
    runner.codes/memory-mib: "512"  # opcional: teto de memória (128 a 65536, padrão 1024; a memória é elástica)
spec:
  runtimeClassName: rc
  containers:
    - name: hello
      image: busybox:1.36
      command: ["sh", "-c", "uname -sr; sleep 3600"]
```

O pod que o e2e usa sai de `rc_demo_pod`, em [`lib.sh`](lib.sh). `kubectl logs rc-hello` mostra `Linux 6.12.91`, o
kernel da libkrunfw, e não o do nó. As anotações chegam ao shim porque o runtime declara `pod_annotations`.

## Como funciona

```
kubelet ─CRI─▶ containerd ─▶ containerd-shim-rc-v2  (um processo por pod, TTRPC task v3)
                                │
                                ├─ sandbox (pause) ─▶ rc vmm  (libkrun + KVM, processo da VM)
                                │                        ├─ disco: /opt/rc/pod.ext4, read-only
                                │                        ├─ virtio-fs "rcpod": /var/lib/rc-shim/pods/<id>/share
                                │                        ├─ virtio-net: TAP no netns do pod (tc mirred com a eth0)
                                │                        └─ vsock 5000: o guest do rc
                                │
                                └─ container da app ─▶ rootfs copiado para o share, montagens no guest,
                                                        processo em chroot (OpProc), stdio pelas FIFOs
```

**Ciclo de um pod**

1. O kubelet pede o sandbox. O containerd roda `containerd-shim-rc-v2 start`; o [manager](../internal/adapters/k8sshim/manager.go)
   lança o processo do shim com um socket por grupo (o id do pod). Os containers seguintes do mesmo pod recebem o
   endereço do shim que já escuta.
2. `Create` do sandbox: o [PodBooter](../internal/adapters/k8sshim/podvm.go) liga a VM (pelo launcher do rc, o
   mesmo do `rc machine`), espera o guest responder, monta um tmpfs no `/run`, monta o share em `/run/rc-pod` e põe o
   hostname do pod. O "processo" do pause não existe: ele fica rodando até o `Kill`.
3. `Create` de um container: o shim monta as camadas do snapshotter em `<bundle>/rootfs`, copia para
   `share/<id>/rootfs`, copia as origens das montagens bind (volumes, `/etc/hosts`, `resolv.conf`, serviceaccount)
   para `share/mounts/N` (a mesma origem no pod usa a mesma cópia: é assim que dois containers veem o mesmo
   emptyDir) e manda o guest montar proc, sysfs, tmpfs, devpts e os binds ([MountScript](../internal/adapters/k8sshim/mounts.go)).
4. `Start`: o shim abre as FIFOs do containerd ([stdio](../internal/adapters/k8sshim/stdio.go)) e uma conexão
   `OpProc` com o guest ([proc.go](../internal/guest/proc.go)): o processo roda em chroot no rootfs do container, só
   com o Env da spec e o usuário numérico dela. A conexão leva stdin, eof, resize e sinais para o guest, e stdout,
   stderr (ou o TTY) e o código de saída de volta.
5. `Exec` (`kubectl exec`): outro `OpProc` no mesmo root.
6. `Kill`/`Wait`/`Delete`: o sinal vai ao processo no guest; o código volta como o do programa, ou 128+sinal; o
   delete desmonta no guest e apaga a cópia; o delete do sandbox desliga a VM (SIGKILL no vmm) e apaga o diretório do
   pod. Sem container sobrando, o `Shutdown` encerra o shim.

**Rede** ([podnet](../internal/adapters/podnet/)): no processo vmm, o shim lê a `eth0` do netns que o CNI criou (IPv4,
prefixo, MAC, MTU e gateway), cria o `tap0` **dentro** desse netns, liga os dois com um filtro `tc matchall` +
`mirred egress redirect` em cada sentido e bombeia os quadros entre o TAP e o socket da libkrun
(`krun_add_net_unixstream`, 4 bytes de tamanho + quadro). O guest sobe a `eth0` com o MAC, o IP e o gateway do pod.
Gateway fora do prefixo (Calico) ganha uma rota de link antes da rota padrão.

## Decisões e o porquê

| Decisão | Por quê | Descartado |
|---|---|---|
| **um shim por pod** (`GroupLabels`: `io.kubernetes.cri.sandbox-id`) | sem agrupar, o containerd abre um shim por container e o container da app não acha a VM | um shim por container |
| **uma VM por pod** | containers do pod compartilham rede, IPC e volumes por contrato do Kubernetes | uma VM por container (quebraria localhost e emptyDir) |
| **containerd v2.3.7** como biblioteca | a 2.4 só fala o start novo (containerd ≥ 2.3) e quebraria no microk8s (containerd 2.1.6) | v2.4 |
| **resposta do start traduzida** ([legacy.go](../internal/adapters/k8sshim/legacy.go)) | a 2.3.7 aceita o start antigo mas responde sempre em protobuf; o containerd 2.1 lia isso como endereço | reimplementar o start |
| **rootfs copiado** (`cp -a --reflink=auto`) | reexportar o overlay pelo virtio-fs quebra o copy-up no guest | virtio-fs direto do snapshot |
| **chroot do guest, sem crun** | a VM já é a fronteira; o guest do rc é estático e não precisa de nada na imagem | crun dentro da VM |
| **TAP + tc mirred**, sem NAT | a VM vira o pod para o cluster (IP, MAC, ARP, kube-proxy) | gateway em userspace (gVisor) com NAT |
| **`Stats` com CPU e RSS do vmm** ([usage.go](../internal/adapters/k8sshim/usage.go)) | `Stats` vazio derruba o resumo do kubelet do nó inteiro | não implementar |
| **filhos do shim pelo reaper** (`runChild`) | o pkg/shim colhe todo filho; `cmd.Wait` comum recebia ECHILD | `exec.Command().Run()` |
| **tamanho da VM por anotação** | a VM liga no sandbox, antes de os limites dos containers chegarem | somar os limites dos containers |

## Defeitos achados no nó real

Todos apareceram só no Linux de verdade, viraram teste de unidade e estão corrigidos:

| Sintoma | Causa | Correção |
|---|---|---|
| `nix profile add .#krun` falha com `File exists` | na libkrun do nixpkgs, `lib/` e `lib64/` trazem os mesmos arquivos | `ln -sf` no `flake.nix` |
| todo job do rc falha: `prctl(PR_SET_NO_NEW_PRIVS): operation not supported` | abaixo da ABI 8 do Landlock o go-landlock usa psx, e sem cgo o `AllThreadsSyscall` devolve ENOTSUP (o purego faz o runtime se ver "com cgo") | ruleset na thread do `krun_start_enter` ([sandbox_rules.go](../internal/adapters/krun/sandbox_rules.go)); as threads da libkrun herdam |
| teste do launcher não compilava no Linux | `syscall.Getsid` só existe no BSD/macOS | `unix.Getsid` |
| `boot pod VM: mkdir: can't create directory '/run/rc-pod': Read-only file system` | a raiz da imagem é read-only | tmpfs no `/run` antes do share |
| `prepare container: cp …: waitid: no child processes` | o pkg/shim colhe todo filho no SIGCHLD | filho esperado pelo `reaper.Default` |
| eviction manager cego, metrics-server sem dado, namespace preso em Terminating | `Stats` vazio do shim: `failed to decode container metrics` no kubelet | métricas cgroup v2 com o uso real |
| microk8s: `failed to create TTRPC connection: unsupported protocol: Yunix` | o containerd 2.1 lia a resposta protobuf do start como endereço | JSON antigo para o caminho antigo |
| microk8s: pod sem rede, `Network is unreachable` | Calico: `/32` com gateway `169.254.1.1` fora da sub-rede | rota de link para o gateway ([netcfg.go](../internal/guest/netcfg.go), `GatewayOnLink`) |

## Como foi testado

| Camada | Como | Resultado |
|---|---|---|
| nó com KVM | `task k8s:kvm-probe` cria um droplet, lê `/dev/kvm` e as flags `vmx`, e apaga | `/dev/kvm` presente no `s-2vcpu-4gb` da DigitalOcean |
| unidade (macOS) | `go test -race`; o shim contra booter, VM e processo falsos; o `OpProc` contra o guest de verdade por `net.Pipe` | k8sshim 95,4%, podnet 100%, `proc.go` 98–100% |
| unidade (Linux, root) | `TestAttach` cria netns, veth e eth0 como um CNI; `TestRestrictThreadBlocks` confina um filho | os dois verdes, com prova de falha |
| defeitos plantados | [`scripts/k8s-defects.py`](../scripts/k8s-defects.py) (`task k8s:defects`) aplica um defeito por vez e roda o pacote | 20 plantados, 20 pegos (19 na 1ª rodada; o que passou batido virou assert) |
| ponta a ponta | [`k8s/e2e.sh`](k8s/e2e.sh) no nó, pelo [`scripts/k8s-e2e.sh`](../scripts/k8s-e2e.sh) | 19/19 no k3s e no microk8s, duas rodadas seguidas |
| prova de falha do e2e | código de saída fixo em 0 no shim | as 2 checagens de código de saída reprovaram |
| gate geral | `task gates` (cobertura, alvos, e2e do rc, runbook) | verde, total 95,2% |

As 19 checagens do e2e: pod Ready em até 120 s; `RC_K8S_OK` no log; kernel da VM diferente do do nó; IP do pod na
`eth0` da VM; resumo de estatísticas do kubelet respondendo; `exec` sem TTY, com TTY e com stdin; código do exec (7);
DNS do cluster; pod rc → pod runc; pod runc → pod rc; Service ClusterIP até o pod rc; código do container (3); dois
containers no mesmo emptyDir; os dois na mesma VM (mesmo `boot_id`); e não no kernel do nó; nenhum diretório de pod e
nenhum `rc vmm` depois do delete.

## Reproduzir o teste

Os nós de teste são droplets da DigitalOcean (o token sai do pass, `bassi/digitalocean/api-token`), um por
distribuição, com o estado em `.k8s/<nome>/` (fora do git, com chave SSH própria):

```bash
K8S_NODE=k3s task k8s:node -- create         # s-4vcpu-8gb em nyc3, ~US$ 0,07/h enquanto existir
K8S_NODE=k3s task k8s:node -- provision      # Nix (daemon), libkrun do flake e build no nó
K8S_NODE=k3s task k8s:node -- ssh 'curl -sfL https://get.k3s.io | sh -'
task k8s:e2e -- k3s                          # sync, build, deploy/k3s/install.sh e deploy/k8s/e2e.sh k3s

K8S_NODE=microk8s task k8s:node -- create && K8S_NODE=microk8s task k8s:node -- provision
K8S_NODE=microk8s task k8s:node -- ssh 'snap install microk8s --classic'
task k8s:e2e -- microk8s

K8S_NODE=k3s task k8s:node -- shim           # iterar: troca só os binários, sem reiniciar o k3s
K8S_NODE=k3s task k8s:node -- destroy        # apaga droplet e chave na DigitalOcean
```

Mudou o guest (`internal/guest`)? A imagem do pod precisa ser refeita: o `task k8s:e2e` já roda o instalador, que
reconstrói `/opt/rc/pod.ext4` (a chave de cache inclui o hash do guest).

## Quando algo der errado

| Sintoma | Onde olhar |
|---|---|
| pod em `ContainerCreating` | `kubectl describe pod`: o erro do shim aparece inteiro em `FailedCreatePodSandBox` ou `Failed` |
| `did not become ready` | `/var/lib/rc-shim/pods/<id>/console.log` (boot do guest) |
| shim não acha a VM, ou dois shims no mesmo pod | o containerd precisa passar `io.kubernetes.cri.sandbox-id`; confira `crictl inspectp` |
| `unsupported protocol` | shim de outra versão: `deploy/<dist>/install.sh` de novo |
| microk8s: `snap restart` falha e o containerd volta sozinho | corrida no plugin `sideload` do snap (`concurrent map read and map write`, sem o rc no trace); o instalador espera o systemd religar |
| pod sem rede | `ip -n <netns> addr; ip -n <netns> route; tc -n <netns> filter show dev eth0 ingress` no nó |
| `kubectl top` vazio para o pod | o uso da VM vai para o 1º container da app; os outros mostram zero |
| sobrou VM depois de um crash do shim | o `delete` de limpeza do containerd apaga `/var/lib/rc-shim/pods/<id>`; a VM morre junto com o shim pelo pipe de vigia |

## Limites

| O quê | Por quê |
|---|---|
| `kubectl port-forward` | o containerd disca `127.0.0.1` no netns do pod, e o programa escuta dentro da VM |
| `hostNetwork`, `hostIPC`, `hostPID`, propagação de montagem | a VM tem kernel próprio |
| volume que muda depois (token de serviceaccount, configmap, secret atualizado) | os volumes são copiados no `Create` do container |
| limites por container, `kubectl top` por container | sem cgroup por container dentro da VM; limite e uso são da VM |
| securityContext (capabilities, seccomp do pod) | o processo roda como o usuário da spec, em chroot, sem crun |
| Landlock/seccomp no vmm do pod | a política das VMs de job não prevê o netns nem o share do pod (dívida declarada) |
| IPv6 do pod | só o IPv4 da `eth0` é lido |
| `linux/arm64` | compila; não foi medido com KVM |

## Mapa do código

| Caminho | O quê |
|---|---|
| [`cmd/containerd-shim-rc-v2/main.go`](../cmd/containerd-shim-rc-v2/main.go) | liga as peças: plugin TTRPC, config, tradução do start antigo |
| [`internal/adapters/k8sshim/`](../internal/adapters/k8sshim/) | bundle OCI, serviço de task, VM do pod, montagens, stdio, rootfs, manager, uso, start antigo |
| [`internal/adapters/podnet/`](../internal/adapters/podnet/) | TAP no netns do pod, tc mirred e a bomba de quadros |
| [`internal/guest/proc.go`](../internal/guest/proc.go) | `OpProc`: processo de container em chroot, com stdio, TTY e sinais |
| [`internal/adapters/krun/vmm_net.go`](../internal/adapters/krun/vmm_net.go) | `startPodNet`: a rede do pod no processo vmm (`VMSpec.PodNetns`) |
| [`deploy/install-node.sh`](install-node.sh), [`k3s/`](k3s/), [`microk8s/`](microk8s/) | instaladores |
| [`deploy/lib.sh`](lib.sh), [`deploy/k8s/e2e.sh`](k8s/e2e.sh) | RuntimeClass, pod de prova e cabeçalho do runtime; o e2e |
| [`scripts/k8s-node.sh`](../scripts/k8s-node.sh), [`k8s-e2e.sh`](../scripts/k8s-e2e.sh), [`k8s-defects.py`](../scripts/k8s-defects.py), [`do-kvm-probe.sh`](../scripts/do-kvm-probe.sh) | nós de teste, prova de ponta a ponta, defeitos plantados, medição de KVM |
| [`docs/stories/8.1.kubernetes-runtime.md`](../docs/stories/8.1.kubernetes-runtime.md) | a story, com critérios de aceite e registro do desenvolvimento |
