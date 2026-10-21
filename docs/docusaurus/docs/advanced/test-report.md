---
title: 'Relatório de testes por plataforma'
description: 'Testes ponta a ponta do rc no macOS, no Linux e no Windows: ambiente, método, números medidos, defeitos achados e o que não foi testado'
---

# Relatório de testes por plataforma

Última rodada completa: **20/10/2026**, binário `rc` sobre libkrun + libkrunfw. As medições de base (boot, memória,
build de imagem, execução por stack) são de **19/10/2026**, no mesmo host. Todo número desta página foi medido; o que
não foi testado está dito como não testado.

## Rodada de 20/10/2026 (macOS 27, Apple M1 Pro)

| Gate | Resultado |
|---|---|
| `task test` | 20 pacotes, todos verdes (unidade com `-race`) |
| `task test:cov` | **95,2%** total, domínio **100%**, guest **75%** (catraca em 75), SDK Python **100%** em linha e branch |
| `task test:e2e` | **42 de 42** testes verdes, em duas rodadas seguidas |
| `task runbook` | **126** conferências, **0** falhas |
| `task guides` | **9** pastas, **175** conferências, **0** falhas |
| `task test:targets` | darwin/arm64, linux/amd64 e linux/arm64 compilam; windows/amd64 recusado com o motivo |

No Linux, Landlock + seccomp foram testados com o processo confinado de verdade no Ubuntu 26.04, numa VM sem KVM; uma
microVM completa no Linux **não foi medida**. `--nested` só teve a **recusa** medida (este Mac é M1); o caminho que
liga não foi medido.

> **Atualização da mesma data:** uma microVM completa **foi medida** no Linux x86_64 com KVM (droplet da
> DigitalOcean, KVM aninhado), com o job sob Landlock (por thread, ABI 4) + seccomp, e o runtime de Kubernetes passou
> 19/19 no k3s e no microk8s. Detalhes em [Linux com KVM e Kubernetes](#linux-com-kvm-e-kubernetes).

## Resumo por plataforma

| Plataforma | Ambiente | microVM real | Resultado |
|---|---|---|---|
| **macOS** | MacBook M1 Pro, macOS 27.0.1, Hypervisor.framework | **sim** | e2e 42/42 em duas rodadas seguidas; runbook e guias sem falha |
| **Linux** | Ubuntu 26.04.1 LTS no VMPal, kernel 7.0.0-30, aarch64 | **não** (sem `/dev/kvm`) | confinamento Landlock + seccomp aplicado e provado no processo real; 30 testes verdes em duas rodadas |
| **Linux com KVM** | droplet DigitalOcean `s-4vcpu-8gb`, Ubuntu 24.04, kernel 6.8, x86_64, KVM aninhado | **sim** | job Python sob Landlock (ABI 4, por thread) + seccomp; Kubernetes 19/19 no k3s 1.36 e no microk8s 1.35, duas rodadas |
| **Windows** | Windows 11 Pro 24H2 (build 26100.9457) ARM64 no VMPal | **não** | fora do escopo: build recusado de propósito; hypervisor não sobe na VM (`EL2 not present`) |

O motivo comum do "não" no Linux e no Windows: as duas são VMs dentro de um **M1 Pro**. O Virtualization.framework
só entrega o nível de exceção EL2 (necessário para um hypervisor dentro da VM) a partir do **M3**. Sem EL2, nenhuma
VM aninhada sobe, seja KVM ou Hyper-V.

## macOS (alvo medido de ponta a ponta)

### Ambiente

| Item | Valor |
|---|---|
| hardware | MacBook Pro M1 Pro (arm64) |
| sistema | macOS 27.0.1 |
| VMM | libkrun 1.19.6 (derivação Nix do repo, `nix/`) |
| kernel do guest | libkrunfw 5.6.2 → Linux 6.12.109 aarch64 |
| binário | `bin/rc`, assinado ad-hoc com `com.apple.security.hypervisor` |
| guest | `bin/rc-guest-linux-arm64`, estático, 2,6 MB |

### Boot e recursos (spike, 30 boots por medida)

Medido com um harness que dispara o processo da VM e cronometra até a primeira resposta de um job pelo vsock.

| Medida | Valor |
|---|---|
| boot até a 1ª resposta Python, root ext4 read-only | p50 **260 ms**, p95 **266 ms** (mín. 254, máx. 279) |
| boot até a 1ª resposta `sh`, root por virtiofs | p50 245 ms, p95 264 ms |
| ciclo completo spawn → `uname` → desligar | 221–252 ms com cache quente; 869 ms no primeiro |
| footprint de VM parada (512 MiB configurados) | **44 MB** (`footprint`); RSS 172 MB inclui páginas limpas compartilhadas |
| RAM configurada 256 / 1024 / 2048 MiB | boot 260 / 260 / 261 ms; RSS +40 MB no total |
| `SIGKILL` no grupo do processo | nenhum processo sobrevive (todos os boots medidos) |

Decisão tirada daqui: root em disco ext4 read-only. O virtiofs não foi mais rápido de forma relevante e expõe um
diretório do host ao guest.

### Rede

| Configuração | `wget http://1.1.1.1` | `nc 1.1.1.1 80` |
|---|---|---|
| vsock sem TSI (padrão das VMs de job) | `Network unreachable` | rc=1 |
| vsock com TSI (só a VM de build) | conecta | rc=0 |

Prova nos dois sentidos: o teste usa IP, porque com nome a falha de DNS esconderia a diferença.

### Build das imagens (15 stacks, `rc image build --all`)

| Stack | Imagem OCI | Build | ext4 |
|---|---|---|---|
| awk | alpine:3.22 | 2,6 s | 45 MiB |
| bash | bash:5.2 | 2,9 s | 57 MiB |
| c | alpine:3.22 + build-base | 19,8 s | 332 MiB |
| cpp | alpine:3.22 + build-base | 19,5 s | 332 MiB |
| go | golang:1.25-alpine | 35,3 s | 377 MiB |
| java | eclipse-temurin:21-jdk-alpine | 34,0 s | 478 MiB |
| jq | alpine:3.22 + jq | 2,7 s | 47 MiB |
| nodejs | node:22-alpine | 14,1 s | 234 MiB |
| perl | alpine:3.22 + perl | 8,4 s | 98 MiB |
| php | php:8.3-cli-alpine | 9,4 s | 152 MiB |
| python | python:3.12-alpine | 8,2 s | 128 MiB |
| ruby | ruby:3.3-alpine | 8,2 s | 134 MiB |
| rust | rust:1-alpine | 37,4 s | 1,2 GiB |
| sh | alpine:3.22 | 2,2 s | 45 MiB |
| sqlite | alpine:3.22 + sqlite | 2,8 s | 49 MiB |

O `mkfs.ext4` domina o tempo das imagens grandes (27,8 s dos 35 s do Go): o ext4 é escrito no host via virtiofs.
Pull e achatamento do `python:3.12-alpine`: 2,7 s, 55 MB.

### Execução por stack (`rc run`, boot frio)

| Stack | Execução | Total (com boot) |
|---|---|---|
| python | 31–44 ms | ~240 ms |
| nodejs | 128–148 ms | 353–371 ms |
| go | **247 ms** com o cache aquecido (3.429 ms sem ele) | 510 ms |
| java (JDK 21.0.12) | 522 ms | 739 ms |
| c | 71–72 ms | 282 ms |
| sh | 2 ms | 199 ms |

### API com pool pré-bootado

| Situação | Requisição HTTP completa |
|---|---|
| 1 stack com imagem, pool de 2 | 26–43 ms |
| 15 stacks, outras ainda repondo o pool | 73–78 ms |
| 15 stacks, todos os pools cheios | 49–175 ms |
| antes do teto global de boots (30 VMs bootando juntas) | 1,7–2,3 s (regressão achada e corrigida) |

### Suíte e2e (`task test:e2e`, tag `e2e`)

42 testes, rodados duas vezes seguidas e verdes nas duas. Cada teste trava um comportamento medido. A tabela abaixo
descreve os que fixam o contrato dos jobs e das máquinas; os demais estão na tabela seguinte.

| Teste | O que prova |
|---|---|
| `TestStdoutAndExitCode` | saída correta; `exit 3` do programa vira rc≠0 da CLI |
| `TestTimeoutKillsVM` | laço infinito estoura em ~2,2 s e nenhum processo de VM sobra |
| `TestNoNetwork` | conexão a 1.1.1.1:80 bloqueada. **Prova de falha:** com `Network: true` plantado no pool o teste reprovou (a sonda Python imprimiu que conectou; a mensagem estava em português na época e hoje é `guest reached the network: "CONNECTED\n"`); restaurado, passou |
| `TestRootReadOnlyAndIsolation` | `/etc` não aceita escrita; arquivo gravado em `/tmp` por um job não existe no seguinte |
| `TestMemoryCeiling` | alocar 2 GiB numa VM de 512 MiB falha dentro da VM sem afetar o host |
| `TestLanguageFamilies` | 13 stacks executam (nodejs, go, java, c, cpp, rust, ruby, php, perl, bash, awk, jq, sqlite); compilação Java falha sai rc≠0 |
| `TestShell` | `rc shell` com stdin por pipe sobe com root read-only, `/tmp` gravável e sem rede; na stack bash é bash 5.2 de verdade |
| `TestShellTTY` | `rc shell` sob pseudo-terminal real (`creack/pty`, 40×120): o stdin do guest é terminal, `stty size` devolve `40 120`, e Ctrl+C interrompe um `sleep 30` sem derrubar a VM (o teste leva ~1,4 s; sem a interrupção levaria 30 s). **Prova de falha:** sem o modo raw no host o teste reprovou (`VM survived the shell exit`); restaurado, passou |
| `TestServePool` | o `serve` enche os pools, atende `/run` e, no SIGTERM, não deixa VM viva |
| `TestRunStreams` | `rc run` com Python que escreve, dorme 1,5 s e escreve de novo: a 1ª linha chega ao stdout do host ≥ 1 s antes da 2ª (medido: 0,25 s e 1,75 s depois do início). **Prova de falha:** com o `rc run` acumulando a saída, as duas linhas chegaram com 3,8 µs de distância e o teste reprovou |
| `TestServeStream` | `POST /api/v1/execute/stream` com a VM real: `text/event-stream`, eventos `stdout` com a 1ª linha ≥ 1 s antes da 2ª, `result` final com `exitCode` 0; job recusado responde 400 antes do stream |
| `TestMachineLifecycle` | `rc machine`: create, CONFLICT no nome repetido, INVALID_STATE no exec com a máquina parada, start (198–244 ms), gravação em `/etc`, `/opt` e `/workspace`, rc do exec repassado (7), Python da stack rodando dentro, stop, logs com a máquina parada, start de novo: tudo persistiu e `/tmp` voltou vazio; delete não deixa VM. **Prova de falha:** com a máquina subindo sem o `machine.sh`, a gravação em `/etc` falhou e o teste reprovou |
| `TestMachineShellTTY` | `rc machine shell` sob pseudo-terminal real (40×120) numa máquina ligada: o shell vê um TTY e `40 120`, Ctrl+C interrompe um `sleep 30` sem derrubar o shell, o que o shell grava em `/workspace` aparece para o exec, `exit 4` vira rc 4 e a máquina continua ligada. **Prova de falha:** sem o devpts no `machine.sh` (o defeito real encontrado: `open /dev/ptmx: no such device`), o shell não abriu e o teste reprovou |
| `TestRunnerCode` | máquina de um `runner.code` com `--cpus 1` por cima do arquivo (a flag vence); o workdir `/app` nasce no start; o workload sobe com env e workdir e aparece no `logs --source workload`; o init roda uma vez (contador em 1 depois de stop+start); o stop manda `SIGINT` e o trap do workload grava antes do sync. **Prova de falha (dois defeitos plantados):** stop mandando SIGTERM no lugar do `stop_signal` → o arquivo do trap não existe; init ignorando o marcador → contador em 2. Os dois reprovaram |
| `TestRunnerCodeStopOnExit` | com `stop_on_exit`, o fim do workload desliga a máquina sozinha (~0,5 s) e o log mostra a saída e `exited with code 0` |
| `TestRunnerCodeRefusals` | `net = true` recusado com o motivo e nada criado; sem stack nem image é erro |
| `TestRunnerCodeImage` | `--image alpine:3.20` (imagem fora do catálogo): construída, liga e roda `cat /etc/alpine-release` |
| `TestMachineNoNetwork` | máquina sem `--net` não tem `eth0` e não sai |
| `TestMachineNetwork` | com `--net`: `eth0` `100.96.0.2/30`, DNS no gateway, `example.com` responde; metadata `169.254.169.254` recusada na hora mesmo com a rede aberta |
| `TestMachineEgressPolicy` | `--allow-host example.com`: o nome passa, `iana.org` dá `bad address` em menos de 1 s e vai para o `egress-denials.log`; `machine update` libera e remove com a máquina ligada. **Prova de falha:** com o gateway ignorando a política, metadata e nome bloqueado passaram e o teste reprovou |
| `TestMachinePortsAndVolumes` | `runner.code` com volume rw e ro, porta publicada e sem `--net`: o workload serve o arquivo do host pela porta, a escrita do guest aparece no host, o `:ro` recusa escrita **mesmo depois de `mount -o remount,rw` pelo root do guest**, o guest não disca para fora, e os volumes voltam a cada start. **Prova de falha:** com o `:ro` só na opção de montagem (sem o flag no device), o remount abriu o volume e o teste reprovou. Antes dessa checagem, esse defeito passava batido |
| `TestMachineCopy` | `rc machine cp` de 5 MiB (vários pedaços) para a máquina e de volta: sha256 igual dentro e fora |
| `TestMachineAPI` | rotas `/api/v1/machines`: 201/409/200/204/404, exec com rc 4, exec/stream com a 1ª linha ≥ 1 s antes da 2ª e `exit` no fim, PUT/GET de 2,6 MB idênticos, INVALID_STATE (409) com a máquina parada, stop em menos de 5 s. **Prova de falha:** com a VM destacada sem Wait (zumbi no `rc serve`), o stop levou 10 s e o teste reprovou |
| `TestGPUVulkan` | `--gpu` na stack `llama`: `vulkaninfo` mostra `Virtio-GPU Venus (Apple M1 Pro)`; sem `--gpu` não existe `/dev/dri`; `--gpu` em stack sem GPU é recusado com E007. **Prova de falha:** com o `Configure` sem a chamada `krun_set_gpu_options2`, o teste reprovou (`no Venus device with --gpu`) |
| `TestGPULlama` | `llama-bench` com o mesmo modelo na GPU (`-ngl 99 -dev Vulkan0`) e na CPU: o device usado é o Venus e o prompt sai ≥ 2× mais rápido (medido 893–1.365 t/s contra 193–264 t/s, 4 rodadas) |

Testes que entraram com as stories 7.4 a 7.6 e com os guias:

| Teste | O que prova |
|---|---|
| `TestMachineRun` | `machine run`: o comando roda numa máquina que some no fim, o rc é o do comando, `-i` repassa o stdin, sem nada para rodar é erro antes de criar |
| `TestMachineRunDetached` | `-d` deixa a máquina ligada com o workload sob o nome pedido |
| `TestMachineRunTTY` | `-it` sob pseudo-terminal real abre o shell com TTY; o `exit` dele vira o rc e a máquina some |
| `TestMachineRunCallerKilled` | um `machine run` cujo CLI leva SIGKILL não deixa a VM viva sem dono (antes da correção, ficava) |
| `TestMachineBranchAndPause` | pausada, a máquina recusa exec com o motivo; o branch leva o que a mãe acabou de gravar; mãe e filha seguem independentes; `resume` devolve a mãe |
| `TestMachineEgressCIDROnly` | com só `allow_cidrs`, um nome resolvido não abre conexão (furo fechado em 19/10/2026); o CIDR liberado continua alcançável |
| `TestMachinePing` | ICMP passa pelo gateway sob a mesma política do TCP; destino fora da allowlist não volta e fica no log de negações |
| `TestNamedNetworkMachines` | duas máquinas da mesma rede se alcançam pelo IP e por `<máquina>.<rede>.internal`, sem `--net`; outra rede não alcança; máquina parada some do DNS |
| `TestMachineCredential` | o guest só vê o placeholder e o serviço HTTPS recebe o valor real; placeholder na URL é recusado com 403; host fora da binding recebe o placeholder literal |
| `TestSecretsAndSSHAgent` | `--secret-env`/`--secret-file` chegam ao exec e não ficam no registro; variável ausente falha o start; `--ssh-agent` lista a chave do host sem a chave privada entrar na VM |
| `TestExposeAndMountSockets` | `--expose-socket` e `--mount-socket`, nas duas direções |
| `TestDockerSocketAndDiskVolume` | `docker_socket` responde no diretório da máquina; volume de disco é ext4 de verdade, fora do overlay, e persiste |
| `TestStagedVolume` | `:staged`: o host não muda até o sync; o sync espelha; o stop gracioso sincroniza; a filha de um branch não devolve nada |
| `TestMachineS3Volume` | volume `s3://` contra um servidor S3 real: leitura, gravação byte a byte (8 MiB), `mv` e `rm`, `:ro`, remontagem após stop/start, credencial errada falha o start com o motivo |
| `TestMachineDisplay` | o quadro do guest chega ao visualizador do host com as cores certas; tecla e clique por `POST /input` chegam ao evdev do guest |
| `TestPack` | stack em stub + payload; máquina em arquivo único lida com a origem apagada; modo persistente; `create --from` e `pack inspect`; recusas |
| `TestDevStdinLinks` | `/dev/stdin`, `/dev/stdout`, `/dev/stderr` e `/dev/fd` existem na VM de job e na máquina |

### `rc shell`

| Verificação | Resultado |
|---|---|
| python: `python3`, `/tmp` gravável, `/etc` read-only, sem rede | ok |
| go: `go version` com o PATH da imagem | `go1.25.14 linux/arm64` |
| bash: prompt e `[[ ]]` | `bash-5.2#`, ok |
| sob pseudo-terminal | stdin do guest = `/dev/console` sobre o `hvc0`; `test -t 0` verdadeiro; `stty size` = tamanho do terminal do host |
| Ctrl+C | interrompe o comando no guest; a VM segue viva |

Como funciona: com terminal no stdin, o `rc shell` liga `tty` na spec (`krun_disable_implicit_console` +
`krun_add_virtio_console_default(0, 1, 2)`; com os três fds sendo TTY a libkrun cria uma porta de console só) e
põe o terminal do host em modo raw durante a sessão, restaurando antes de sair. Com stdin por pipe o comportamento
continua o de antes (console implícito, sem TTY). O comando `tty` ainda imprime `not a tty`, porque ele não acha
o nome do dispositivo; `test -t 0` e `stty` mostram que é terminal.

**Evento de carga:** uma rodada da suíte com `-count=2` falhou em 6 de 9 testes com load average ~24 no host
(VMs do VMPal a 88% de CPU e `nix-daemon` a 79%); `TestServePool` levou 40 s contra 7–8 s normais. Isoladas e
em duas rodadas seguintes, as mesmas suítes passaram inteiras.

### Números do lote de 20/10/2026

| Medida | Valor |
|---|---|
| pack | imagem python 134 MiB → payload de 31 MiB; máquina com pacote instalado, arquivo único: 46 MiB |
| volume S3 | 8 MiB gravados no guest chegam ao bucket com o mesmo md5 (servidor S3 local) |
| defeito de segurança achado | `allow_cidrs` sozinho deixava sair para qualquer nome; corrigido e travado por `TestMachineEgressCIDROnly` |

## Linux (confinamento testado; microVM não)

### Ambiente

| Item | Valor |
|---|---|
| VM | "Ubuntu VM 2" no VMPal, 4 vCPU, 7,9 GB |
| sistema | Ubuntu 26.04.1 LTS, kernel 7.0.0-30-generic, aarch64 |
| LSMs ativos | `lockdown,capability,landlock,yama,apparmor,ima,evm` |
| Landlock | **ABI 8** (`landlock_create_ruleset(NULL, 0, LANDLOCK_CREATE_RULESET_VERSION)`) |
| `/dev/kvm` | **ausente** (sem EL2 aninhado no M1 Pro) |

### Método

O binário de testes do pacote `internal/adapters/krun` é compilado no Mac para `linux/arm64` e executado na VM:

```bash
CGO_ENABLED=0 GOOS=linux GOARCH=arm64 go test -c -cover -o krun.test ./internal/adapters/krun/
vmpal cp krun.test "Ubuntu VM 2:rc-sandbox/krun.test"
# na VM:
./krun.test -test.v -test.count=1 -test.coverprofile=cover.out
```

O teste do confinamento relança o próprio binário como **sonda**. A sonda aplica a política real
(`ApplySandbox`, a mesma função que o processo `vmm` chama) e tenta cada operação. O processo de teste em si
nunca é confinado.

### Resultado do confinamento (sonda confinada)

| Operação | Esperado | Medido |
|---|---|---|
| ler o disco root | permitido | permitido |
| criar o socket da própria VM (`bind`) | permitido | permitido |
| ler arquivo fora da política | negado | **negado** (Landlock) |
| gravar ao lado do disco root | negado | **negado** (Landlock) |
| `connect` TCP | negado | **negado** (Landlock, rede) |
| `execve("/bin/true")` | negado | **negado** (seccomp, `EPERM`) |
| `personality(0xffffffff)` | negado | **negado** (seccomp, `EPERM`) |
| `connect` no socket Unix de outra VM | negado a partir da ABI 9 | **permitido** (kernel com ABI 8) |

**Prova de falha** (`TestProbeWithoutSandbox`): a mesma sonda sem confinamento lê o arquivo proibido, grava ao lado
do disco, conecta no socket de outra VM, abre TCP e chama `personality`. Ou seja, o teste distingue.

`ptrace` não é usado como prova de seccomp: no Ubuntu o Yama já nega `ptrace` ao pai sem confinamento nenhum.

### Resultado da suíte no Linux

30 testes, verdes em duas rodadas seguidas (`TestLoadRealLibrary` pulado: sem libkrun na VM). Cobertura do pacote:
81,9%. O código executado dentro do subprocesso confinado não entra na medição.

### `rc doctor` na VM

```
FAIL  platform     no access to /dev/kvm: open /dev/kvm: no such file or directory
ok    sandbox landlock ABI 8 + seccomp denylist
warn  landlock ABI 8 < 9: a VM process can still connect to another VM's unix socket
```

### Não testado no Linux

- uma microVM completa (boot, job, timeout) sob Landlock + seccomp: exige KVM;
- a lista de caminhos que a libkrun abre em `krun_start_enter` no Linux. A política libera as bibliotecas
  carregadas, `/proc`, `/sys`, `/dev/kvm` e os nós de dispositivo comuns, e pré-carrega a libkrunfw. Se a libkrun
  precisar de outro caminho, a VM não sobe; `RUNNER_SANDBOX=off` desliga o confinamento para diagnóstico;
- `linux/amd64` (só compilado).

## Linux com KVM e Kubernetes

### Ambiente

| Item | Valor |
|---|---|
| máquina | droplet DigitalOcean `s-4vcpu-8gb` (4 vCPU, 8 GB), nyc3; `scripts/do-kvm-probe.sh` mediu `/dev/kvm` e as flags `vmx` antes de criar o nó de teste |
| sistema | Ubuntu 24.04, kernel 6.8.0-142, x86_64; Landlock **ABI 4** |
| libkrun | 1.19.5 e libkrunfw 5.5.0 do nixpkgs (`task krun:install`) |
| Kubernetes | k3s v1.36.5 (containerd 2.3.4, flannel) e microk8s v1.35.6 (containerd 2.1.6, Calico), um droplet cada |

### Resultado

| O quê | Medido |
|---|---|
| `rc run --stack python`, boot frio | `status=success`, ~1,4 s de ponta a ponta (KVM aninhado; no Mac, ~240 ms) |
| `rc image build --stack python` | 60 s (o `mkfs` levou 45 s) |
| pod `runtimeClassName: rc` até `Running` | ~2 s, imagem já no nó |
| `deploy/k8s/e2e.sh` | **19/19** no k3s e no microk8s, duas rodadas seguidas em cada |
| prova de falha do e2e | código de saída fixo em 0 no shim: as 2 checagens de código de saída reprovaram |
| `task k8s:defects` | 20 defeitos plantados; 19 pegos na 1ª rodada, o que passou batido virou teste, 20 de 20 depois |
| `TestRestrictThreadBlocks` (root, kernel real) | Landlock por thread barra o arquivo de fora; sem o `landlock_restrict_self` o teste reprova |
| `TestAttach` (root, netns de verdade) | tap0 nasce no netns do pod e não no do host, com um filtro mirred em cada sentido |

### Defeitos achados só no Linux real

| Sintoma | Causa | Correção |
|---|---|---|
| `nix profile add .#krun` falha com `File exists` | na libkrun do nixpkgs, `lib/` e `lib64/` trazem os mesmos arquivos | `ln -sf` no `flake.nix` |
| todo job falha: `prctl(PR_SET_NO_NEW_PRIVS): operation not supported` | abaixo da ABI 8 o go-landlock usa psx, e sem cgo o `AllThreadsSyscall` devolve ENOTSUP (o purego faz o runtime se ver "com cgo") | ruleset na thread do `krun_start_enter` (`sandbox_rules.go`); as threads da libkrun herdam |
| teste do launcher não compilava no Linux | `syscall.Getsid` só existe no BSD/macOS | `unix.Getsid` |
| cópia do rootfs "falha" com `waitid: no child processes` | o pkg/shim colhe todo filho no SIGCHLD | filho do shim espera pelo `reaper.Default` |
| resumo do kubelet quebra no nó inteiro; namespace preso em Terminating | `Stats` vazio do shim (`failed to decode container metrics`) | métricas cgroup v2 com o uso real do vmm |
| microk8s: `unsupported protocol: Yunix` | o containerd 2.1 lia a resposta protobuf do start como endereço | JSON antigo para quem chama pelo protocolo antigo |
| microk8s: pod sem rede (`Network is unreachable`) | Calico dá `/32` com gateway `169.254.1.1` fora da sub-rede | rota de link para o gateway antes da padrão |

### Não testado no Linux com KVM

- `linux/arm64` com KVM (só x86_64);
- Landlock com ABI ≥ 8 (caminho TSYNC) com uma microVM real: o droplet tem ABI 4;
- `--nested` ligando (o `rc doctor` do droplet diz `supported`, sem prova);
- o vmm da VM de pod roda **sem** Landlock/seccomp (dívida declarada).

## Windows (fora do escopo, medido)

### Ambiente

| Item | Valor |
|---|---|
| VM | "Windows VM 1" no VMPal |
| sistema | Windows 11 Pro 24H2, build 26100.9457, ARM64, CPU "Apple Silicon" |

### Build do `rc`

```
GOOS=windows GOARCH=arm64|amd64 go build ./cmd/rc
undefined: runnerCodesDoesNotSupportWindows_libkrunUpstreamHasNoWHPBackend
```

A recusa é deliberada: a libkrun upstream só tem backend KVM e Hypervisor.framework, sem Windows Hypervisor
Platform.

### Hypervisor dentro da VM

| Passo | Medido |
|---|---|
| estado inicial | `HypervisorPresent=False`; `VirtualizationFirmwareEnabled`, `VMMonitorModeExtensions` e `SLAT` todos `False`; WSL não instalado |
| `vmpal set --nested on` | Windows: `nested virtualization is for Linux VMs`; Ubuntu: `Nested virtualization needs a Mac with M3 or later` |
| ponto de restauração do VMPal `antes-hypervisor-2026-10-19` | criado |
| `Enable-WindowsOptionalFeature` HypervisorPlatform, VirtualMachinePlatform, Microsoft-Hyper-V-All | habilitados, `RestartNeeded=True` |
| após reiniciar | `bcdedit hypervisorlaunchtype Auto`; `vmms` e `vmcompute` rodando; **`HypervisorPresent=False`** |
| log de sistema | `Microsoft-Windows-Hyper-V-Hypervisor id=43: Hypervisor launch failed; EL2 not present.` |
| revertido ao ponto de restauração | os três recursos `Disabled`; `vmms` e `vmcompute` ausentes |

Conclusão: sem EL2 a VM não roda hypervisor nenhum, e não há configuração do Windows que contorne isso. Mesmo com
suporte do `rc`, essa VM não teria onde criar microVMs.

## O que falta para fechar a matriz

| Lacuna | O que destrava |
|---|---|
| microVM sob Landlock com TSYNC (ABI ≥ 8) | Linux com KVM e Landlock ABI ≥ 8 (o Ubuntu 26.04, kernel 7.0, tem a 8) (a ABI 4 do droplet já foi medida, pelo caminho por thread) |
| `linux/arm64` com KVM em execução | Linux arm64 com KVM (o `linux/amd64` foi medido no droplet) |
| isolamento de socket Unix entre VMs | kernel com Landlock ABI ≥ 9 |
| `--nested` ligando de verdade | Mac M3+ ou Linux com KVM aninhável |
| Windows nativo | trocar a libkrun upstream pelo fork com WHP (decisão de arquitetura) e máquina Windows com virtualização |
