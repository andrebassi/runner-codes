---
title: 'Relatório de testes por plataforma'
description: 'Testes ponta a ponta do rc no macOS, no Linux e no Windows: ambiente, método, números medidos, defeitos achados e o que não foi testado'
---

# Relatório de testes por plataforma

Testes executados em **19/10/2026** contra o código da migração Firecracker → libkrun (binário `rc`).
Todo número desta página foi medido; o que não foi testado está dito como não testado.

## Resumo

| Plataforma | Ambiente | microVM real | Resultado |
|---|---|---|---|
| **macOS** | MacBook M1 Pro, macOS 27.0.1, Hypervisor.framework | **sim** | suíte e2e (9 testes, 15 stacks) verde em duas rodadas seguidas |
| **Linux** | Ubuntu 26.04.1 LTS no VMPal, kernel 7.0.0-30, aarch64 | **não** (sem `/dev/kvm`) | confinamento Landlock + seccomp aplicado e provado no processo real; 30 testes verdes em duas rodadas |
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
| VMM | libkrun 1.19.6 (`brew tap slp/krun`) |
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

Rodada duas vezes seguidas (`-count=2`), verde nas duas. Cada teste trava um comportamento medido.

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

### Gates

| Gate | Resultado |
|---|---|
| `task test:cov` | 95,8% total, domínio 100%, guest 36,2% (catraca 36%). `COVERAGE_FLOOR=101` reprova |
| `task test:targets` | darwin/arm64, linux/amd64 e linux/arm64 compilam; windows/amd64 recusado com o motivo |
| `go test -race ./internal/...` | verde |

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

A recusa é deliberada: a libkrun upstream só tem backend KVM e Hypervisor.framework. O suporte ao Windows
Hypervisor Platform existe apenas no fork `libkrun`.

### Hypervisor dentro da VM

| Passo | Medido |
|---|---|
| estado inicial | `HypervisorPresent=False`; `VirtualizationFirmwareEnabled`, `VMMonitorModeExtensions` e `SLAT` todos `False`; WSL não instalado |
| `vmpal set --nested on` | Windows: `nested virtualization is for Linux VMs`; Ubuntu: `Nested virtualization needs a Mac with M3 or later` |
| snapshot `antes-hypervisor-2026-10-19` | criado |
| `Enable-WindowsOptionalFeature` HypervisorPlatform, VirtualMachinePlatform, Microsoft-Hyper-V-All | habilitados, `RestartNeeded=True` |
| após reiniciar | `bcdedit hypervisorlaunchtype Auto`; `vmms` e `vmcompute` rodando; **`HypervisorPresent=False`** |
| log de sistema | `Microsoft-Windows-Hyper-V-Hypervisor id=43: Hypervisor launch failed; EL2 not present.` |
| revertido ao snapshot | os três recursos `Disabled`; `vmms` e `vmcompute` ausentes |

Conclusão: sem EL2 a VM não roda hypervisor nenhum, e não há configuração do Windows que contorne isso. Mesmo com
suporte do `rc`, essa VM não teria onde criar microVMs.

## O que falta para fechar a matriz

| Lacuna | O que destrava |
|---|---|
| microVM sob Landlock + seccomp | Linux com KVM: Mac M3+ (VMPal com `--nested on`), Linux x86 físico ou EC2 metal |
| `linux/amd64` em execução | Linux x86 com KVM |
| isolamento de socket Unix entre VMs | kernel com Landlock ABI ≥ 9 |
| Windows nativo | trocar a libkrun upstream pelo fork com WHP (decisão de arquitetura) e máquina Windows com virtualização |
