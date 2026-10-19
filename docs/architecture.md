# runner.codes — Arquitetura (libkrun)

## Introduction

Este documento descreve como o runner.codes executa código não confiável em microVMs **libkrun + libkrunfw**.
Ele substitui o desenho anterior, baseado em Firecracker. Não há starter template: é migração de um projeto
existente (brownfield).

### Change Log

| Date | Version | Description | Author |
|---|---|---|---|
| 2026-10-19 | 0.1 | Arquitetura inicial libkrun, com os números do spike (story 1.1) | @architect |
| 2026-10-19 | 0.2 | Guest como binário próprio, contrato real do /execute, números de produção | @architect |

## High Level Architecture

### Technical Summary

Dois binários Go:
- **`runner.code`** (host): API HTTP + pool de VMs, CLI, build de imagem e o subcomando `vmm` (um processo por VM, que carrega a libkrun por purego e chama `krun_start_enter`);
- **`runner-guest`** (dentro da VM): o guest-runner. É separado porque o purego força link dinâmico contra a glibc no Linux, e um guest dinâmico numa imagem Alpine (musl) falha com `not found` (medido).

O host fala com o guest pelo protocolo atual (4 bytes BE + JSON), num socket Unix que a libkrun liga à porta
vsock 5000 do guest. Cada linguagem tem um ext4 read-only gerado a partir da imagem OCI oficial, já aquecido.
O pool mantém VMs bootadas, cada uma usada uma única vez.

### Platform and Infrastructure

| Alvo | Hypervisor | libkrun / libkrunfw | Situação |
|---|---|---|---|
| macOS arm64 | Hypervisor.framework | Nix, derivação própria (`nix/`): 1.19.6 / 5.6.2 | medido |
| Linux x86_64 / aarch64 | KVM | nixpkgs: 1.19.5 / 5.5.0 | não medido ainda |
| Windows | — | sem backend WHP no upstream | fora do escopo |

### Repository Structure

```
cmd/rc/      host: serve, run, vmm, image build, languages, doctor
cmd/runner-guest/        guest estático (roda dentro da VM)
internal/domain/         Job, Result, Language, validação
internal/ports/          interfaces VMLauncher, Pool, ImageStore, Catalog, GuestClient
internal/app/            ExecuteService, PoolManager
internal/adapters/krun/  binding purego + processo vmm + sandbox
internal/adapters/guestproto/  cliente do protocolo vsock
internal/adapters/oci/   pull/flatten OCI e build do ext4 na VM de build
internal/adapters/catalog/     languages.yaml embutido
internal/adapters/httpapi/     API Gin
internal/guest/          servidor e executor dentro da VM (ex-pkg/guest)
```

### Diagram

```
 cliente ──HTTP──▶ serve ──▶ ExecuteService ──▶ PoolManager ──spawn──▶ rc vmm (1 por VM)
                                  │                                        │ purego
                                  │                                        ▼
                                  │                                   libkrun ── libkrunfw (kernel 6.12)
                                  │                                        │ HVF / KVM
                                  └──socket Unix──(vsock 5000)──▶ guest: /opt/runner/guest
                                                                    root /dev/vda ext4 ro, /tmp tmpfs
```

### Architectural Patterns

- **Hexagonal:** o domínio não importa libkrun, HTTP nem OCI; os adapters ficam atrás das ports.
- **Processo descartável por VM:** `krun_start_enter` assume o processo e sai com o código do workload.
  `SIGKILL` no grupo é o timeout.
- **Uso único:** nenhuma VM roda dois jobs. O pool só antecipa o boot.
- **Imutável + efêmero:** root read-only compartilhado; toda escrita vai para tmpfs dentro da VM.

## Tech Stack

| Category | Technology | Version | Purpose | Rationale |
|---|---|---|---|---|
| Linguagem | Go | 1.25+ (módulo) | host e guest | binário estático, cross-compile; rule 19 |
| VMM | libkrun | 1.19.6 (mac) / 1.19.5 (nix) | microVM | HVF + KVM nativos, boot ~260 ms medido |
| Kernel | libkrunfw | 5.6.2 → Linux 6.12.109 | kernel do guest | embutido, sem gerenciar vmlinux |
| FFI | ebitengine/purego | 0.11.2 | dlopen sem cgo | mantém `CGO_ENABLED=0` |
| OCI | go-containerregistry | 0.22.1 | pull e achatamento | Go puro, sem daemon |
| Imagem | e2fsprogs (na VM de build) | 1.47.2 | `mkfs.ext4 -d` | sem root; o Linux da VM diferencia maiúsculas |
| VM de build | Alpine minirootfs | 3.22.6 aarch64 | ferramentas de build | sha256 conferido |
| HTTP | gin | 1.9.1 | API | já usado |
| CLI | cobra | 1.8.0 | subcomandos | já usado |

## Data Models

- **Language:**
  - `Name`, `Image` (ref OCI);
  - `CPUs`, `MemoryMiB`: teto contra abuso;
  - `Warmup`: comando rodado em chroot no build;
  - `DiskSlackMiB`.
- **Job:** `TraceID`, `Language`, `Files[]` ou `Code`, `Stdin`, `Timeout`.
- **Result:** `Status`, `Stdout`, `Stderr`, `Exception`, `ExitCode`, `ExecutionTime`, `TimedOut`.
- **VM:** `ID`, `Language`, `Socket`, `PID`, `ReadyAt`. Ela pertence ao pool até ser entregue e morre depois do job.
- **ImageRecord:** `Language`, `Digest`, `Path` (`<cache>/images/<lang>-<digest>.ext4`), `BuiltAt`.

## API Specification

| Método | Rota | Corpo | Resposta |
|---|---|---|---|
| GET | `/health` | — | `{status, version}` |
| GET | `/api/v1/languages` | — | `[{name, image, ready}]` |
| POST | `/api/v1/run` | `{language, files[{name,content}], stdin}` | `{status, stdout, stderr, exception, executionTime, error}` |
| POST | `/api/v1/execute` | `{language, code, files[], stdin, timeout}` (timeout em segundos) | mesmo formato de `/run` |

O contrato de `/run` e `/execute` é o do `pkg/api/server.go` anterior: `status: "success"` diz que a API executou (o programa pode ter falhado), timeout vem em `exception: "Timeout"`, validação recusada é HTTP 400 (E005/E006) e falha de infraestrutura é 500. Não há autenticação. O servidor escuta em
`127.0.0.1` por padrão; expor fora do host é decisão de quem implanta, e fica dito na doc de segurança.

## External APIs

- **Registries OCI** (Docker Hub, ghcr.io): leitura anônima de manifest e blobs, usada só em `image build`.
  O Docker Hub limita pull anônimo por IP; o cache por digest evita pull repetido.

## Core Workflows

### Execução com pool cheio

```
cliente → POST /run → ExecuteService.Validate → Pool.Take(lang) → VM pronta
       → GuestClient.Run(sock, job) com deadline → Result
       → Pool.Destroy(vm) (SIGKILL no grupo) → Pool.Refill(lang) em background
```

### Boot de uma VM (medido)

1. `os/exec` de `rc vmm --spec <json>`, com `Setpgid`, `DYLD_LIBRARY_PATH` no mac e `Pdeathsig` no Linux.
2. `vmm`: `krun_create_ctx` → `set_vm_config` → `add_disk2(root, ro)` + `set_root_disk_remount(/dev/vda, ext4, ro)`
   → `disable_implicit_vsock` + `add_vsock(0)` → `add_vsock_port2(5000, sock, listen=true)`
   → `set_exec(/opt/runner/start.sh)` → `start_enter`.
3. O init da libkrun monta `/proc`. O `start.sh` monta tmpfs em `/tmp` e executa o guest.
4. O host tenta um round trip a cada 1 ms até ter resposta. Conectar não basta: o socket existe antes de o guest escutar.

### Build de imagem

```
host: go-containerregistry pull (linux/<arch>) → mutate.Extract → <work>/<lang>.tar
VM de build (Alpine, virtiofs, TSI só aqui): tar -xp em tmpfs → injeta guest + start.sh
       → chroot warmup → mkfs.ext4 -d → <work>/<lang>.ext4
```

### Números do spike (macOS 27.0.1, Apple Silicon, 19/10/2026)

| Medida | Valor |
|---|---|
| boot até a 1ª resposta Python, root ext4 ro (30 boots) | p50 **260 ms**, p95 **266 ms** |
| boot até a 1ª resposta `sh`, root virtiofs (30 boots) | p50 245 ms, p95 264 ms |
| `uname` com ciclo completo, cache quente / frio | 221–252 ms / 869 ms |
| footprint de VM parada (512 MiB configurados) | **44 MB** (RSS 172 MB inclui páginas limpas compartilhadas) |
| RAM configurada de 256 a 2048 MiB | boot igual (260 ms); RSS +40 MB |
| `SIGKILL` no grupo | nenhum processo sobreviveu (todos os boots medidos) |
| rede sem TSI / com TSI | `Network unreachable` / conecta em 1.1.1.1:80 |
| pull + achatamento `python:3.12-alpine` arm64 | 2,7 s, 55 MB |
| warmup Python (`compileall` da stdlib) | +21 MB de `.pyc` (76 MB no total); a imagem oficial não traz `.pyc` |

### Números do caminho de produção (mesmo host, 19/10/2026)

| Medida | Valor |
|---|---|
| `image build --stack python` (bootstrap já feito) | 8,4–10,2 s; warmup ~1 s; ext4 de 128–138 MiB |
| job Python pela CLI, boot frio | ~240 ms no total (execução 31–38 ms) |
| job pela API com pool cheio | 26–43 ms de requisição HTTP completa |
| e2e (6 testes) | verde em duas rodadas seguidas |

### Armadilhas medidas

- **argv com espaço chega quebrado no guest:** a libkrun leva o argv pela linha de comando do kernel. Comando
  com espaço vai por arquivo na imagem (`start.sh`), nunca por argv.
- **`Couldn't find or load libkrunfw.5.dylib`:** a libkrun carrega a libkrunfw por nome. No mac, o `vmm`
  precisa de `DYLD_LIBRARY_PATH` apontando para o diretório da libkrunfw.
- **APFS sem distinção de maiúsculas** (`Case` e `case` viram um arquivo só): extrair rootfs Linux no Mac
  corrompe. A extração acontece dentro da VM de build.
- **O e2fsprogs do Homebrew foi compilado sem libarchive**, então `mkfs.ext4 -d <tar>` falha com
  "Operation not supported". Outro motivo para a VM de build.
- **Guest dinâmico não roda na imagem:** `/opt/runner/guest: not found`. O purego faz o binário que o importa sair
  dinâmico (interpretador glibc) mesmo com `CGO_ENABLED=0`; o guest é outro `main`, e o `build.sh` reprova se ele sair dinâmico.
- **VM sem `PATH`:** a spec sem env sobe o guest com ambiente vazio; o warmup do Python "rodou" em 20 ms sem compilar
  nada. A VM de build recebe `PATH` com `/usr/local/bin`.
- **`umount /build/dev: Resource busy`** depois do warmup em chroot: desmontagem `-l`, já que o tmpfs é descartado.
- **`i/o timeout` em vez de `DeadlineExceeded`:** o deadline do socket pode disparar antes do ctx; o cliente traduz,
  senão o timeout do job virava HTTP 500 (achado por teste intermitente, 1 em 5 rodadas).
- **O binário do `vmm` precisa do entitlement `com.apple.security.hypervisor`.** A assinatura ad-hoc basta.

## Database Schema

Não há banco. O estado é o diretório de cache (`$XDG_CACHE_HOME/runner-codes` ou `~/Library/Caches/runner-codes`),
com `images/`, `build/` e `run/` (sockets). Na memória do serviço ficam só as VMs do pool.

## Backend Architecture

- `internal/app.ExecuteService.Run(ctx, job)`: valida, pega uma VM, envia, aplica o deadline, destrói a VM.
- `internal/app.PoolManager`: um canal de VMs prontas por linguagem com capacidade N. Um goroutine de
  reposição por linguagem faz no máximo 2 boots simultâneos por linguagem (o smolvm limita a 4 no total,
  `pool_controller.rs:19`). Pool vazio → boot síncrono.
- **Sandbox do processo `vmm`:**
  - Linux: `Setpgid`, `Pdeathsig=SIGKILL`, Landlock (disco root e bibliotecas só leitura; console e
    `run/<id>/` da VM com escrita; `/dev/kvm` com ioctl; rede TCP/UDP negada; escopo de sinais; socket Unix de
    outra VM negado a partir da ABI 9) e seccomp denylist com TSYNC (`internal/adapters/krun/sandbox*.go`).
    Medido no Ubuntu 26.04 (ABI 8) com o processo confinado de verdade; microVM completa sob confinamento
    não medida (sem KVM aninhado no M1).
  - macOS: `Setpgid` + pipe de vigia (o `vmm` sai quando o pipe do pai fecha) e assinatura com
    entitlement. O macOS não oferece Landlock nem seccomp, e isso fica dito na doc de segurança.

## Frontend Architecture

Não se aplica.

## Deployment Architecture

- O release gera `runner.code` para `darwin/arm64`, `linux/amd64` e `linux/arm64`, e o guest
  `rc-guest-linux-{amd64,arm64}`, que fica ao lado do host (ou em `RUNNER_GUEST_BIN`) e é injetado nas
  imagens no build. Não vai por `go:embed`: o guest teria de existir antes de compilar o host, e o hash dele já entra
  na chave de cache da imagem.
- No mac, `task sign` assina com o entitlement. No Linux, o usuário precisa de acesso a `/dev/kvm`.
- Não há infraestrutura de nuvem: as 79 tasks `aws:*` e a EC2 metal saem.

## Development Workflow

```bash
task krun:install         # nix profile add .#krun (libkrun + libkrunfw), mac e Linux
task build                # host + guest, assina no mac
task image:build -- python
task test                 # unidade
task test:cov             # gate 90%
task test:e2e             # libkrun real (tag e2e)
task runbook              # docs/runbook.md executado: todo recurso novo ganha seção lá e passo no scripts/runbook.sh
task serve                # API em 127.0.0.1:8080
```

Variáveis:
- `RUNNER_CACHE_DIR`: cache;
- `RUNNER_KRUN_LIB`: caminho da libkrun (padrão `~/.nix-profile/lib/libkrun.dylib` no mac; no Linux o do perfil do Nix ou o soname `libkrun.so.1`);
- `RUNNER_POOL_SIZE`: VMs por linguagem, padrão 2.
