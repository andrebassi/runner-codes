---
title: 'Arquitetura'
description: 'As peças do Runner Codes e o caminho de um job do HTTP até a microVM'
sidebar_position: 3
---

## Visão geral

```
 cliente HTTP / CLI
        │
        ▼
┌───────────────────────────────┐
│ rc serve | run    │  host (macOS ou Linux)
│  ├─ domínio: valida o job     │
│  ├─ pool: VMs prontas por     │
│  │   linguagem                │
│  └─ launcher: spawn do vmm    │
└──────────────┬────────────────┘
               │ spawn (nunca fork)
               ▼
┌───────────────────────────────┐
│ rc vmm --spec     │  um processo por VM
│  libkrun (purego) +           │
│  libkrunfw (kernel Linux)     │
│  HVF no macOS / KVM no Linux  │
└──────────────┬────────────────┘
               │ socket Unix  ⇄  vsock porta 5000
               ▼
┌───────────────────────────────┐
│ microVM                       │
│  root: ext4 read-only (vda)   │
│  /tmp: tmpfs 256 MiB          │
│  /opt/runner/start.sh         │
│  /opt/runner/guest (estático) │
│  sem rede                     │
└───────────────────────────────┘
```

## Binários

| Binário | Onde roda | O que faz |
|---------|-----------|-----------|
| `runner.code` | host | `serve` (API + pool), `run` (job pela CLI), `image build`, `languages`, `doctor` e o subcomando interno `vmm` |
| `runner-guest` | dentro da VM | escuta a porta vsock 5000, escreve os arquivos, compila se for o caso, executa e devolve o resultado. Guardado como `bin/rc-guest-linux-<arch>` |

São dois binários porque o purego força link dinâmico contra a glibc no Linux, e as imagens das linguagens são Alpine (musl). Um guest dinâmico falha dentro da VM com `not found`; o guest é compilado estático.

## Camadas do host

O código do host segue arquitetura hexagonal:

| Camada | Pacote | Responsabilidade |
|--------|--------|------------------|
| domínio | `internal/domain` | job, resultado, limites (stdin, arquivos, tamanho, timeout) e erros E005/E006 |
| aplicação | `internal/app` | executor e pool de VMs |
| adapters | `internal/adapters/krun` | carga da libkrun, spec da VM, launcher e processo `vmm` |
| | `internal/adapters/oci` | pull da imagem OCI e build do ext4 |
| | `internal/adapters/catalog` | `languages.yaml` embutido |
| | `internal/adapters/guestproto` | cliente do protocolo vsock |
| | `internal/adapters/httpapi` | API HTTP |
| guest | `internal/guest` | servidor vsock e executor por linguagem |

## Caminho de um job pela API

1. `POST /api/v1/run` chega ao `httpapi`, que monta o job.
2. O domínio valida: linguagem no catálogo (senão E006), stdin ≤ 1.000.000 bytes (senão E005), até 32 arquivos, código total ≤ 4 MiB, nomes de arquivo válidos, timeout entre 0 e 300 s. Recusa vira HTTP 400.
3. O pool entrega uma VM já bootada daquela linguagem. Se o pool estiver vazio, sobe uma VM na hora (boot frio) — nunca recusa.
4. O host conecta no socket Unix ligado à porta vsock 5000 da VM e manda o job como frame (4 bytes de tamanho + JSON).
5. O guest grava os arquivos num diretório em `/tmp`, compila se a linguagem exigir, executa com o stdin do job e o timeout, e responde com outro frame.
6. A VM é destruída (SIGKILL no grupo do processo) e o pool repõe uma VM nova em background.
7. A API devolve `status`, `stdout`, `stderr`, `exception`, `executionTime`.

## Ciclo de vida de uma VM

- O launcher cria o processo `rc vmm --spec <arquivo>` por **spawn**. Fork de processo multi-thread é morto pelo Hypervisor.framework cerca de 2 s depois, por isso nunca se usa fork.
- O processo `vmm` carrega a libkrun, configura a VM a partir da spec e chama `krun_start_enter`, que assume o processo até a VM terminar.
- Timeout de job: SIGKILL no grupo do processo.
- Se o pai morre, a VM morre: `Pdeathsig` no Linux; no macOS, um pipe de vigia no fd 3.

Detalhes em [libkrun](/concepts/libkrun).

## Imagens

Cada linguagem tem um ext4 read-only gerado a partir da imagem OCI oficial, com o guest, o `start.sh` e o warmup gravados. O build roda numa VM Alpine própria. Veja [Imagens](/concepts/images).

## Configuração por ambiente

| Variável | Padrão | Uso |
|----------|--------|-----|
| `RUNNER_LISTEN` | `127.0.0.1:8080` | endereço da API (`serve`) |
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS), `~/.cache/runner-codes` (Linux) | imagens, VM de build e sockets |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib` (macOS); no Linux `~/.nix-profile/lib/libkrun.so.1` ou o soname `libkrun.so.1` | caminho da libkrun |
| `RUNNER_GUEST_BIN` | `rc-guest-linux-<arch>` ao lado do host | guest injetado nas imagens |
