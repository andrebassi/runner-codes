---
title: 'Arquitetura'
description: 'As peças do Runner Codes e o caminho de um job e de uma máquina, do HTTP até a microVM'
sidebar_position: 3
---

## Visão geral

```
 cliente HTTP / CLI / SDK
        │
        ▼
┌───────────────────────────────┐
│ rc  (host: macOS ou Linux)    │
│  ├─ domínio: valida job/spec  │
│  ├─ pool: VMs prontas por     │
│  │   linguagem (só jobs)      │
│  ├─ máquinas: registro, disco │
│  └─ launcher: spawn do vmm    │
└──────────────┬────────────────┘
               │ spawn (nunca fork)
               ▼
┌───────────────────────────────┐
│ rc vmm --spec   (1 por VM)    │
│  libkrun (purego) +           │
│  libkrunfw (kernel Linux)     │
│  HVF no macOS / KVM no Linux  │
│  gateway de rede (máquinas    │
│   com --net), visor de tela   │
└──────────────┬────────────────┘
               │ socket Unix  ⇄  vsock porta 5000
               ▼
┌───────────────────────────────┐
│ microVM                       │
│  job: root ext4 read-only,    │
│       /tmp tmpfs, sem rede    │
│  máquina: overlay da imagem + │
│       disco próprio (/dev/vdb)│
│  /opt/runner/guest (estático) │
└───────────────────────────────┘
```

## Binários

| Binário | Onde roda | O que faz |
|---------|-----------|-----------|
| `rc` | host | `run`, `shell`, `machine ...`, `pack ...`, `serve` (API + pool), `image build/prune`, `languages`, `doctor` e o subcomando interno `vmm` |
| `runner-guest` | dentro da VM | escuta a porta vsock 5000 e atende cada operação: rodar código, exec, cópia de arquivo, PTY, workload, porta publicada, segredos, agente SSH, montagem S3. Guardado como `bin/rc-guest-linux-<arch>` |

São dois binários porque o purego força link dinâmico contra a glibc no Linux, e as imagens das linguagens são Alpine
(musl). Um guest dinâmico falha dentro da VM com `not found`; o guest é compilado estático, e o `scripts/build.sh`
reprova se ele sair dinâmico.

## Camadas do host

O código do host segue arquitetura hexagonal; `cmd/rc` só liga as peças.

| Camada | Pacote | Responsabilidade |
|--------|--------|------------------|
| domínio | `internal/domain` | job, resultado, limites (stdin, arquivos, tamanho, timeout) e erros E00x |
| aplicação | `internal/app` | executor e pool de jobs; ciclo de vida das máquinas, branch, volumes staged, credenciais, segredos |
| ports | `internal/ports` | interfaces (`VMSpec` e a validação dela, launcher, catálogo, imagens) |
| adapters | `internal/adapters/krun` | carga da libkrun, configuração da VM, launcher, processo `vmm` e confinamento no Linux |
| | `internal/adapters/oci` | pull da imagem OCI e build do ext4 na VM de build |
| | `internal/adapters/catalog` | `languages.yaml` embutido |
| | `internal/adapters/guestproto` | cliente do protocolo vsock |
| | `internal/adapters/httpapi` | API HTTP (jobs e máquinas) |
| | `internal/adapters/machinestore` | registro e disco de cada máquina (clone do disco base) |
| | `internal/adapters/runfile` | leitura do arquivo `runner.code` (TOML) |
| | `internal/adapters/netgw` | gateway de rede em userspace: política de saída, DNS, portas, ICMP, rede nomeada, interceptação de credencial |
| | `internal/adapters/screen`, `viewer` | tela da VM no navegador |
| | `internal/adapters/hostfs` | lado do host dos volumes (modo dos arquivos do `:staged`) |
| outros | `internal/credca`, `internal/objectstore`, `internal/s3fs`, `internal/pack` | CA por máquina, cliente S3, núcleo do volume `s3://`, formato do pack |
| guest | `internal/guest` | servidor vsock e executor por linguagem |

SDKs: `pkg/client` (Go) e `sdk/python` (Python), só biblioteca padrão.

## Caminho de um job pela API

1. `POST /api/v1/run` chega ao `httpapi`, que monta o job.
2. O domínio valida: linguagem no catálogo (senão E006), stdin ≤ 1.000.000 bytes (senão E005), até 32 arquivos,
   código total ≤ 4 MiB, nomes de arquivo válidos, timeout entre 0 e 300 s. Recusa vira HTTP 400.
3. O pool entrega uma VM já bootada daquela linguagem. Se o pool estiver vazio, sobe uma VM na hora (boot frio):
   nunca recusa.
4. O host conecta no socket Unix ligado à porta vsock 5000 da VM e manda o job como frame (4 bytes de tamanho + JSON).
5. O guest grava os arquivos num diretório em `/tmp`, compila se a linguagem exigir, executa com o stdin do job e o
   timeout, e responde com outro frame.
6. A VM é destruída (SIGKILL no grupo do processo) e o pool repõe uma VM nova em background.
7. A API devolve `status`, `stdout`, `stderr`, `exception`, `executionTime`.

## Caminho de uma máquina

1. `rc machine create` grava o registro em `<cache>/machines/<nome>/machine.json` e um disco ext4 clonado do disco
   base (clonefile no APFS, FICLONE no btrfs/XFS, cópia esparsa no resto).
2. `rc machine start` sobe o `rc vmm` **destacado** (sessão própria, sem vínculo com o pai): a máquina continua ligada
   depois que o comando sai. O pid fica em `vmm.pid`.
3. O primeiro processo da VM é o `/opt/runner/machine.sh`: monta o disco em `/persist`, um overlayfs com a imagem por
   baixo e o `upper/` por cima, liga `/workspace` e roda o guest num `chroot`.
4. `exec`, `shell`, `cp`, `logs` falam com o guest pelo mesmo socket da porta 5000. Com `--net`, a VM ganha `eth0` e o
   tráfego passa pelo gateway do próprio processo `vmm`, que aplica a política de saída.
5. `rc machine stop` pede `shutdown` ao guest, que faz `sync` e sai; se não terminar no `--timeout`, SIGKILL.

Detalhes em [machine](/cli/machine) e [Rede e volumes](/cli/network).

## Ciclo de vida de uma VM

- O launcher cria o processo `rc vmm --spec <arquivo>` por **spawn**. Fork de processo multi-thread é morto pelo
  Hypervisor.framework, por isso nunca se usa fork.
- O processo `vmm` carrega a libkrun, configura a VM a partir da spec e chama `krun_start_enter`, que assume o
  processo até a VM terminar.
- Timeout de job: SIGKILL no grupo do processo.
- VM de job morre com o pai: `Pdeathsig` no Linux; no macOS, um pipe de vigia no fd 3. A VM de máquina é destacada.
- Se o `vmm` morre no boot, o start termina na hora citando a última linha do `vmm.log` escrita neste start.

Detalhes em [libkrun](/concepts/libkrun).

## Imagens

Cada stack tem um ext4 read-only gerado a partir da imagem OCI oficial, com o guest, o `start.sh` e o warmup
gravados. O build roda numa VM Alpine própria. Veja [Imagens](/concepts/images).

## Configuração por ambiente

| Variável | Padrão | Uso |
|----------|--------|-----|
| `RUNNER_LISTEN` | `127.0.0.1:8080` | endereço da API (`serve`) |
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS), `~/.cache/runner-codes` (Linux) | imagens, VM de build, máquinas, packs e sockets |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib` (macOS); no Linux `~/.nix-profile/lib/libkrun.so.1` ou o soname `libkrun.so.1` | caminho da libkrun |
| `RUNNER_GUEST_BIN` | `rc-guest-linux-<arch>` ao lado do host | guest injetado nas imagens |
| `RUNNER_SANDBOX` | ligado | `off` desliga Landlock + seccomp no Linux, só para diagnóstico |
