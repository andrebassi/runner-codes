---
title: 'Visão geral da CLI'
description: 'Os subcomandos do rc'
---

## O binário

`rc` é o binário do host. Ele roda no macOS Apple Silicon e no Linux x86_64/aarch64 com KVM.

```bash
rc --help
```

| Subcomando | O que faz | Página |
|------------|-----------|--------|
| `run` | executa um código numa VM nova (boot frio), com a saída em streaming | [run e shell](/cli/run) |
| `shell` | shell interativo numa VM nova de uma stack | [run e shell](/cli/run#shell) |
| `machine ...` | máquinas persistentes: `create`, `start`, `exec`, `shell`, `cp`, `logs`, `stop`, `delete`, `run`, `branch`, `pause`, `resume`, `sync`, `update`, `status`, `ls`, `data-dir`, `prune` | [machine](/cli/machine), [runner.code](/cli/runner-code), [Rede e volumes](/cli/network) |
| `pack ...` | `create`, `inspect`, `prune`: imagem ou máquina num artefato portátil | [pack](/cli/pack) |
| `serve` | API HTTP com pool de VMs pré-bootadas e rotas de máquina | [API HTTP](/cli/http-api) |
| `image build`, `image prune` | gera o ext4 de uma ou mais stacks a partir da imagem OCI oficial; limpa o cache | [image](/cli/image) |
| `languages` | lista o catálogo e se a imagem de cada stack já foi construída | abaixo |
| `doctor` | confere se o host consegue rodar microVMs e o que ele oferece | abaixo |
| `vmm` | processo de uma VM; uso interno, chamado pelo launcher | [libkrun](/concepts/libkrun) |

O guest (`runner-guest`) é outro binário e roda só dentro da VM; não é chamado à mão.

## languages

```bash
rc languages
```

Uma linha por stack do catálogo, com `ready=true` quando o ext4 já existe no cache, e a imagem OCI de origem:

```
awk      ready=true  alpine:3.22
...
llama    ready=true  fedora:43
nodejs   ready=false node:22-alpine
python   ready=true  python:3.12-alpine
```

## doctor

```bash
rc doctor
```

Uma linha por item, com `ok`, `info`, `warn` ou `FAIL`. Saída real num Mac M1 Pro:

```
ok    platform
ok    libkrun
ok    gpu: libkrun built with virtio-gpu (Venus); use --gpu on a stack with gpu
info  cuda: not supported (no CUDA remoting; --gpu is Vulkan)
info  nested: not supported on this host; --nested is refused
ok    guest
info  sandbox: macOS has no Landlock/seccomp; the VM process runs unconfined
ok    image awk
...
```

| Item | O que verifica |
|------|----------------|
| `platform` | macOS em Apple Silicon, ou Linux com `/dev/kvm` aberto para leitura e escrita |
| `libkrun` | se a biblioteca carrega pelo caminho configurado |
| `gpu`, `cuda` | se a libkrun tem virtio-gpu (Venus); CUDA não é suportado |
| `nested` | se o host aninha virtualização (`--nested`) |
| `guest` | se o binário do guest existe ao lado do host |
| `sandbox` | no Linux, a ABI do Landlock e o seccomp; avisa quando a ABI é < 9 |
| `image <stack>` | uma linha por imagem já construída |

Falha sai como `FAIL <item> <motivo>`, e o comando termina com erro quando há alguma.

## Variáveis de ambiente

| Variável | Padrão | Uso |
|----------|--------|-----|
| `RUNNER_LISTEN` | `127.0.0.1:8080` | endereço do `serve` |
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS), `~/.cache/runner-codes` (Linux) | imagens, VM de build, máquinas, packs, sockets |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib` (macOS); no Linux `~/.nix-profile/lib/libkrun.so.1` ou o soname `libkrun.so.1` | caminho da libkrun |
| `RUNNER_GUEST_BIN` | `rc-guest-linux-<arch>` ao lado do host | guest injetado nas imagens |
| `RUNNER_SANDBOX` | ligado | `off` desliga Landlock + seccomp no Linux, só para diagnóstico |

## Pelo Taskfile

| Task | Equivale a |
|------|------------|
| `task run -- <flags>` | `./bin/rc run <flags>` |
| `task shell -- <flags>` | `./bin/rc shell <flags>` |
| `task serve -- <flags>` | `./bin/rc serve <flags>` |
| `task image:build -- <flags>` | `./bin/rc image build <flags>` |
| `task doctor` | `./bin/rc doctor` |
