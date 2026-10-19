---
title: 'Visão geral da CLI'
description: 'Os subcomandos do runner.code'
---

## O binário

`runner.code` é o binário do host. Ele roda no macOS Apple Silicon e no Linux x86_64/aarch64 com KVM.

```bash
runner.code --help
```

| Subcomando | O que faz | Página |
|------------|-----------|--------|
| `serve` | API HTTP com pool de VMs pré-bootadas | [API HTTP](/cli/http-api) |
| `run` | executa um código numa VM nova (boot frio) | [run](/cli/run) |
| `image build` | gera o ext4 de uma ou mais linguagens a partir da imagem OCI oficial | [image](/cli/image) |
| `languages` | lista o catálogo e se a imagem de cada linguagem já foi construída | abaixo |
| `doctor` | confere se o host consegue rodar microVMs | abaixo |
| `vmm` | processo de uma VM; uso interno, chamado pelo launcher | [libkrun](/concepts/libkrun) |

O guest (`runner-guest`) é outro binário e roda só dentro da VM; não é chamado à mão.

## languages

```bash
runner.code languages
```

Uma linha por linguagem do catálogo, com `ready=true` quando o ext4 já existe no cache, e a imagem OCI de origem:

```
python   ready=true  python:3.12-alpine
nodejs   ready=false node:22-alpine
...
```

## doctor

```bash
runner.code doctor
```

Confere, uma linha por item:

| Item | O que verifica |
|------|----------------|
| `plataforma` | macOS em Apple Silicon, ou Linux com `/dev/kvm` aberto para leitura e escrita |
| `libkrun` | se a biblioteca carrega pelo caminho configurado |
| `guest` | se o binário do guest existe ao lado do host |
| `imagem <lang>` | lista as imagens já construídas |

Cada falha sai como `FALHA <item> <motivo>`, e o comando termina com erro quando há alguma.

## Variáveis de ambiente

| Variável | Padrão | Uso |
|----------|--------|-----|
| `RUNNER_LISTEN` | `127.0.0.1:8080` | endereço do `serve` |
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS), `~/.cache/runner-codes` (Linux) | imagens, VM de build, sockets |
| `RUNNER_KRUN_LIB` | `/opt/homebrew/lib/libkrun.dylib` (macOS), `libkrun.so.1` (Linux) | caminho da libkrun |
| `RUNNER_GUEST_BIN` | `runner.code-guest-linux-<arch>` ao lado do host | guest injetado nas imagens |

## Pelo Taskfile

| Task | Equivale a |
|------|------------|
| `task run -- <flags>` | `./bin/runner.code run <flags>` |
| `task serve -- <flags>` | `./bin/runner.code serve <flags>` |
| `task image:build -- <flags>` | `./bin/runner.code image build <flags>` |
| `task doctor` | `./bin/runner.code doctor` |
