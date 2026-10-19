---
title: 'Início rápido'
description: 'Instalar a libkrun, compilar, construir a imagem Python e rodar o primeiro job'
sidebar_position: 2
---

## Pré-requisitos

- macOS em Apple Silicon, **ou** Linux x86_64/aarch64 com acesso de leitura e escrita a `/dev/kvm`.
- Go e [Task](https://taskfile.dev) para compilar.
- Acesso à internet para baixar as imagens OCI das linguagens (só no build da imagem; a VM do job não tem rede).

Windows não é suportado.

## 1. Instalar a libkrun e a libkrunfw

### macOS

```bash
brew tap slp/krun
brew trust slp/krun
brew install slp/krun/libkrunfw slp/krun/libkrun
```

O `brew trust` é necessário: sem ele o brew recusa instalar a partir de um tap não confiado.

### Linux

Pelo nixpkgs (libkrun 1.19.5, libkrunfw 5.5.0 no momento da escrita):

```bash
nix profile install nixpkgs#libkrun nixpkgs#libkrunfw
```

A task `task krun:install` faz o passo certo para o sistema em uso.

## 2. Compilar

```bash
task build
```

Gera em `bin/`:

- `runner.code` — o host (API, CLI, build de imagem e o processo de cada VM);
- `runner.code-guest-linux-<arch>` — o guest estático que roda dentro da VM.

No macOS, o build assina o `runner.code` com o entitlement `com.apple.security.hypervisor`. Sem ele, `krun_start_enter` falha.

## 3. Conferir o host

```bash
task doctor
```

O `doctor` confere a plataforma (Apple Silicon ou `/dev/kvm` acessível), se a libkrun carrega, se o binário do guest existe e quais imagens já foram construídas. Cada falha vem com o motivo.

## 4. Construir a imagem da linguagem

```bash
task image:build -- --lang python
```

Baixa `python:3.12-alpine`, gera o ext4 read-only com o guest e o warmup gravados, e guarda no cache. Na máquina medida, a imagem Python levou ~8–10 s. Um segundo build com as mesmas entradas sai do cache. Para todas as linguagens do catálogo: `task image:build -- --all`.

## 5. Rodar um job pela CLI

```bash
task run -- --lang python --code 'print(1)'
```

Cada `run` sobe uma VM nova (boot frio) e a destrói no fim. O comando sai com rc 1 se o programa falhar ou estourar o tempo. Veja [`run`](/cli/run).

## 6. Subir a API

```bash
task serve
```

Escuta em `127.0.0.1:8080` e mantém um [pool](/concepts/pool) de 2 VMs prontas por linguagem com imagem construída. Em outro terminal:

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{"language":"python","files":[{"name":"main.py","content":"print(input()[::-1])"}],"stdin":"abc"}'
```

```json
{"status":"success","stdout":"cba\n","stderr":null,"exception":null,"executionTime":35}
```

O valor de `executionTime` varia a cada execução. A API não tem autenticação: mantenha-a em loopback. Veja [API HTTP](/cli/http-api).
