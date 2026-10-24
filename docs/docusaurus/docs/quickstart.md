---
title: 'Início rápido'
description: 'Instalar a libkrun, compilar, construir a imagem Python, rodar o primeiro job e a primeira máquina'
sidebar_position: 2
---

## Pré-requisitos

- macOS em Apple Silicon, **ou** Linux x86_64/aarch64 com acesso de leitura e escrita a `/dev/kvm`.
- [Nix](https://nixos.org) para instalar a libkrun; Go e [Task](https://taskfile.dev) para compilar.
- Acesso à internet para baixar as imagens OCI das linguagens (só no build da imagem; a VM do job não tem rede).

Windows não é suportado.

## 1. Instalar a libkrun e a libkrunfw

Pelo Nix, nos dois sistemas (sem Homebrew nem Docker):

```bash
task krun:install    # nix profile add .#krun
```

- **macOS:** libkrun 1.19.6 e libkrunfw 5.6.2, de derivações próprias do repo (`nix/`). A 1ª vez compila a
  libkrun (~10 min).
- **Linux:** libkrun e libkrunfw do nixpkgs, com virtio-blk ligado.

As duas bibliotecas vão para `~/.nix-profile/lib`, onde o `rc` procura por padrão.

## 2. Compilar

```bash
task build
```

Gera em `bin/`:

- `rc`: o host (CLI, API, build de imagem, máquinas e o processo de cada VM);
- `rc-guest-linux-<arch>`: o guest estático que roda dentro da VM.

No macOS, o build assina o `rc` com o entitlement `com.apple.security.hypervisor`. Sem ele, `krun_start_enter` falha.

## 3. Conferir o host

```bash
./bin/rc doctor
```

Uma linha por item: `ok` em `platform`, `libkrun` e `guest`, linhas `info` para o que este host não oferece (CUDA,
virtualização aninhada, confinamento no macOS) e uma linha `ok image <stack>` por imagem construída. Falha sai como
`FAIL <item> <motivo>`.

## 4. Construir a imagem da linguagem

```bash
task image:build -- --stack python
```

Baixa `python:3.12-alpine`, gera o ext4 read-only com o guest e o warmup gravados, e guarda no cache. Na máquina
medida, a imagem Python levou ~8–10 s. Um segundo build com as mesmas entradas sai do cache. Para todas as
linguagens do catálogo: `task image:build -- --all`.

## 5. Rodar um job

```bash
./bin/rc run --stack python --code 'print(1)'
./bin/rc shell --stack python      # shell interativo numa VM nova; some no exit
```

Cada `run` sobe uma VM nova (boot frio), mostra a saída enquanto o programa roda e destrói a VM no fim. O comando
sai com rc 1 se o programa falhar ou estourar o tempo. Veja [run e shell](/cli/run).

## 6. Ligar uma máquina persistente

```bash
./bin/rc machine create --name demo --stack python
./bin/rc machine start --name demo
./bin/rc machine exec --name demo -- sh -c 'echo persistiu > /etc/minha-config'
./bin/rc machine stop --name demo
./bin/rc machine start --name demo
./bin/rc machine exec --name demo -- cat /etc/minha-config     # persistiu
./bin/rc machine delete --name demo --force   # sem --force, máquina ligada é recusada (INVALID_STATE)
```

O que é gravado em `/` e em `/workspace` sobrevive ao stop; `/tmp` volta vazio. Sem `--net`, a máquina não tem
rede. Veja [machine](/cli/machine), [runner.code](/cli/runner-code) e [Rede e volumes](/cli/network).

## 7. Subir a API

```bash
task serve
```

Escuta em `127.0.0.1:8080` e mantém um [pool](/concepts/pool) de 2 VMs prontas por linguagem com imagem
construída. Em outro terminal:

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{"language":"python","files":[{"name":"main.py","content":"print(input()[::-1])"}],"stdin":"abc"}'
```

```json
{"status":"success","stdout":"cba\n","stderr":null,"exception":null,"executionTime":35}
```

O valor de `executionTime` varia a cada execução. A API não tem autenticação: mantenha-a em loopback. Veja
[API HTTP](/cli/http-api) e os [SDKs](/api-reference/sdks).

## 8. Conferir tudo de uma vez

```bash
task runbook
```

Executa o `docs/runbook.md` de ponta a ponta e reprova se algum passo sair diferente do esperado. Na última rodada
(24/10/2026, macOS 27), foram 146 conferências e 0 falhas.
