---
title: 'Introdução'
description: 'Execução de código não confiável em microVMs libkrun: jobs descartáveis e máquinas persistentes'
sidebar_position: 1
---

## O que é o Runner Codes

O Runner Codes executa código não confiável (tipicamente gerado por LLM) dentro de microVMs. O binário é o `rc`, e
ele trabalha de dois jeitos:

- **Job descartável:** uma VM nova por job, sem rede, com root read-only, destruída no fim. É o que fazem
  `rc run`, `rc shell` e as rotas `/api/v1/run` e `/api/v1/execute`.
- **Máquina persistente:** uma VM com disco próprio, que guarda o que foi instalado e gravado entre um start e outro.
  É o `rc machine ...`, declarável num arquivo `runner.code` e controlável pela API.

**Código-fonte:** https://github.com/andrebassi/runner-codes

## As peças

- **VMM:** [libkrun](/concepts/libkrun) 1.19.6 com o kernel da libkrunfw 5.6.2 (Linux 6.12.109), carregada direto
  como biblioteca por purego (sem cgo). Cada VM é um processo `rc vmm`.
- **Hypervisor:** Hypervisor.framework no macOS Apple Silicon; KVM (`/dev/kvm`) no Linux x86_64 e aarch64.
- **Imagens:** cada stack vira um ext4 read-only gerado a partir da [imagem OCI oficial](/concepts/images), sem
  Docker. Máquinas aceitam também uma imagem OCI qualquer (`--image`).
- **Canal com o guest:** 4 bytes big-endian de tamanho + JSON, na porta vsock 5000
  ([protocolo](/api-reference/vsock-protocol)).
- **Latência:** o [pool](/concepts/pool) do `rc serve` mantém VMs já bootadas por linguagem, cada uma usada uma vez.

## O que o `rc` faz

| Recurso | Onde está |
|---------|-----------|
| job efêmero pela CLI, com saída em streaming; shell interativo numa VM nova | [run e shell](/cli/run) |
| máquinas: `create`, `start`, `exec`, `shell`, `cp`, `logs`, `stop`, `delete`, `branch`, `pause`, `resume`, `sync`, `update`, `data-dir`, `prune` | [machine](/cli/machine) |
| máquina declarada num arquivo | [runner.code](/cli/runner-code) |
| rede por política (allow-host, CIDR, `--block-private`), redes nomeadas, ICMP, portas e faixas de portas | [Rede e volumes](/cli/network) |
| volumes: diretório do host, `:ro`, `:staged`, disco da máquina, S3 | [Rede e volumes](/cli/network#volumes) |
| credenciais que a máquina usa sem ler, segredos, agente SSH, pontes de socket | [Credenciais](/cli/credentials), [machine](/cli/machine) |
| GPU por Vulkan (Venus) e a stack `llama` | [GPU](/advanced/gpu) |
| tela da VM no navegador, com teclado e mouse (`--display`) | [Tela no navegador](/cli/display) |
| imagem ou máquina num artefato portátil | [pack](/cli/pack) |
| API HTTP com pool, SSE e rotas de máquina | [API HTTP](/cli/http-api), [Visão geral da API](/api-reference/overview) |
| SDKs Go (`pkg/client`) e Python (`sdk/python`) | [SDKs](/api-reference/sdks) |
| conferência do host | `rc doctor`, em [Visão geral da CLI](/cli/overview#doctor) |

## Plataformas

| Plataforma | Estado |
|------------|--------|
| macOS, Apple Silicon | suportado, testado de ponta a ponta e medido |
| Linux x86_64 / aarch64 com `/dev/kvm` | suportado; o confinamento Landlock + seccomp foi testado, mas uma microVM completa no Linux **ainda não foi medida** |
| Windows | **não suportado**: a libkrun upstream não tem backend WHP, e o build para Windows falha de propósito |

## Números medidos

macOS 27, Apple Silicon. No Linux ainda não houve medição.

| O quê | Valor |
|-------|-------|
| boot até a 1ª resposta Python (30 boots, root ext4 ro) | p50 260 ms, p95 266 ms |
| job Python pela CLI, boot frio | ~240 ms no total |
| job pela API com o pool cheio | 26–43 ms de requisição HTTP completa (uma stack) |
| `machine start` até o guest responder | 204–244 ms |
| footprint de uma VM parada | 44 MB |
| `go run` com o cache de build aquecido na imagem | 247 ms (sem o cache, 3,4 s) |

Detalhes e método em [Desempenho](/advanced/performance) e no [Relatório de testes](/advanced/test-report).

## Linguagens

O catálogo tem 15 linguagens: Python, Node.js, Go, Rust, Java, C, C++, Ruby, PHP, Perl, Bash, sh, AWK, jq e SQLite,
mais a stack `llama` (llama.cpp com Vulkan). Veja [Linguagens](/languages/overview).

## Próximos passos

- [Início rápido](/quickstart): instalar a libkrun, compilar, construir a imagem, rodar o primeiro job e a primeira máquina.
- [Referência para agentes](/agents): cada comando, flag, rota e erro numa página.
- [Arquitetura](/architecture): as peças e o caminho de um job e de uma máquina.
- [Exemplos e guias](/guides/examples): exemplos de `runner.code` e guias que provam cada fluxo numa VM real.
- [Segurança](/advanced/security): o que isola o código e o que ainda falta.
