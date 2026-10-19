---
title: 'Introdução'
description: 'Execução de código não confiável em microVMs libkrun, uma VM nova por job'
sidebar_position: 1
---

## O que é o Runner Codes

O Runner Codes executa código não confiável — tipicamente gerado por LLM — dentro de microVMs, uma VM nova para cada job. O host recebe o job (pela CLI ou por uma API HTTP), sobe uma microVM com a imagem da linguagem, entrega o código ao guest pelo vsock e devolve stdout, stderr, código de saída e tempo de execução.

**Código-fonte:** https://github.com/andrebassi/runner-codes

## Como funciona, em uma frase por peça

- **VMM:** [libkrun](/concepts/libkrun) 1.19.6 com o kernel da libkrunfw 5.6.2 (Linux 6.12.109), chamada direto pela biblioteca, carregada por purego (sem cgo).
- **Hypervisor:** Hypervisor.framework no macOS Apple Silicon; KVM (`/dev/kvm`) no Linux x86_64 e aarch64.
- **Root da VM:** um disco ext4 read-only gerado a partir da [imagem OCI oficial](/concepts/images) da linguagem; escrita só num tmpfs em `/tmp`, que some com a VM.
- **Rede:** desligada. A VM não tem interface de rede nem TSI.
- **Protocolo:** 4 bytes big-endian de tamanho + JSON, na porta vsock 5000 — o mesmo [contrato](/concepts/vsock-communication) de antes da migração.
- **Latência:** um [pool](/concepts/pool) mantém VMs já bootadas por linguagem; cada VM atende um único job e é destruída.

## Plataformas

| Plataforma | Estado |
|------------|--------|
| macOS, Apple Silicon | suportado e medido |
| Linux x86_64 / aarch64 com `/dev/kvm` | suportado; desempenho **ainda não medido** |
| Windows | **não suportado** — a libkrun upstream não tem backend WHP, e o build para Windows falha de propósito |

## Histórico: o que mudou

Esta documentação substitui a versão baseada em Firecracker. Antes, o projeto rodava Firecracker numa instância bare metal da AWS, com rootfs ext4 montado por debootstrap como root, snapshots guardados em object storage e um guest com handshake `CONNECT 5000`; os endpoints `/execute` e `/run` eram stub.

Hoje a libkrun roda direto no Mac ou num Linux com KVM, as imagens são construídas localmente a partir das imagens OCI oficiais, e `/api/v1/run` e `/api/v1/execute` executam de verdade. O formato das mensagens host↔guest e o contrato da API foram mantidos.

## Números medidos

Medidos no macOS 27.0.1, Apple Silicon, em 19/10/2026. No Linux ainda não houve medição.

| O quê | Valor |
|-------|-------|
| boot até a 1ª resposta Python (30 boots, root ext4 ro) | p50 260 ms, p95 266 ms |
| job Python pela CLI, boot frio | ~240 ms no total (execução 31–38 ms) |
| job pela API com o pool cheio | 26–43 ms de requisição HTTP completa |
| footprint de uma VM parada | 44 MB |

Detalhes e método em [Desempenho](/advanced/performance).

## Linguagens

O catálogo tem 15 linguagens: Python, Node.js, Go, Rust, Java, C, C++, Ruby, PHP, Perl, Bash, sh, AWK, jq e SQLite. Veja [Linguagens](/languages/overview).

## Próximos passos

- [Início rápido](/quickstart): instalar a libkrun, compilar, construir a imagem e rodar o primeiro job.
- [Arquitetura](/architecture): as peças e o caminho de um job.
- [Segurança](/advanced/security): o que isola o código e o que ainda falta.
