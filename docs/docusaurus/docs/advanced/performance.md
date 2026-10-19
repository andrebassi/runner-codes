---
title: 'Desempenho'
description: 'Números medidos de boot, execução, memória e build de imagem'
---

## Onde foi medido

Todos os números desta página foram medidos no **macOS 27.0.1, Apple Silicon, em 19/10/2026**. **O Linux ainda não foi medido**; não assuma que os valores se repetem com KVM.

## Boot

| Medida | Valor |
|--------|-------|
| boot até a 1ª resposta do guest Python, root ext4 read-only, 30 boots | p50 260 ms, p95 266 ms |
| efeito da RAM configurada (256 a 2048 MiB) no boot | nenhum |

"Resposta" é a primeira resposta do guest ao job `sh true` que a espera de prontidão manda a cada 1 ms. É o momento em que a VM pode receber um job real.

## Execução de um job

| Caminho | Valor |
|---------|-------|
| CLI, job Python, boot frio | ~240 ms no total, dos quais 31–38 ms de execução |
| API, pool cheio | 26–43 ms de requisição HTTP completa |

A diferença entre os dois é o boot, que o [pool](/concepts/pool) tira do caminho da requisição. Com o pool vazio, a API cai no mesmo boot frio da CLI.

## Memória

| Medida | Valor |
|--------|-------|
| footprint de uma VM parada | 44 MB |
| RSS da mesma VM | 172 MB, incluindo páginas limpas compartilhadas |

A memória da libkrun é elástica: o host paga só as páginas que o guest toca. O teto de RAM por linguagem no catálogo (de 256 a 1024 MiB) limita o abuso, mas não é reservado.

## Build de imagem

| Medida | Valor |
|--------|-------|
| build da imagem Python | ~8–10 s |
| pull + achatamento das camadas | 2,7 s |
| warmup do Python (`compileall`) | ~1 s; a imagem passou de 66 para 85 MiB |

O build é custo único por chave de cache; um segundo build com as mesmas entradas sai do cache.

## O que o desempenho deve ao desenho

- **Root em disco ext4 read-only**, não virtiofs: virtiofs não foi mais rápido e não confina o guest aos diretórios compartilhados.
- **Warmup gravado na imagem:** o `.pyc` da stdlib do Python e o `GOCACHE` do Go já vêm prontos; para Go, a cópia para o tmpfs acontece no boot, antes de a VM entrar no pool.
- **Pool com VM de uso único:** o boot acontece em background, no máximo 2 simultâneos por linguagem.

## Medições pendentes

- desempenho no Linux (x86_64 e aarch64, KVM);
- latência das demais linguagens além de Python.
