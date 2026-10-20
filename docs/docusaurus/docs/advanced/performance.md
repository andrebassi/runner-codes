---
title: 'Desempenho'
description: 'Números medidos de boot, execução, máquinas, memória e build de imagem'
---

## Onde foi medido

Todos os números desta página foram medidos no **macOS 27, Apple Silicon (M1 Pro)**, em 19 e 20/10/2026. **O Linux
ainda não foi medido**; não assuma que os valores se repetem com KVM.

## Boot

| Medida | Valor |
|--------|-------|
| boot até a 1ª resposta do guest Python, root ext4 read-only, 30 boots | p50 260 ms, p95 266 ms |
| efeito da RAM configurada (256 a 2048 MiB) no boot | nenhum |

"Resposta" é a primeira resposta do guest ao job `sh true` que a espera de prontidão manda a cada 1 ms. É o momento
em que a VM pode receber um job real.

## Execução de um job

| Caminho | Valor |
|---------|-------|
| CLI, job Python, boot frio | ~240 ms no total, dos quais 31–38 ms de execução |
| API, pool cheio, uma stack com imagem | 26–43 ms de requisição HTTP completa |
| API, 15 stacks, todos os pools cheios | 49–175 ms |

A diferença entre CLI e API é o boot, que o [pool](/concepts/pool) tira do caminho da requisição. Com o pool vazio, a
API cai no mesmo boot frio da CLI.

Por stack, pela CLI com boot frio:

| Stack | Execução | Total (com boot) |
|-------|----------|------------------|
| python | 31–44 ms | ~240 ms |
| nodejs | 128–148 ms | 353–371 ms |
| go | 247 ms com o cache de build aquecido (3,4 s sem ele) | 510 ms |
| java | 522 ms | 739 ms |
| c | 71–72 ms | 282 ms |
| sh | 2 ms | 199 ms |

## Máquinas

| Medida | Valor |
|--------|-------|
| `machine start` até o guest responder (python) | 204–244 ms |
| `start` de máquina com rede | 190 ms |
| disco de 10 GiB recém-criado, espaço no host | 6,4 MB |
| `cp` de 50 MB para a máquina / da máquina | 620 ms (~84 MB/s) / 594 ms (~88 MB/s) |
| nome bloqueado pela política até o erro no guest | < 1 s |

## Memória

| Medida | Valor |
|--------|-------|
| footprint de uma VM parada | 44 MB |
| RSS da mesma VM | 172 MB, incluindo páginas limpas compartilhadas |

A memória da libkrun é elástica: o host paga só as páginas que o guest toca. O teto de RAM por linguagem no catálogo
limita o abuso, mas não é reservado.

## Build de imagem e pack

| Medida | Valor |
|--------|-------|
| build da imagem Python | ~8–10 s |
| pull + achatamento das camadas (python) | 2,7 s |
| warmup do Python (`compileall`) | ~1 s |
| build da imagem Go | 35 s, dos quais 28 s de `mkfs.ext4` |
| pack da imagem python | 134 MiB → payload de 31 MiB |
| pack de uma máquina python com pacote instalado, arquivo único | 46 MiB |

O build é custo único por chave de cache; um segundo build com as mesmas entradas sai do cache. O tempo de cada stack
está no [Relatório de testes](/advanced/test-report).

## GPU

`llama-bench` com Qwen2.5 0.5B: processar o prompt saiu a 893–1.365 tokens/s na GPU (Venus) contra 193–264 na CPU.
Detalhes e a leitura dos números em [GPU](/advanced/gpu).

## O que o desempenho deve ao desenho

- **Root em disco ext4 read-only**, não virtiofs: virtiofs não foi mais rápido e não confina o guest aos diretórios
  compartilhados.
- **Warmup gravado na imagem:** o `.pyc` da stdlib do Python e o cache de build do Go já vêm prontos; para Go, a
  cópia para o tmpfs acontece no boot, antes de a VM entrar no pool.
- **Pool com VM de uso único:** o boot acontece em background, no máximo 2 simultâneos por linguagem.
- **Disco de máquina clonado:** clonefile no APFS; o disco novo não copia os 10 GiB.

## Medições pendentes

- desempenho no Linux (x86_64 e aarch64, KVM);
- máquinas com GPU em modelo maior que 0,5B.
