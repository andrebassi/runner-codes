---
title: 'Testes'
description: 'Testes de unidade, gate de cobertura, gate multi-alvo e testes ponta a ponta'
---

## task test

```bash
task test
```

Testes de unidade de `./internal/...` com race detector. Não precisa da libkrun. Na rodada de 20/10/2026: 20 pacotes,
todos verdes.

## task test:cov

```bash
task test:cov
```

Gate de cobertura que **reprova** (rc ≠ 0), não só relata:

- cobertura total de `./internal/...` e `./pkg/...` ≥ 90% (piso ajustável por `COVERAGE_FLOOR`);
- domínio (`internal/domain`) em 100%;
- guest (`internal/guest`) acima da catraca (`GUEST_FLOOR`, hoje 75%);
- SDK Python (`sdk/python`) com linha e branch ≥ 90% (`task sdk:py:cov`).

Na rodada de 20/10/2026: 95,2% total, domínio 100%, guest 75%, SDK Python 100%.

Fora da conta, com o motivo escrito no script:

| Excluído | Motivo |
|----------|--------|
| `cmd/rc` | wiring: só liga as peças; verificado pelo e2e |
| `internal/guest` (do total) | executor herdado, que só roda com as toolchains dentro da VM; medido à parte como dívida declarada, com catraca que reprova se cair |

Prova de que o gate morde: `COVERAGE_FLOOR=101 task test:cov` precisa sair com rc ≠ 0.

## task test:targets

```bash
task test:targets
```

| Alvo | Esperado |
|------|----------|
| darwin/arm64 | compila |
| linux/amd64 | compila |
| linux/arm64 | compila |
| windows/amd64 | **falha**, com o motivo declarado |

Também reprova arquivo com build tag de SO que não mire um alvo com decisão.

## task test:e2e

```bash
task image:build -- --all
task test:e2e
```

Roda contra a libkrun real, com VMs de verdade. Precisa das imagens construídas (a `llama` para os testes de GPU). São
42 testes; na rodada de 20/10/2026, os 42 passaram em duas rodadas seguidas. Alguns do que eles provam:

| Área | Prova |
|------|-------|
| job | stdout e exit code; timeout que mata a VM sem deixar processo; sem rede (com prova de falha); root read-only e nada passa de um job a outro; teto de memória; famílias de linguagem |
| `shell` | TTY de verdade, Ctrl+C interrompe só o comando |
| `serve` | pool, SSE com a 1ª linha antes do fim, rotas de máquina |
| máquinas | ciclo de vida e persistência, terminal, `runner.code`, `machine run`, branch e pausa, cópia de arquivo |
| rede | sem `--net` não há `eth0`; política por host e por CIDR; metadata recusada; ICMP; rede nomeada |
| volumes e pontes | `:ro` resiste a remount, `:staged`, disco, S3, `docker.sock`, `--expose-socket`/`--mount-socket` |
| credenciais | placeholder no guest, valor real no serviço; segredos e agente SSH |
| GPU, tela, pack | Venus visível com `--gpu`; quadro e entrada pela tela; pack que preserva o disco |

A lista completa, teste por teste, está no [Relatório de testes](/advanced/test-report).

## task runbook

```bash
task runbook
```

Executa o `docs/runbook.md` passo a passo e confere cada resultado esperado. Todo recurso novo ganha, na mesma entrega,
uma seção no runbook e o passo no `scripts/runbook.sh`. Na rodada de 21/10/2026: 126 conferências, 0 falhas.

## task guides

```bash
task guides
task guides DIRS="pack teardown"
```

Cada pasta de `examples/guides` tem um `run.sh` que prova o que o guia diz numa VM de verdade e sai com o número de
falhas. Na rodada de 21/10/2026: 9 pastas, 175 conferências, 0 falhas. Veja [Exemplos e guias](/guides/examples).

## task gates

```bash
task gates
```

Rodada completa: `test:targets`, `test:cov`, `test:e2e` e `runbook`, nessa ordem.

## Plataformas testadas

Os testes ponta a ponta, o runbook, os guias e as medições de desempenho foram feitos no macOS 27, Apple Silicon. No
Linux, o gate multi-alvo garante que compila, e o confinamento Landlock + seccomp foi testado no Ubuntu 26.04 sem KVM;
uma microVM completa no Linux ainda não foi medida. Detalhes em [Relatório de testes](/advanced/test-report).
