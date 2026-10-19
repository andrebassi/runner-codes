---
title: 'Testes'
description: 'Testes de unidade, gate de cobertura, gate multi-alvo e testes ponta a ponta'
---

## task test

```bash
task test
```

Testes de unidade de `./internal/...` com race detector. Não precisa da libkrun.

## task test:cov

```bash
task test:cov
```

Gate de cobertura que **reprova** (rc ≠ 0), não só relata:

- cobertura total ≥ 90% (piso ajustável por `COVERAGE_FLOOR`);
- domínio (`internal/domain`) em 100%.

Fora da conta, com o motivo escrito no script:

| Excluído | Motivo |
|----------|--------|
| `cmd/runner.code` | wiring: só liga as peças; verificado pelo e2e |
| `internal/guest` | executor herdado, que só roda com as toolchains dentro da VM; medido à parte como dívida declarada, com catraca que reprova se cair |

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

Roda contra a libkrun real, com VMs de verdade. Precisa das imagens construídas. O que ele prova:

| Teste | Prova |
|-------|-------|
| stdout e exit code | o resultado chega inteiro e o código de saída é repassado |
| timeout | o job que passa do tempo é morto e reportado como timeout |
| sem rede | conexão a `1.1.1.1` falha dentro da VM; com rede ligada de propósito, o teste reprova (prova de falha do próprio teste) |
| root read-only e isolamento | escrever no root falha; arquivo criado num job não existe no job seguinte |
| teto de memória | alocar 2 GiB numa VM de 512 MiB falha sem afetar o host |
| famílias de linguagem | interpretadas, compiladas e JVM executam |
| `serve` com pool | a API responde usando VMs do pool |

## task gates

```bash
task gates
```

Rodada completa: `test:targets`, `test:cov` e `test:e2e`, nessa ordem.

## Plataformas testadas

Os testes ponta a ponta e as medições de desempenho foram feitos no macOS 27.0.1, Apple Silicon. No Linux, o gate multi-alvo garante que compila; execução e desempenho no Linux ainda não foram medidos.
