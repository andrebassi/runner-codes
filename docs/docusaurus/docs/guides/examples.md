---
title: 'Exemplos e guias'
description: 'Exemplos dos SDKs e guias que provam cada fluxo do rc numa VM real'
---

## Exemplos

Em [`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples) ficam os exemplos dos SDKs, cada um
testado contra um `rc serve` local no macOS 27 (Apple M1 Pro). Para declarar uma máquina num arquivo versionado, veja
o formato do [`runner.code`](/cli/runner-code).

| Exemplo | O que mostra | Estado |
|---------|--------------|--------|
| [`sdk-go`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-go) | o [SDK Go](/api-reference/sdks): job e ciclo de vida de máquina contra um `rc serve` | funciona |
| [`sdk-python`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-python) | o mesmo com o [SDK Python](/api-reference/sdks) | funciona |

## Guias

Cada pasta de [`examples/guides/`](https://github.com/andrebassi/runner-codes/tree/main/examples/guides) é um guia de
uso do `rc`, com um `README.md` que explica o fluxo e um `run.sh` que prova o que o guia diz numa VM de verdade e sai
com o número de falhas.

```bash
task build
task guides                        # todas as pastas
task guides DIRS="pack teardown"   # algumas
```

Na rodada de 21/10/2026 (macOS 27, Apple M1 Pro): 9 pastas, 175 conferências, 0 falhas.

| Guia | O que prova |
|------|-------------|
| [install](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/install) | boot de verdade: marcador do guest e kernel diferente do host |
| [throwaway-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/throwaway-machine) | repo `:ro`, saída gravável, sem rede; rede um host por vez; `--block-private`, loopback do host |
| [pack](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/pack) | imagem e máquina num artefato; o marcador vem junto com a origem apagada; cache reaproveitado |
| [teardown](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/teardown) | parar, apagar, provar limpo; Ctrl-C e `kill -9` no CLI não deixam VM viva |
| [credentials](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/credentials) | o valor nunca entra na VM (busca com contraprova); interceptação só no host da binding; 403 na URL |
| [branch-and-checkpoint](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/branch-and-checkpoint) | checkpoint de disco com retenção e trava, restauração de qualquer geração, pause/resume, filhas de uma fonte aquecida, checkpoint exportado como pack |
| [gpu-cuda](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/gpu-cuda) | não há CUDA: `doctor`, CLI, `runner.code` e API dizem não, com o motivo, sem criar nada |
| [local-api](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/local-api) | `rc serve` com corpos JSON de ciclo de vida; `exitCode` no corpo; SSE uma linha por evento; campo desconhecido recusado |
| [reference](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/reference) | uma etapa por tema com prova (`runner.code`, redes nomeadas, nested, limitações, sockets, display, posse de arquivo, S3) e os temas fora do escopo, com o motivo |

O `lib.sh` da pasta reúne o que todo `run.sh` usa: o formato do `rc machine ls`, o processo de uma VM
(`rc vmm --spec <dir>/spec.json`), o diretório da máquina (`rc machine data-dir`) e a lista vazia (`No machines found`).

## Runbook

Além dos guias, o [`docs/runbook.md`](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.md) é a
sequência de teste à mão, passo a passo, com o resultado esperado de cada um; `task runbook` executa os mesmos passos
e reprova se algum sair diferente (146 conferências, 0 falhas em 24/10/2026).
