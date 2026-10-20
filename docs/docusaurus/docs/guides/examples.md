---
title: 'Exemplos e guias'
description: 'Exemplos de runner.code prontos para rodar e guias que provam cada fluxo do rc numa VM real'
---

## Exemplos

Um exemplo por pasta em [`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples), cada um testado
numa VM real no macOS 27 (Apple M1 Pro). Os de máquina são um arquivo [`runner.code`](/cli/runner-code); o `rc` aceita
`-s` como sinônimo de `-f` para apontá-lo.

| Exemplo | O que mostra | Estado |
|---------|--------------|--------|
| [`python-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/python-app) | imagem pública, `init` com `pip` pela rede | funciona |
| [`node-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/node-app) | `init` com `npm -g`, app numa porta publicada | funciona |
| [`openclaw-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/openclaw-app) | gateway de LLM como workload, `/health` pela porta | funciona |
| [`doom-web`](https://github.com/andrebassi/runner-codes/tree/main/examples/doom-web) | servidor estático de um volume, jogo no navegador | funciona |
| [`docker-in-vm`](https://github.com/andrebassi/runner-codes/tree/main/examples/docker-in-vm) | Docker completo dentro da VM | funciona |
| [`headless-browser`](https://github.com/andrebassi/runner-codes/tree/main/examples/headless-browser) | Chromium headless e screenshot | funciona, sem GPU |
| [`gpu-chrome`](https://github.com/andrebassi/runner-codes/tree/main/examples/gpu-chrome) | Fedora com Vulkan/Venus e Chromium | Vulkan funciona; Chromium sem GPU |
| [`local-llm`](https://github.com/andrebassi/runner-codes/tree/main/examples/local-llm) | llama.cpp com Vulkan na GPU do host | funciona: 56,7 tokens/s na GPU contra 10,3 na CPU (Qwen2.5 0.5B Q4_K_M) |
| [`desktop`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop) | desktop Wayland (Weston) na [tela do navegador](/cli/display), com teclado e mouse | funciona |
| [`sdk-go`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-go) | o [SDK Go](/api-reference/sdks): job e ciclo de vida de máquina contra um `rc serve` | funciona |
| [`sdk-python`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-python) | o mesmo com o [SDK Python](/api-reference/sdks) | funciona |

```bash
rc machine run -s examples/python-app/runner.code -- python3 --version
rc machine run -d --name doom -s examples/doom-web/runner.code && open http://localhost:8080
rc machine run -d --name openclaw -s examples/openclaw-app/runner.code && curl localhost:18789/health
```

### Tempo do primeiro start (medido)

O primeiro start roda o `init` (instalação de pacotes); os seguintes reaproveitam o disco da máquina.

| Exemplo | 1º start | Seguintes |
|---------|----------|-----------|
| python-app | 6 s | < 1 s |
| node-app | 2 s | < 1 s |
| openclaw-app | ~73 s (inclui converter a imagem) | < 1 s |
| docker-in-vm | 4 s | 4 s (o dockerd sobe a cada start) |
| headless-browser | 15 s | < 1 s |
| gpu-chrome | 581 s (metadados e pacotes do Fedora) | < 1 s |
| local-llm | 488 s (Fedora + build do llama.cpp) | < 1 s |
| desktop | 12 s | 5 s (udev, seatd e Weston sobem a cada start) |

As notas de cada exemplo (por que o Chromium renderiza sem GPU, as armadilhas do Docker dentro da VM, a tela 2D do
desktop) estão no [`examples/README.md`](https://github.com/andrebassi/runner-codes/tree/main/examples).

## Guias

Cada pasta de [`examples/guides/`](https://github.com/andrebassi/runner-codes/tree/main/examples/guides) é um guia de
uso do `rc`, com um `README.md` que explica o fluxo e um `run.sh` que prova o que o guia diz numa VM de verdade e sai
com o número de falhas.

```bash
task build
task guides                        # todas as pastas
task guides DIRS="pack teardown"   # algumas
```

Na rodada de 20/10/2026 (macOS 27, Apple M1 Pro): 12 pastas, 215 conferências, 0 falhas.

| Guia | O que prova |
|------|-------------|
| [install](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/install) | boot de verdade: marcador do guest e kernel diferente do host |
| [throwaway-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/throwaway-machine) | repo `:ro`, saída gravável, sem rede; rede um host por vez; `--block-private`, loopback do host |
| [pack](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/pack) | imagem e máquina num artefato; o marcador vem junto com a origem apagada; cache reaproveitado |
| [teardown](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/teardown) | parar, apagar, provar limpo; Ctrl-C e `kill -9` no CLI não deixam VM viva |
| [credentials](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/credentials) | o valor nunca entra na VM (busca com contraprova); interceptação só no host da binding; 403 na URL |
| [dev-env](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/dev-env) | persistência de pacote, `$HOME` e disco; `/tmp` zera; init uma vez, como root; `user` e `--user` |
| [docker-in-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/docker-in-machine) | Docker num ext4 de verdade, contêiner aninhado, `docker.sock` no host, dados que sobrevivem |
| [coding-agent](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/coding-agent) | agentes de código instalados pelo init; git no repo montado; agente SSH do host sem a chave na VM; chave da API como segredo |
| [branch-and-checkpoint](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/branch-and-checkpoint) | checkpoint de disco com retenção e trava, restauração de qualquer geração, pause/resume, filhas de uma fonte aquecida, checkpoint exportado como pack |
| [gpu-cuda](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/gpu-cuda) | não há CUDA: `doctor`, CLI, `runner.code` e API dizem não, com o motivo, sem criar nada |
| [local-api](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/local-api) | `rc serve` com corpos JSON de ciclo de vida; `exitCode` no corpo; SSE uma linha por evento; campo desconhecido recusado |
| [reference](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/reference) | uma etapa por tema com prova (`runner.code`, redes nomeadas, nested, limitações, sockets, display, posse de arquivo, S3) e os temas fora do escopo, com o motivo |

O `lib.sh` da pasta reúne o que todo `run.sh` usa: o formato do `rc machine ls`, o processo de uma VM
(`rc vmm --spec <dir>/spec.json`), o diretório da máquina (`rc machine data-dir`) e a lista vazia (`No machines found`).

## Runbook

Além dos guias, o [`docs/runbook.md`](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.md) é a
sequência de teste à mão, passo a passo, com o resultado esperado de cada um; `task runbook` executa os mesmos passos
e reprova se algum sair diferente (116 conferências, 0 falhas em 20/10/2026).
