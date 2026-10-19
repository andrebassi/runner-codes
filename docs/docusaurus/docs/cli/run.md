---
title: 'run'
description: 'Executar um código numa microVM nova pela linha de comando'
---

## Uso

```bash
rc run --stack <linguagem> (--code '<código>' | --file <arquivo>) [--stdin '<texto>'] [--timeout 5s]
```

| Flag | Padrão | Descrição |
|------|--------|-----------|
| `--stack` | — | linguagem do catálogo |
| `--code` | — | código do arquivo principal |
| `--file` | — | lê o código de um arquivo local (substitui `--code`) |
| `--stdin` | vazio | entrada padrão do programa |
| `--timeout` | `10s` | duração no formato Go (`500ms`, `5s`, `2m`); máximo 300 s |

## O que acontece

O `run` não usa pool: sobe uma VM nova (boot frio), executa o job e destrói a VM. Na máquina medida, um job Python levou ~240 ms no total, com 31–38 ms de execução.

- O stdout do programa vai para o stdout.
- O stderr do programa vai para o stderr, seguido de uma linha de resumo:

```
[status=success exit=0 exec=33ms total=241ms timeout=false]
```

(valores ilustrativos)

## Código de saída

| Situação | rc |
|----------|----|
| programa saiu com 0 | 0 |
| programa saiu com código ≠ 0, falhou ao compilar ou estourou o timeout | 1 |
| job recusado (linguagem, limites) ou falha de infraestrutura | 1, com a mensagem de erro |

## Exemplos

```bash
# Python com stdin
rc run --stack python --code 'print(input().upper())' --stdin 'olá'

# Arquivo local
rc run --stack go --file ./main.go

# Timeout curto
rc run --stack bash --code 'sleep 10' --timeout 2s
```

Pelo Taskfile:

```bash
task run -- --stack python --code 'print(1)'
```

## Pré-requisito

A imagem da linguagem precisa estar construída (`rc image build --stack <l>`); sem ela, o job é recusado com E006.
