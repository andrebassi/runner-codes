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
| `--gpu` | desligado | VM com a GPU do host por Vulkan (virtio-gpu/Venus); só em stack com `gpu` no catálogo, senão E007. Veja [GPU](/advanced/gpu) |

## O que acontece

O `run` não usa pool: sobe uma VM nova (boot frio), executa o job e destrói a VM. Na máquina medida, um job Python levou ~240 ms no total, com 31–38 ms de execução.

A saída sai em **streaming**: cada pedaço aparece no terminal enquanto o programa roda, não só no fim.

- O stdout do programa vai para o stdout.
- O stderr do programa vai para o stderr, seguido de uma linha de resumo:

```
[status=success exit=0 exec=33ms total=241ms timeout=false]
```

(valores ilustrativos)

### Buffer da linguagem

O streaming entrega o que o programa **escreve** no pipe. Quando a saída não é um terminal, várias linguagens
acumulam o stdout num buffer e só escrevem no fim (ou a cada bloco):

| Linguagem | Comportamento no streaming |
|-----------|----------------------------|
| Python | linha a linha: o runner liga `PYTHONUNBUFFERED=1` no modo streaming |
| Node.js, Go, Bash, sh | linha a linha, sem configuração |
| C, C++ (`printf`), Ruby, Perl | em bloco: chama `fflush(stdout)`, `$stdout.sync = true` ou `$\| = 1` no programa |
| stderr | sem buffer em todas |

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
