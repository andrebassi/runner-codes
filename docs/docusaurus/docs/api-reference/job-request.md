---
title: 'Job request'
description: 'O job que o host envia ao guest pelo vsock'
---

## Formato

O host converte a requisição HTTP (ou a chamada da CLI) num job e o envia ao guest como um frame vsock (4 bytes big-endian de tamanho + JSON).

```json
{
  "trace_id": "tr-123",
  "lang": "python",
  "code": "",
  "files": [
    {"name": "main.py", "content": "import util\nprint(util.dobro(int(input())))"},
    {"name": "util.py", "content": "def dobro(x):\n    return 2 * x\n"}
  ],
  "stdin": "21",
  "timeout": 10
}
```

## Campos

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `trace_id` | string | identificador do job, devolvido no resultado |
| `lang` | string | linguagem; precisa existir no executor do guest |
| `code` | string | código do arquivo principal quando não há `files` |
| `files` | lista de `{name, content}` | arquivos do job; o **primeiro** é o principal |
| `stdin` | string | entrada padrão; omitido quando vazio |
| `timeout` | inteiro | segundos |
| `stream` | booleano | pede a saída em pedaços antes do resultado; veja [Streaming](/api-reference/vsock-protocol#streaming) |
| `op` | string | operação; vazio é o job de código. As operações de máquina (`exec`, `put`, `pty`...) estão em [Protocolo vsock](/api-reference/vsock-protocol#operações) |

## Arquivo principal

- Com `files`, cada arquivo é gravado pelo nome num diretório temporário em `/tmp`, e o primeiro é o executado (ou compilado).
- Com só `code`, o guest grava `script.<ext>` — exceto Java, que vira `Main.java`, porque a classe executada é `Main`.

## Regras aplicadas antes de o job chegar ao guest

O domínio, no host, recusa o job (HTTP 400 na API) quando:

| Regra | Limite |
|-------|--------|
| linguagem | no catálogo e com imagem construída neste host (senão E006) |
| stdin | ≤ 1.000.000 bytes (senão E005) |
| timeout | > 0 e ≤ 300 s; ausente vira 10 s |
| conteúdo | `code` ou ao menos um arquivo |
| arquivos | até 32, sem nome repetido |
| tamanho total | soma de `code` e conteúdos ≤ 4 MiB |
| nome de arquivo | sem `/`, sem `..`, sem começar com `.`, ≤ 255 bytes |

As regras de nome impedem escrita fora do diretório do job dentro do guest, mesmo a VM sendo descartável.

## Prontidão

Antes do primeiro job real, o host manda à VM um job `sh true` repetidamente (a cada 1 ms) até receber resposta. É assim que ele sabe que o guest já está escutando.
