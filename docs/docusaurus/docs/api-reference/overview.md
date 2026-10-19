---
title: 'Visão geral da API'
description: 'Endpoints HTTP, formatos de requisição e resposta, erros e limites'
---

## Base

A API sobe com `rc serve` e escuta por padrão em `http://127.0.0.1:8080`. **Não há autenticação.** O padrão é só loopback; não exponha fora dela sem um proxy autenticado na frente.

| Método | Caminho | O que faz |
|--------|---------|-----------|
| GET | `/health` | estado e versão |
| GET | `/api/v1/languages` | catálogo e se a imagem de cada linguagem está pronta |
| POST | `/api/v1/run` | executa arquivos, com stdin único ou em lote |
| POST | `/api/v1/execute` | executa `code` solto ou arquivos, com timeout por requisição |

## GET /health

```json
{"status": "ok", "version": "dev"}
```

`version` é a versão gravada no binário no release; em build local, `dev`.

## GET /api/v1/languages

```json
[
  {"name": "python", "image": "python:3.12-alpine", "ready": true},
  {"name": "nodejs", "image": "node:22-alpine", "ready": false}
]
```

`ready` diz se o ext4 da linguagem já foi construído neste host. Linguagem com `ready: false` é recusada com E006, como se não estivesse no catálogo; construa a imagem com [`image build`](/cli/image).

## POST /api/v1/run

```json
{
  "language": "python",
  "files": [{"name": "main.py", "content": "print(input())"}],
  "stdin": "olá"
}
```

| Campo | Tipo | Obrigatório | Descrição |
|-------|------|-------------|-----------|
| `language` | string | sim | nome no catálogo |
| `files` | lista de `{name, content}` | sim | o primeiro arquivo é o principal |
| `stdin` | string ou lista | não | lista roda uma vez por item, cada uma numa VM nova |

Resposta com stdin único:

```json
{"status": "success", "stdout": "olá\n", "stderr": null, "exception": null, "executionTime": 33}
```

Com `"stdin": ["a", "b"]`, a resposta é uma lista, com o stdin ecoado em cada item:

```json
[
  {"status": "success", "stdout": "a\n", "stderr": null, "exception": null, "executionTime": 32, "stdin": "a"},
  {"status": "success", "stdout": "b\n", "stderr": null, "exception": null, "executionTime": 31, "stdin": "b"}
]
```

Os valores de `executionTime` acima são ilustrativos.

## POST /api/v1/execute

```json
{
  "language": "python",
  "code": "import time; time.sleep(2); print('ok')",
  "timeout": 5
}
```

| Campo | Tipo | Obrigatório | Descrição |
|-------|------|-------------|-----------|
| `language` | string | sim | nome no catálogo |
| `code` | string | não | código do arquivo principal, quando não há `files` |
| `files` | lista de `{name, content}` | não | arquivos; o primeiro é o principal |
| `stdin` | string | não | entrada padrão |
| `timeout` | inteiro (segundos) | não | padrão 10, máximo 300 |

A resposta tem o mesmo formato de `/api/v1/run`.

## Campos da resposta

| Campo | Descrição |
|-------|-----------|
| `status` | `"success"` quando a API executou o job. **Não** quer dizer que o programa deu certo: veja `stderr` e `exception` |
| `stdout` | saída padrão |
| `stderr` | saída de erro, ou `null` quando vazia |
| `exception` | `null`, `"Timeout"`, `"Compilation failed"` ou a descrição de uma falha de infraestrutura |
| `executionTime` | tempo de execução dentro da VM, em ms |
| `error` | presente em recusa (E005, E006 ou o motivo) e quando o guest não responde a tempo (E001) |
| `stdin` | só no modo lote: o stdin daquele item |

## Erros e status HTTP

| Situação | HTTP | Corpo |
|----------|------|-------|
| execução concluída (inclusive programa que falhou ou estourou o tempo) | 200 | resposta normal |
| JSON inválido ou campo obrigatório ausente | 400 | `{"status": "failed", "error": "requisição inválida: ..."}` |
| stdin acima de 1 MB | 400 | `"error": "E005: stdin too long"` |
| linguagem fora do catálogo ou sem imagem construída neste host | 400 | `"error": "E006: unsupported language <nome>"` |
| outro limite do domínio violado (timeout fora de (0, 300 s], job sem código e sem arquivos, arquivo repetido, nome inválido, muitos arquivos, código grande demais) | 400 | `"error"` com o motivo |
| guest não respondeu dentro do timeout mais a folga do host | 200 | `{"status": "failed", "error": "E001: operation timed out"}` |
| falha de infraestrutura (VM não subiu, erro no canal com o guest) | 500 | `{"status": "failed", "exception": "..."}` |

## Limites

| Limite | Valor |
|--------|-------|
| stdin | ≤ 1.000.000 bytes |
| arquivos por job | até 32 |
| código total (soma dos arquivos) | ≤ 4 MiB |
| nome de arquivo | sem `/`, sem `..`, sem começar com `.`, ≤ 255 bytes |
| timeout | padrão 10 s, máximo 300 s |

## Protocolo interno

Entre o host e o guest, o job viaja por vsock em frames de tamanho + JSON. Veja [Job request](/api-reference/job-request), [Job response](/api-reference/job-response) e [Protocolo vsock](/api-reference/vsock-protocol).
