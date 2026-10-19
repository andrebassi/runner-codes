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

## POST /api/v1/execute/stream

Mesmo corpo do `/api/v1/execute`. A resposta é `text/event-stream` (Server-Sent Events): a saída chega **enquanto
o programa roda**, e um evento `result` fecha a resposta.

```bash
curl -N -X POST http://127.0.0.1:8080/api/v1/execute/stream -H 'content-type: application/json' \
  -d '{"language":"python","code":"import time\nprint(\"early\")\ntime.sleep(1.5)\nprint(\"late\")"}'
```

```
event: stdout
data: "early\n"

event: stdout
data: "late\n"

event: result
data: {"status":"success","exitCode":0,"exception":null,"executionTime":1534,"timedOut":false}
```

| Evento | `data` |
|--------|--------|
| `stdout`, `stderr` | o pedaço de saída como **string JSON** (a quebra de linha vem escapada: SSE não aceita quebra crua no `data`) |
| `result` | `status`, `exitCode`, `exception`, `executionTime` (ms), `timedOut`, e `error` quando houver. Não repete stdout/stderr |
| `error` | `{"error": "..."}`: falha de infraestrutura depois de o stream ter começado (ex.: conexão com o guest caiu) |

- Job recusado na validação responde **400 JSON**, igual ao `/execute`, antes de abrir o stream. Falha de
  infraestrutura antes do primeiro evento responde 500 JSON.
- `stdin` em lista (lote) não é aceito nesta rota: 400.
- O cabeçalho `X-Accel-Buffering: no` desliga o buffer de proxy nginx.
- Linguagens com stdout em bloco quando a saída é pipe (C, Ruby, Perl) só entregam no `flush` do programa; o Python
  roda com `PYTHONUNBUFFERED=1`. Detalhes em [`rc run`](/cli/run).

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

## Máquinas

Ciclo de vida das [máquinas persistentes](/cli/machine), com os nomes de rota do smolvm.

| Método | Caminho | Corpo / query | Resposta |
|--------|---------|---------------|----------|
| POST | `/api/v1/machines` | `{"name","stack","cpus","memory_mib","disk_gib","gpu"}` | 201 + máquina (`state: stopped`) |
| GET | `/api/v1/machines` | — | lista com `state` e `pid` |
| GET | `/api/v1/machines/:name` | — | a máquina |
| DELETE | `/api/v1/machines/:name` | `?force=true` mata se ligada | 204 |
| POST | `/api/v1/machines/:name/start` | — | 200 + máquina (`state: running`) |
| POST | `/api/v1/machines/:name/stop` | `?timeout=N` em segundos (0–300; 0 mata na hora) | 204 |
| POST | `/api/v1/machines/:name/exec` | `{"command":["argv",...],"env":["K=V"],"workdir","stdin","timeout"}` | `{"exitCode","stdout","stderr","timedOut","executionTime"}` |
| POST | `/api/v1/machines/:name/exec/stream` | mesmo corpo | SSE: `stdout`, `stderr` e `exit` (`{"exitCode":N}`) |
| PUT | `/api/v1/machines/:name/files?path=/abs[&mode=644]` | corpo cru = conteúdo | `{"path","bytes"}` |
| GET | `/api/v1/machines/:name/files?path=/abs` | — | conteúdo cru (`application/octet-stream`) |
| GET | `/api/v1/machines/:name/logs?tail=N` | — | console em texto |

```bash
curl -s -X POST localhost:8080/api/v1/machines -d '{"name":"dev","stack":"python"}'
curl -s -X POST localhost:8080/api/v1/machines/dev/start
curl -s -X POST localhost:8080/api/v1/machines/dev/exec -d '{"command":["python3","-c","print(42)"]}'
# {"exitCode":0,"stdout":"42\n","stderr":"","timedOut":false,"executionTime":31}
curl -s -X PUT --data-binary @app.py 'localhost:8080/api/v1/machines/dev/files?path=/workspace/app.py'
```

O `command` é um argv **sem shell implícito**. Para usar shell, mande `["sh","-c","..."]`.

Erros das rotas de máquina:

```json
{"error":{"code":"INVALID_STATE","message":"INVALID_STATE: machine dev is not running; start it first"}}
```

| Código | HTTP |
|--------|------|
| `NOT_FOUND` | 404 |
| `CONFLICT`, `INVALID_STATE` | 409 |
| `CONFIG_ERROR` | 400 |
| `RUNNER_ERROR` (infraestrutura) | 500 |

## Protocolo interno

Entre o host e o guest, o job viaja por vsock em frames de tamanho + JSON. Veja [Job request](/api-reference/job-request), [Job response](/api-reference/job-response) e [Protocolo vsock](/api-reference/vsock-protocol).
