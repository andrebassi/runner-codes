---
title: 'API HTTP (serve)'
description: 'Subir a API HTTP com pool de VMs pré-bootadas'
---

## Uso

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
```

| Flag | Padrão | Descrição |
|------|--------|-----------|
| `--listen` | `127.0.0.1:8080` (ou `RUNNER_LISTEN`) | endereço de escuta |
| `--pool-size` | `2` | VMs prontas por linguagem |
| `--allow-nested-virt` | desligado | aceita `nestedVirt` no create de máquina; sem a flag, quem alcança a porta não consegue entregar a virtualização do host a um guest |

Ao subir, o `serve` imprime o endereço, o tamanho do pool e as linguagens com imagem pronta. Só essas entram no [pool](/concepts/pool).

SIGINT ou SIGTERM fecham o servidor e matam todas as VMs.

## Segurança

A API **não tem autenticação**. O padrão é escutar só em loopback. Para expor a outra máquina, coloque um proxy autenticado na frente; não troque `--listen` por `0.0.0.0` sozinho.

## Endpoints

| Método | Caminho | Para quê |
|--------|---------|----------|
| GET | `/health` | estado, versão e VMs prontas por stack |
| GET | `/api/v1/languages` | catálogo com `ready` |
| POST | `/api/v1/run` | job com `files` e `stdin` (string, ou lista: uma VM por item) |
| POST | `/api/v1/execute` | job com `code` solto, `timeout` e `gpu` |
| POST | `/api/v1/execute/stream` | o mesmo, em SSE: saída enquanto roda e `result` no fim |
| POST/GET/DELETE | `/api/v1/machines[/:name]` | criar (por campos ou por `runner_code`), listar, ver, apagar máquinas |
| POST | `/api/v1/machines/:name/{start,stop,pause,resume,sync,branch}` | ciclo de vida |
| POST | `/api/v1/machines/:name/exec[/stream]` | comando na máquina ligada, com `exitCode` |
| PUT/GET | `/api/v1/machines/:name/files` | gravar e ler arquivo |
| GET | `/api/v1/machines/:name/logs` | console |
| PUT | `/api/v1/machines/:name/egress` | trocar a política de saída, inclusive com a máquina ligada |

Corpos, respostas, códigos de erro e limites em [Visão geral da API](/api-reference/overview). Clientes prontos em
[SDKs Go e Python](/api-reference/sdks).

## Exemplos

```bash
curl -s http://127.0.0.1:8080/health
```

```bash
curl -s http://127.0.0.1:8080/api/v1/languages
```

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{"language":"python","files":[{"name":"main.py","content":"print(int(input())*2)"}],"stdin":["1","2","3"]}'
```

Com stdin em lista, cada item roda numa VM nova e a resposta é uma lista com o stdin ecoado.

```bash
curl -s http://127.0.0.1:8080/api/v1/execute \
  -H 'Content-Type: application/json' \
  -d '{"language":"bash","code":"echo $((6*7))","timeout":5}'
```

```bash
curl -s -X POST http://127.0.0.1:8080/api/v1/machines -d '{"name":"dev","stack":"python"}'
curl -s -X POST http://127.0.0.1:8080/api/v1/machines/dev/start
curl -s -X POST http://127.0.0.1:8080/api/v1/machines/dev/exec -d '{"command":["python3","-c","print(42)"]}'
```

## Latência

Com o pool cheio, a requisição HTTP completa levou 26–43 ms na máquina medida (macOS 27.0.1, Apple Silicon). Com o pool vazio, o job espera um boot frio.
