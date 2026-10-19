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

Ao subir, o `serve` imprime o endereço, o tamanho do pool e as linguagens com imagem pronta. Só essas entram no [pool](/concepts/pool).

SIGINT ou SIGTERM fecham o servidor e matam todas as VMs.

## Segurança

A API **não tem autenticação**. O padrão é escutar só em loopback. Para expor a outra máquina, coloque um proxy autenticado na frente; não troque `--listen` por `0.0.0.0` sozinho.

## Endpoints

| Método | Caminho |
|--------|---------|
| GET | `/health` |
| GET | `/api/v1/languages` |
| POST | `/api/v1/run` |
| POST | `/api/v1/execute` |
| POST | `/api/v1/execute/stream` (SSE: saída enquanto roda) |

Formatos, erros e limites em [Visão geral da API](/api-reference/overview).

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

## Latência

Com o pool cheio, a requisição HTTP completa levou 26–43 ms na máquina medida (macOS 27.0.1, Apple Silicon). Com o pool vazio, o job espera um boot frio.
