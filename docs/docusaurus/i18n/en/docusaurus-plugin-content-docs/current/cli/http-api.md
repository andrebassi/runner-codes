---
title: 'HTTP API (serve)'
description: 'Start the HTTP API with a pool of pre-booted VMs'
---

## Usage

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
```

| Flag | Default | Description |
|------|--------|-----------|
| `--listen` | `127.0.0.1:8080` (or `RUNNER_LISTEN`) | listen address |
| `--pool-size` | `2` | ready VMs per language |
| `--allow-nested-virt` | off | accepts `nestedVirt` on machine create; without the flag, whoever reaches the port cannot hand the host's virtualization to a guest |

On startup, `serve` prints the address, the pool size and the languages with a ready image. Only those enter the [pool](/concepts/pool).

SIGINT or SIGTERM shut down the server and kill every VM.

## Security

The API **has no authentication**. By default it listens on loopback only. To expose it to another machine, put an authenticated proxy in front of it; do not just switch `--listen` to `0.0.0.0`.

## Endpoints

| Method | Path | Purpose |
|--------|---------|----------|
| GET | `/health` | status, version and ready VMs per stack |
| GET | `/api/v1/languages` | catalog with `ready` |
| POST | `/api/v1/run` | job with `files` and `stdin` (a string, or a list: one VM per item) |
| POST | `/api/v1/execute` | job with bare `code`, `timeout` and `gpu` |
| POST | `/api/v1/execute/stream` | the same, over SSE: output while it runs and `result` at the end |
| POST/GET/DELETE | `/api/v1/machines[/:name]` | create (by fields or by `runner_code`), list, show, delete machines |
| POST | `/api/v1/machines/:name/{start,stop,pause,resume,sync,branch}` | lifecycle |
| POST | `/api/v1/machines/:name/exec[/stream]` | command on the running machine, with `exitCode` |
| PUT/GET | `/api/v1/machines/:name/files` | write and read a file |
| GET | `/api/v1/machines/:name/logs` | console |
| PUT | `/api/v1/machines/:name/egress` | change the egress policy, even while the machine is running |

Bodies, responses, error codes and limits in [API overview](/api-reference/overview). Ready-made clients in
[Go and Python SDKs](/api-reference/sdks).

## Examples

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

With stdin as a list, each item runs in a fresh VM and the response is a list with the stdin echoed back.

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

## Latency

With a full pool, the complete HTTP request took 26–43 ms on the measured machine (macOS 27.0.1, Apple Silicon). With an empty pool, the job waits for a cold boot.
