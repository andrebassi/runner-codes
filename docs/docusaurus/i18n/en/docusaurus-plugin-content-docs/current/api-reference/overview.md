---
title: 'API overview'
description: 'HTTP endpoints, request and response formats, errors and limits'
---

## Basics

The API starts with `rc serve` and listens on `http://127.0.0.1:8080` by default. **There is no authentication.** The default is loopback only; do not expose it beyond that without an authenticating proxy in front of it.

| Method | Path | What it does |
|--------|------|--------------|
| GET | `/health` | status and version |
| GET | `/api/v1/languages` | catalog and whether each language's image is ready |
| POST | `/api/v1/run` | runs files, with a single stdin or in batch |
| POST | `/api/v1/execute` | runs standalone `code` or files, with a per-request timeout |

## GET /health

```json
{"status": "ok", "version": "dev"}
```

`version` is the version stamped into the binary at release time; in a local build, `dev`.

## GET /api/v1/languages

```json
[
  {"name": "python", "image": "python:3.12-alpine", "ready": true},
  {"name": "nodejs", "image": "node:22-alpine", "ready": false}
]
```

`ready` says whether the language's ext4 has already been built on this host. A language with `ready: false` is rejected with E006, as if it were not in the catalog; build the image with [`image build`](/cli/image).

## POST /api/v1/run

```json
{
  "language": "python",
  "files": [{"name": "main.py", "content": "print(input())"}],
  "stdin": "olá"
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `language` | string | yes | name in the catalog |
| `files` | list of `{name, content}` | yes | the first file is the main one |
| `stdin` | string or list | no | a list runs once per item, each in a fresh VM |

Response with a single stdin:

```json
{"status": "success", "stdout": "olá\n", "stderr": null, "exception": null, "executionTime": 33}
```

With `"stdin": ["a", "b"]`, the response is a list, with the stdin echoed in each item:

```json
[
  {"status": "success", "stdout": "a\n", "stderr": null, "exception": null, "executionTime": 32, "stdin": "a"},
  {"status": "success", "stdout": "b\n", "stderr": null, "exception": null, "executionTime": 31, "stdin": "b"}
]
```

The `executionTime` values above are illustrative.

## POST /api/v1/execute

```json
{
  "language": "python",
  "code": "import time; time.sleep(2); print('ok')",
  "timeout": 5
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `language` | string | yes | name in the catalog |
| `code` | string | no | code of the main file, when there are no `files` |
| `files` | list of `{name, content}` | no | files; the first one is the main one |
| `stdin` | string | no | standard input |
| `timeout` | integer (seconds) | no | default 10, maximum 300 |

The response has the same format as `/api/v1/run`.

## POST /api/v1/execute/stream

Same body as `/api/v1/execute`. The response is `text/event-stream` (Server-Sent Events): the output arrives **while
the program runs**, and a `result` event closes the response.

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

| Event | `data` |
|-------|--------|
| `stdout`, `stderr` | the output chunk as a **JSON string** (the newline comes escaped: SSE does not accept a raw newline in `data`) |
| `result` | `status`, `exitCode`, `exception`, `executionTime` (ms), `timedOut`, and `error` when present. It does not repeat stdout/stderr |
| `error` | `{"error": "..."}`: infrastructure failure after the stream has started (e.g. the connection to the guest dropped) |

- A job rejected by validation responds with **400 JSON**, just like `/execute`, before opening the stream. An
  infrastructure failure before the first event responds with 500 JSON.
- A list `stdin` (batch) is not accepted on this route: 400.
- The `X-Accel-Buffering: no` header disables nginx proxy buffering.
- Languages that block-buffer stdout when the output is a pipe (C, Ruby, Perl) only deliver on the program's `flush`;
  Python runs with `PYTHONUNBUFFERED=1`. Details in [`rc run`](/cli/run).

## Response fields

| Field | Description |
|-------|-------------|
| `status` | `"success"` when the API ran the job. It does **not** mean the program succeeded: see `stderr` and `exception` |
| `stdout` | standard output |
| `stderr` | error output, or `null` when empty |
| `exception` | `null`, `"Timeout"`, `"Compilation failed"` or the description of an infrastructure failure |
| `executionTime` | execution time inside the VM, in ms |
| `error` | present on rejection (E005, E006 or the reason) and when the guest does not respond in time (E001) |
| `stdin` | batch mode only: that item's stdin |

## Errors and HTTP status

| Situation | HTTP | Body |
|-----------|------|------|
| execution completed (including a program that failed or timed out) | 200 | normal response |
| invalid JSON or missing required field | 400 | `{"status": "failed", "error": "requisição inválida: ..."}` |
| stdin above 1 MB | 400 | `"error": "E005: stdin too long"` |
| language outside the catalog or without an image built on this host | 400 | `"error": "E006: unsupported language <nome>"` |
| another domain limit violated (timeout outside (0, 300 s], job with no code and no files, repeated file, invalid name, too many files, code too large) | 400 | `"error"` with the reason |
| guest did not respond within the timeout plus the host grace period | 200 | `{"status": "failed", "error": "E001: operation timed out"}` |
| infrastructure failure (VM did not boot, error on the channel to the guest) | 500 | `{"status": "failed", "exception": "..."}` |

## Limits

| Limit | Value |
|-------|-------|
| stdin | ≤ 1,000,000 bytes |
| files per job | up to 32 |
| total code (sum of the files) | ≤ 4 MiB |
| file name | no `/`, no `..`, not starting with `.`, ≤ 255 bytes |
| timeout | default 10 s, maximum 300 s |

## Machines

Lifecycle of [persistent machines](/cli/machine).

| Method | Path | Body / query | Response |
|--------|------|--------------|----------|
| POST | `/api/v1/machines` | `{"name","stack"\|"image","cpus","memory_mib","disk_gib","gpu","gpu_vram_mib","env","workdir","init","workload","stop_signal","stop_timeout","stop_on_exit","net","ports":["8080:80"],"egress":{"allow_hosts",...},"network":"lab"}` or `{"name","runner_code":"<TOML>"}` (non-empty fields win over the file) | 201 + machine (`state: stopped`) |
| GET | `/api/v1/machines` | — | list with `state` and `pid` |
| GET | `/api/v1/machines/:name` | — | the machine |
| DELETE | `/api/v1/machines/:name` | `?force=true` kills it if running | 204 |
| POST | `/api/v1/machines/:name/start` | — | 200 + machine (`state: running`) |
| POST | `/api/v1/machines/:name/stop` | `?timeout=N` in seconds (0–300; 0 kills immediately); without it, the machine's `stop_timeout` (or 10) | 204 |
| POST | `/api/v1/machines/:name/exec` | `{"command":["argv",...],"env":["K=V"],"workdir","stdin","timeout"}` | `{"exitCode","stdout","stderr","timedOut","executionTime"}` |
| POST | `/api/v1/machines/:name/exec/stream` | same body | SSE, **one event per line, raw text**: `event: stdout` / `data: line1`; a line that arrives split waits for the rest, and a remainder without `\n` is flushed before `exit` (`{"exitCode","timedOut","executionTime"}`) |
| PUT | `/api/v1/machines/:name/files?path=/abs[&mode=644]` | raw body = content | `{"path","bytes"}` |
| GET | `/api/v1/machines/:name/files?path=/abs` | — | raw content (`application/octet-stream`) |
| PUT/GET | `/api/v1/machines/:name/files/*path` | same, with the path in the URL (`files/%2Ftmp%2Fa.txt` or `files/tmp/a.txt`) | same |
| GET | `/api/v1/machines/:name/logs?tail=N&source=all\|workload` | — | console as text; `workload` filters the workload's output, without the marker |
| PUT | `/api/v1/machines/:name/egress` | `{"allow_hosts","allow_host_patterns","allow_cidrs","deny_cidrs","block_private","allow_private"}` (the whole policy) | 200 + machine; while it is running, it applies on the next connection. `INVALID_STATE` if running without a network card |
| POST | `/api/v1/machines/:name/pause` | — | 204; freezes the running machine (`state: paused`), and exec is rejected until resume |
| POST | `/api/v1/machines/:name/resume` | — | 204; a machine that is already running is not an error |
| POST | `/api/v1/machines/:name/sync` | — | 204; returns the copies of the staged volumes to the host (`INVALID_STATE` if stopped) |
| POST | `/api/v1/machines/:name/branch` | `{"name":"filha"}` | 201 + child (`state: stopped`), a copy-on-write copy of the disk; a running parent freezes only during the clone |

```bash
curl -s -X POST localhost:8080/api/v1/machines -d '{"name":"dev","stack":"python"}'
curl -s -X POST localhost:8080/api/v1/machines/dev/start
curl -s -X POST localhost:8080/api/v1/machines/dev/exec -d '{"command":["python3","-c","print(42)"]}'
# {"exitCode":0,"stdout":"42\n","stderr":"","timedOut":false,"executionTime":31}
curl -s -X PUT --data-binary @app.py 'localhost:8080/api/v1/machines/dev/files?path=/workspace/app.py'
```

`command` is an argv **with no implicit shell**. To use a shell, send `["sh","-c","..."]`.

Alternative forms accepted on create:

| Alternative field | Equivalent to |
|---|---|
| `"network": true` / `false` | `net` (a string is still the network name) |
| `"memoryMb"` | `memory_mib` |
| `"cmd"` | `workload` |
| `"allowedHosts"`, `"allowedCidrs"` | `egress.allow_hosts`, `egress.allow_cidrs`, and they turn the network on |
| `"nestedVirt"` (or `"nested"`) | `--nested`; rejected with 400 unless the server is started with `rc serve --allow-nested-virt` |

An unknown field is **rejected** with 400 (`CONFIG_ERROR`, the message names the field), on the create, exec, egress
and branch routes. Silently ignoring it used to let a mistyped `allowedHost` create the machine without the egress
restriction.

The **job** stream (`/api/v1/execute/stream`) did not change: there, `data` is still a JSON string, because the chunk
may carry bytes without a newline.

Errors from the machine routes:

```json
{"error":{"code":"INVALID_STATE","message":"INVALID_STATE: machine dev is not running; start it first"},"code":"INVALID_STATE"}
```

`code` also appears at the top level of the body, for clients that look for it there.

| Code | HTTP |
|------|------|
| `NOT_FOUND` | 404 |
| `CONFLICT`, `INVALID_STATE` | 409 |
| `CONFIG_ERROR` | 400 |
| `RUNNER_ERROR` (infrastructure) | 500 |

## Internal protocol

Between the host and the guest, the job travels over vsock in length + JSON frames. See [Job request](/api-reference/job-request), [Job response](/api-reference/job-response) and [vsock protocol](/api-reference/vsock-protocol).
