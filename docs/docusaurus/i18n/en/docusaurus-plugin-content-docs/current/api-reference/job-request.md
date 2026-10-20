---
title: 'Job request'
description: 'The job the host sends to the guest over vsock'
---

## Format

The host converts the HTTP request (or the CLI call) into a job and sends it to the guest as a vsock frame (4 big-endian bytes of length + JSON).

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

## Fields

| Field | Type | Description |
|-------|------|-------------|
| `trace_id` | string | job identifier, returned in the result |
| `lang` | string | language; it must exist in the guest executor |
| `code` | string | code of the main file when there are no `files` |
| `files` | list of `{name, content}` | the job's files; the **first** one is the main file |
| `stdin` | string | standard input; omitted when empty |
| `timeout` | integer | seconds |
| `stream` | boolean | asks for the output in chunks before the result; see [Streaming](/api-reference/vsock-protocol#streaming) |
| `op` | string | operation; empty means the code job. The machine operations (`exec`, `put`, `pty`...) are in [vsock protocol](/api-reference/vsock-protocol#operations) |

## Main file

- With `files`, each file is written by name to a temporary directory under `/tmp`, and the first one is executed (or compiled).
- With only `code`, the guest writes `script.<ext>` — except Java, which becomes `Main.java`, because the executed class is `Main`.

## Rules applied before the job reaches the guest

The domain, on the host, rejects the job (HTTP 400 in the API) when:

| Rule | Limit |
|------|-------|
| language | in the catalog and with an image built on this host (otherwise E006) |
| stdin | ≤ 1,000,000 bytes (otherwise E005) |
| timeout | > 0 and ≤ 300 s; missing becomes 10 s |
| content | `code` or at least one file |
| files | up to 32, no repeated names |
| total size | sum of `code` and contents ≤ 4 MiB |
| file name | no `/`, no `..`, not starting with `.`, ≤ 255 bytes |

The name rules prevent writes outside the job directory inside the guest, even though the VM is disposable.

## Readiness

Before the first real job, the host repeatedly sends the VM an `sh true` job (every 1 ms) until it gets a response. That is how it knows the guest is already listening.
