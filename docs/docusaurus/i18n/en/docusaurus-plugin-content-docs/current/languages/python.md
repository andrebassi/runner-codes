---
title: 'Python'
description: 'Python on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `python` |
| source OCI image | `python:3.12-alpine` |
| per-job resource ceiling | 1 vCPU, 512 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

`python3 -m compileall` over the stdlib. The official image ships without `.pyc` files; compiling them into the image removes that cost from the first import. The image grew from 66 to 85 MiB and the warmup took ~1 s.

The warmup runs once, in the build VM, and the result is baked into the ext4. See [Images](/concepts/images).

## How the code is executed

The main file (the first in `files`) is executed with `python3 <file>`. The other files sit in the same directory and can be imported. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job, with no network.

## Building the image

```bash
rc image build --stack python
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "python",
    "files": [
      {"name": "main.py", "content": "from util import dobro\nprint(dobro(int(input())))\n"},
      {"name": "util.py", "content": "def dobro(x):\n    return 2 * x\n"}
    ],
    "stdin": "21"
  }'
```

Expected `stdout`: `42`.

## Measured numbers

On macOS 27.0.1, Apple Silicon: boot to the first response at p50 260 ms and p95 266 ms; a CLI job with a cold boot in ~240 ms total (execution 31–38 ms); through the API with a full pool, 26–43 ms. Image build: ~8–10 s. See [Performance](/advanced/performance).
