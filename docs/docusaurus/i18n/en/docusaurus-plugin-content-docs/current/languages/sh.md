---
title: 'sh'
description: 'POSIX shell (sh) on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `sh` |
| source OCI image | `alpine:3.22` |
| per-job resource ceiling | 1 vCPU, 256 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

None: the base image already ships `sh`.

## How the code is executed

The main file (the first in `files`) is executed with `sh <file>` — the Alpine image's shell (BusyBox). For Bash-specific features (arrays, `[[ ]]`), use the [`bash`](/languages/bash) language. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job, with no network.

## Building the image

```bash
rc image build --stack sh
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "sh",
    "files": [
      {"name": "main.sh", "content": "read n\necho $((n * 2))\n"}
    ],
    "stdin": "21"
  }'
```

Expected `stdout`: `42`.
