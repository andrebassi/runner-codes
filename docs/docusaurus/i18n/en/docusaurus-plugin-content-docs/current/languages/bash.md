---
title: 'Bash'
description: 'Bash on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `bash` |
| source OCI image | `bash:5.2` |
| per-job resource ceiling | 1 vCPU, 256 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

None: the official image already ships everything needed.

## How the code is executed

The main file (the first in `files`) is executed with `bash <file>`. The available utilities are those in the official image; there is no network, so `curl`, `wget` and package managers reach nothing. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job.

## Building the image

```bash
rc image build --stack bash
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "bash",
    "files": [
      {"name": "main.sh", "content": "read nome\necho \"olá, $nome\"\n"}
    ],
    "stdin": "mundo"
  }'
```

Expected `stdout`: `olá, mundo`.
