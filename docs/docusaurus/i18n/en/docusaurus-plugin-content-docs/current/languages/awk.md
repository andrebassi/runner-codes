---
title: 'AWK'
description: 'AWK on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `awk` |
| source OCI image | `alpine:3.22` |
| per-job resource ceiling | 1 vCPU, 256 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

None: the base image already ships `awk`.

## How the code is executed

The main file (the first in `files`) is the AWK program, executed with `awk -f <file>`. The job's stdin is the input the program processes, line by line. The `awk` is the one from the Alpine image (BusyBox). Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job, with no network.

## Building the image

```bash
rc image build --stack awk
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "awk",
    "files": [
      {"name": "main.awk", "content": "{ soma += $1 } END { print soma }\n"}
    ],
    "stdin": "10\n20\n12\n"
  }'
```

Expected `stdout`: `42`.
