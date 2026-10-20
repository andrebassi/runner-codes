---
title: 'Go'
description: 'Go on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `go` |
| source OCI image | `golang:1.25-alpine` |
| per-job resource ceiling | 2 vCPU, 1024 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

The build compiles a program with `GOCACHE=/opt/runner/gocache`, which populates the standard library build cache inside the image. Since the root is read-only, `start.sh` copies that cache to `/tmp/go-cache` at boot, the path the guest executor forces into `GOCACHE`. The copy happens before the VM enters the pool and does not weigh on the job. Measured: `go run` in 247 ms with the warm cache, versus 3.4 s without it.

See [Images](/concepts/images).

## How the code is executed

The main file (the first in `files`) is executed with `go run <file>`. With no network, only the standard library is available: external modules are not downloaded. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job.

## Building the image

```bash
rc image build --stack go
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "go",
    "files": [
      {"name": "main.go", "content": "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(6 * 7)\n}\n"}
    ]
  }'
```

Expected `stdout`: `42`.
