---
title: 'C'
description: 'C on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `c` |
| source OCI image | `alpine:3.22` + `build-base` |
| per-job resource ceiling | 1 vCPU, 512 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

`apk add build-base`: the base image ships no compiler. The package is installed in the build VM and baked into the ext4; the job needs no network. See [Images](/concepts/images).

## How the code is executed

The main file (the first in `files`) is compiled with `gcc -o <binary> <file>` and the binary is run with the job's stdin. A compilation error returns `exception: "Compilation failed"`, with the `gcc` output in `stderr`. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job, with no network.

## Building the image

```bash
rc image build --stack c
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "c",
    "files": [
      {"name": "main.c", "content": "#include <stdio.h>\n\nint main(void) {\n    int n;\n    scanf(\"%d\", &n);\n    printf(\"%d\\n\", n * 2);\n    return 0;\n}\n"}
    ],
    "stdin": "21"
  }'
```

Expected `stdout`: `42`.
