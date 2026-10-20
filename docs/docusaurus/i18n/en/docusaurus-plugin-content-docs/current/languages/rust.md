---
title: 'Rust'
description: 'Rust on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `rust` |
| source OCI image | `rust:1-alpine` |
| per-job resource ceiling | 2 vCPU, 1024 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

None: the official image already ships everything needed.

## How the code is executed

The main file (the first in `files`) is compiled with `rustc -o <binary> <file>` and the binary is run with the job's stdin. A compilation error returns `exception: "Compilation failed"`, with the `rustc` output in `stderr`. With no network, external crates are not available. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job.

## Building the image

```bash
rc image build --stack rust
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "rust",
    "files": [
      {"name": "main.rs", "content": "fn main() {\n    println!(\"{}\", 6 * 7);\n}\n"}
    ]
  }'
```

Expected `stdout`: `42`.
