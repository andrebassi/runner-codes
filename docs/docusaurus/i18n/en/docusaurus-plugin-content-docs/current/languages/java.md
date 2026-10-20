---
title: 'Java'
description: 'Java on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `java` |
| source OCI image | `eclipse-temurin:21-jdk-alpine` |
| per-job resource ceiling | 2 vCPU, 1024 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

None: the official image already ships everything needed.

## How the code is executed

The main file (the first in `files`) is compiled with `javac` and the `Main` class is run with `java Main`. Therefore:

- the class with `main` must be named `Main`;
- the file must be named `Main.java`.

With bare `code` in `/api/v1/execute`, the guest writes `Main.java` on its own. A compilation error returns `exception: "Compilation failed"`, with the `javac` output in `stderr`. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job, with no network.

## Building the image

```bash
rc image build --stack java
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "java",
    "files": [
      {"name": "Main.java", "content": "public class Main {\n    public static void main(String[] args) {\n        System.out.println(6 * 7);\n    }\n}\n"}
    ]
  }'
```

Expected `stdout`: `42`.
