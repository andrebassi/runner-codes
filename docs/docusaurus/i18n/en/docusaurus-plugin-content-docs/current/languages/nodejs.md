---
title: 'Node.js'
description: 'Node.js on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `nodejs` |
| source OCI image | `node:22-alpine` |
| per-job resource ceiling | 1 vCPU, 512 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

None: the official image already ships everything needed.

## How the code is executed

The main file (the first in `files`) is executed with `node <file>`. The other files sit in the same directory and can be loaded with `require`. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job, with no network — `npm install` does not work inside the job.

## Building the image

```bash
rc image build --stack nodejs
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "nodejs",
    "files": [
      {"name": "main.js", "content": "let d = \"\";\nprocess.stdin.on(\"data\", c => d += c);\nprocess.stdin.on(\"end\", () => console.log(d.trim().toUpperCase()));\n"}
    ],
    "stdin": "abc"
  }'
```

Expected `stdout`: `ABC`.
