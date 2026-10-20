---
title: 'PHP'
description: 'PHP on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `php` |
| source OCI image | `php:8.3-cli-alpine` |
| per-job resource ceiling | 1 vCPU, 512 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

None: the official image already ships everything needed.

## How the code is executed

The main file (the first in `files`) is executed with `php <file>` (CLI). The other files sit in the same directory and can be included with `require`. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job, with no network.

## Building the image

```bash
rc image build --stack php
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "php",
    "files": [
      {"name": "main.php", "content": "<?php\necho strtoupper(trim(fgets(STDIN))), PHP_EOL;\n"}
    ],
    "stdin": "abc"
  }'
```

Expected `stdout`: `ABC`.
