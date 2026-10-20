---
title: 'Ruby'
description: 'Ruby on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `ruby` |
| source OCI image | `ruby:3.3-alpine` |
| per-job resource ceiling | 1 vCPU, 512 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

None: the official image already ships everything needed.

## How the code is executed

The main file (the first in `files`) is executed with `ruby <file>`. The other files sit in the same directory and can be loaded with `require_relative`. With no network, external gems are not installed. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job.

## Building the image

```bash
rc image build --stack ruby
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "ruby",
    "files": [
      {"name": "main.rb", "content": "puts gets.strip.reverse\n"}
    ],
    "stdin": "abc"
  }'
```

Expected `stdout`: `cba`.
