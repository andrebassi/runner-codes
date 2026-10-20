---
title: 'jq'
description: 'jq on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `jq` |
| source OCI image | `alpine:3.22` + `jq` |
| per-job resource ceiling | 1 vCPU, 256 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

`apk add jq`: the base image does not ship `jq`. The package is installed in the build VM and baked into the ext4. See [Images](/concepts/images).

## How the code is executed

The main file (the first in `files`) is the filter, executed with `jq --null-input -f <file>`. With `--null-input`, the filter's initial input is `null`; to read the JSON from the job's stdin, use `input` (one value) or `inputs` (all of them). Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job, with no network.

## Building the image

```bash
rc image build --stack jq
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "jq",
    "files": [
      {"name": "main.jq", "content": "input | .a + .b\n"}
    ],
    "stdin": "{\"a\": 40, \"b\": 2}"
  }'
```

Expected `stdout`: `42`.
