---
title: 'SQLite'
description: 'SQLite on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `sqlite` |
| source OCI image | `alpine:3.22` + `sqlite` |
| per-job resource ceiling | 1 vCPU, 256 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

`apk add sqlite`: the base image does not ship `sqlite3`. The package is installed in the build VM and baked into the ext4. See [Images](/concepts/images).

## How the code is executed

The content of the main file (the first in `files`) goes through the stdin of `sqlite3 :memory:` — an in-memory database that disappears at the end of the job. If the job has stdin, it is sent **before** the code, followed by a newline. The output uses the default `sqlite3` format (columns separated by `|`). Everything runs in a fresh VM destroyed at the end of the job, with no network.

This is the executable SQLite. The database syntax validators are not in the catalog; see [Database parsers](/guides/database-parsers-setup).

## Building the image

```bash
rc image build --stack sqlite
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "sqlite",
    "files": [
      {"name": "main.sql", "content": "CREATE TABLE t (n INTEGER);\nINSERT INTO t VALUES (40), (2);\nSELECT SUM(n) FROM t;\n"}
    ]
  }'
```

Expected `stdout`: `42`.
