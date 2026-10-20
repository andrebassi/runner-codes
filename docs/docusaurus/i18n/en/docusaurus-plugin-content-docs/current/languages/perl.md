---
title: 'Perl'
description: 'Perl on Runner Codes: image, resources, warmup and example'
---

## Summary

| Item | Value |
|------|-------|
| API name | `perl` |
| source OCI image | `alpine:3.22` + `perl` |
| per-job resource ceiling | 1 vCPU, 512 MiB |

The vCPU and RAM ceiling exists to prevent abuse; libkrun memory is elastic and the host only pays for what the guest touches.

## Warmup

`apk add perl`: the base image does not ship Perl. The package is installed in the build VM and baked into the ext4. See [Images](/concepts/images).

## How the code is executed

The main file (the first in `files`) is executed with `perl <file>`. With no network, CPAN modules are not installed; only what comes in Alpine's `perl` package is available. Everything runs in a directory under `/tmp` (tmpfs), in a fresh VM destroyed at the end of the job.

## Building the image

```bash
rc image build --stack perl
```

## Example

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "perl",
    "files": [
      {"name": "main.pl", "content": "my $l = <STDIN>;\nchomp $l;\nprint scalar reverse($l), \"\\n\";\n"}
    ],
    "stdin": "abc"
  }'
```

Expected `stdout`: `cba`.
