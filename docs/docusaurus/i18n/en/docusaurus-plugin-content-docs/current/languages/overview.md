---
title: 'Languages'
description: 'The language catalog, per-language resources, and how to add a new one'
---

## Catalog

The catalog comes from `languages.yaml`, embedded in the binary (`internal/adapters/catalog/languages.yaml`). It lists 15 languages (plus the `llama` stack, below):

| Language | API name | Source OCI image | vCPU | RAM | Warmup |
|-----------|-------------|----------------------|------|-----|--------|
| [Python](/languages/python) | `python` | `python:3.12-alpine` | 1 | 512 MiB | stdlib `compileall` |
| [Node.js](/languages/nodejs) | `nodejs` | `node:22-alpine` | 1 | 512 MiB | — |
| [Go](/languages/go) | `go` | `golang:1.25-alpine` | 2 | 1024 MiB | `GOCACHE` populated |
| [Rust](/languages/rust) | `rust` | `rust:1-alpine` | 2 | 1024 MiB | — |
| [Java](/languages/java) | `java` | `eclipse-temurin:21-jdk-alpine` | 2 | 1024 MiB | — |
| [C](/languages/c) | `c` | `alpine:3.22` | 1 | 512 MiB | `apk add build-base` |
| [C++](/languages/cpp) | `cpp` | `alpine:3.22` | 1 | 512 MiB | `apk add build-base` |
| [Ruby](/languages/ruby) | `ruby` | `ruby:3.3-alpine` | 1 | 512 MiB | — |
| [PHP](/languages/php) | `php` | `php:8.3-cli-alpine` | 1 | 512 MiB | — |
| [Perl](/languages/perl) | `perl` | `alpine:3.22` | 1 | 512 MiB | `apk add perl` |
| [Bash](/languages/bash) | `bash` | `bash:5.2` | 1 | 256 MiB | — |
| [sh](/languages/sh) | `sh` | `alpine:3.22` | 1 | 256 MiB | — |
| [AWK](/languages/awk) | `awk` | `alpine:3.22` | 1 | 256 MiB | — |
| [jq](/languages/jq) | `jq` | `alpine:3.22` | 1 | 256 MiB | `apk add jq` |
| [SQLite](/languages/sqlite) | `sqlite` | `alpine:3.22` | 1 | 256 MiB | `apk add sqlite` |

`rc languages` (or `GET /api/v1/languages`) shows the same list with `ready`, which tells whether the image has already been built on this host. A language without a built image is rejected with E006.

## Resources

vCPU and RAM are an **abuse ceiling**, not a reservation. libkrun memory is elastic: the host only pays for the pages the guest touches, and an idle VM measured a 44 MB footprint regardless of the ceiling. Idle vCPUs sleep in the hypervisor.

## How code runs

| Family | Languages | Execution |
|---------|------------|----------|
| interpreted | Python, Node.js, Ruby, PHP, Perl, Bash, sh, AWK, jq | `<interpreter> <main file>` |
| compiled | C, C++, Rust | compiles to a binary and runs the binary |
| JVM | Java | `javac` and `java Main` |
| Go | Go | `go run` |
| SQL | SQLite | the code goes through the stdin of `sqlite3 :memory:` |

In every case: the first entry in `files` is the main file, the working directory lives on the `/tmp` tmpfs, the VM is fresh and destroyed at the end, and there is no network.

## Adding a language

There are two conditions, and both must hold:

1. **The language exists in the guest executor** (`internal/guest/executor.go`), with an extension, a command and, if it is compiled, the build command. The executor has entries for more languages than the catalog publishes; if the one you want is not there, it has to be added to the executor (and the guest rebuilt).
2. **A new entry in `languages.yaml`**:

```yaml
  - name: lua
    image: <imagem OCI oficial>
    cpus: 1
    memory_mib: 256
    warmup: ""
```

| Field | Rule |
|-------|-------|
| `name` | exactly the name of the entry in the guest executor |
| `image` | a public OCI image for the host architecture; the build pulls it without Docker |
| `cpus`, `memory_mib` | abuse ceiling |
| `warmup` | one line of `sh`, run in a chroot inside the build VM, with network; the result is baked into the image. Nothing from the warmup runs in the job VM |

Then:

```bash
task build
task image:build -- --stack <nome>
rc run --stack <nome> --code '<programa mínimo>'
```

The executor's binary must be on the image's `PATH` or at the absolute path the executor uses. An Alpine-based (musl) image is the tested case; the guest is static and runs on any of them.

## The `llama` stack (GPU)

Besides the 15 languages, the catalog ships the `llama` stack (`fedora:43`, 4 vCPU, 4096 MiB, `gpu: true`): llama.cpp with
the Vulkan backend and a Qwen2.5 0.5B model baked into the image. It exists for `--gpu`; see [GPU](/advanced/gpu).

## Outside the catalog

- **Machines with any image:** `rc machine create --image <ref>` (or `image = "..."` in the
  [runner.code](/cli/runner-code)) builds and boots an OCI image that is not in the catalog. It serves machines, not
  API jobs.
- **Database parsers:** the guest executor still has entries for SQL and NoSQL syntax validators, but they have
  no image in the catalog. See [Database parsers](/guides/database-parsers-setup).
