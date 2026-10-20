---
title: '语言'
description: '语言目录、各语言的资源配置，以及如何新增一门语言'
---

## 目录 {#catálogo}

目录来自嵌入在二进制中的 `languages.yaml`（`internal/adapters/catalog/languages.yaml`）。共 15 门语言（另有下文的 `llama` stack）：

| 语言 | API 中的名称 | 源 OCI 镜像 | vCPU | RAM | Warmup |
|-----------|-------------|----------------------|------|-----|--------|
| [Python](/languages/python) | `python` | `python:3.12-alpine` | 1 | 512 MiB | 对 stdlib 执行 `compileall` |
| [Node.js](/languages/nodejs) | `nodejs` | `node:22-alpine` | 1 | 512 MiB | — |
| [Go](/languages/go) | `go` | `golang:1.25-alpine` | 2 | 1024 MiB | 填充 `GOCACHE` |
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

`rc languages`（或 `GET /api/v1/languages`）列出同样的列表，并带有 `ready` 字段，表示该语言的镜像是否已在本机构建。镜像尚未构建的语言会以 E006 被拒绝。

## 资源 {#recursos}

vCPU 和 RAM 是**防止滥用的上限**，而不是预留。libkrun 的内存是弹性的：host 只为 guest 实际触及的页面买单，实测一台空闲 VM 的内存占用为 44 MB，与上限无关。空闲的 vCPU 在 hypervisor 中休眠。

## 代码如何运行 {#como-o-código-roda}

| 类别 | 语言 | 执行方式 |
|---------|------------|----------|
| 解释型 | Python, Node.js, Ruby, PHP, Perl, Bash, sh, AWK, jq | `<interpretador> <arquivo principal>` |
| 编译型 | C, C++, Rust | 编译成二进制后执行该二进制 |
| JVM | Java | `javac` 然后 `java Main` |
| Go | Go | `go run` |
| SQL | SQLite | 代码通过 stdin 传给 `sqlite3 :memory:` |

所有语言的共同点：`files` 的第一项是主文件，工作目录位于 `/tmp` 的 tmpfs 中，VM 是全新的、结束时销毁，并且没有网络。

## 新增一门语言 {#adicionar-uma-linguagem}

需要同时满足两个条件：

1. **该语言存在于 guest 执行器中**（`internal/guest/executor.go`），带有扩展名、命令，如果是编译型语言，还要有构建命令。执行器支持的语言比目录公开的更多；如果你要的语言不在其中，需要先把它加到执行器里（并重新编译 guest）。
2. **在 `languages.yaml` 中新增一条记录**：

```yaml
  - name: lua
    image: <imagem OCI oficial>
    cpus: 1
    memory_mib: 256
    warmup: ""
```

| 字段 | 规则 |
|-------|-------|
| `name` | 与 guest 执行器中的条目名称完全一致 |
| `image` | 公开的 OCI 镜像，架构与 host 一致；构建过程无需 Docker 即可拉取 |
| `cpus`, `memory_mib` | 防止滥用的上限 |
| `warmup` | 一行 `sh`，在构建 VM 中以 chroot 方式运行，有网络；结果写入镜像。warmup 的任何部分都不会在 job 的 VM 中运行 |

然后：

```bash
task build
task image:build -- --stack <nome>
rc run --stack <nome> --code '<programa mínimo>'
```

执行器所用的可执行文件必须位于镜像的 `PATH` 中，或位于执行器使用的绝对路径下。基于 Alpine（musl）的镜像是经过测试的情形；guest 是静态链接的，在任何镜像中都能运行。

## `llama` stack（GPU） {#a-stack-llama-gpu}

除了 15 门语言之外，目录中还有 `llama` stack（`fedora:43`，4 vCPU，4096 MiB，`gpu: true`）：使用 Vulkan
后端的 llama.cpp，以及写入镜像的 Qwen2.5 0.5B 模型。它为 `--gpu` 而存在；参见 [GPU](/advanced/gpu)。

## 目录之外 {#fora-do-catálogo}

- **使用任意镜像的机器：** `rc machine create --image <ref>`（或 [runner.code](/cli/runner-code) 中的
  `image = "..."`）会构建并启动一个不在目录中的 OCI 镜像。它供机器使用，而不是供 API 的 job 使用。
- **数据库解析器：** guest 执行器仍保留了 SQL 和 NoSQL 语法校验器的条目，但它们在目录中
  没有镜像。参见[数据库解析器](/guides/database-parsers-setup)。
