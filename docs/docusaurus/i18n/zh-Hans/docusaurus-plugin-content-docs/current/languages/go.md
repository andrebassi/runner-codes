---
title: 'Go'
description: 'Runner Codes 中的 Go：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `go` |
| 源 OCI 镜像 | `golang:1.25-alpine` |
| 每个 job 的资源上限 | 2 vCPU, 1024 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

构建时用 `GOCACHE=/opt/runner/gocache` 编译一个程序，从而在镜像内填充标准库的构建缓存。由于 root 是只读的，`start.sh` 在启动时把这份缓存复制到 `/tmp/go-cache`，也就是 guest 执行器强制设给 `GOCACHE` 的路径。复制发生在 VM 进入 pool 之前，不计入 job 耗时。实测：缓存预热后 `go run` 为 247 ms，没有缓存时为 3.4 s。

参见[镜像](/concepts/images)。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）通过 `go run <arquivo>` 执行。没有网络，只能使用标准库：外部模块不会被下载。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack go
```

## 示例 {#exemplo}

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "go",
    "files": [
      {"name": "main.go", "content": "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(6 * 7)\n}\n"}
    ]
  }'
```

预期 `stdout`：`42`。
