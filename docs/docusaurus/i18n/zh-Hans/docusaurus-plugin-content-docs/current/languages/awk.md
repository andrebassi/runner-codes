---
title: 'AWK'
description: 'Runner Codes 中的 AWK：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `awk` |
| 源 OCI 镜像 | `alpine:3.22` |
| 每个 job 的资源上限 | 1 vCPU, 256 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

无：基础镜像已自带 `awk`。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）就是 AWK 程序，通过 `awk -f <arquivo>` 执行。job 的 stdin 是程序逐行处理的输入。`awk` 来自 Alpine 镜像（BusyBox）。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁，没有网络。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack awk
```

## 示例 {#exemplo}

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "awk",
    "files": [
      {"name": "main.awk", "content": "{ soma += $1 } END { print soma }\n"}
    ],
    "stdin": "10\n20\n12\n"
  }'
```

预期 `stdout`：`42`。
