---
title: 'Bash'
description: 'Runner Codes 中的 Bash：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `bash` |
| 源 OCI 镜像 | `bash:5.2` |
| 每个 job 的资源上限 | 1 vCPU, 256 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

无：官方镜像已经包含所需的一切。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）通过 `bash <arquivo>` 执行。可用的工具就是官方镜像自带的那些；没有网络，所以 `curl`、`wget` 和包管理器都访问不到任何地方。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack bash
```

## 示例 {#exemplo}

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "bash",
    "files": [
      {"name": "main.sh", "content": "read nome\necho \"olá, $nome\"\n"}
    ],
    "stdin": "mundo"
  }'
```

预期 `stdout`：`olá, mundo`。
