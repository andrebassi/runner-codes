---
title: 'jq'
description: 'Runner Codes 中的 jq：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `jq` |
| 源 OCI 镜像 | `alpine:3.22` + `jq` |
| 每个 job 的资源上限 | 1 vCPU, 256 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

`apk add jq`：基础镜像不带 `jq`。该软件包在构建 VM 中安装并写入 ext4。参见[镜像](/concepts/images)。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）就是过滤器，通过 `jq --null-input -f <arquivo>` 执行。使用 `--null-input` 时，过滤器的初始输入是 `null`；要读取 job stdin 中的 JSON，请使用 `input`（一个值）或 `inputs`（全部值）。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁，没有网络。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack jq
```

## 示例 {#exemplo}

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

预期 `stdout`：`42`。
