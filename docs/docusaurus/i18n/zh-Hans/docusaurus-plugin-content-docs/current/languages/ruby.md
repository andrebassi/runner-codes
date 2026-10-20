---
title: 'Ruby'
description: 'Runner Codes 中的 Ruby：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `ruby` |
| 源 OCI 镜像 | `ruby:3.3-alpine` |
| 每个 job 的资源上限 | 1 vCPU, 512 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

无：官方镜像已经包含所需的一切。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）通过 `ruby <arquivo>` 执行。其余文件位于同一目录，可以用 `require_relative` 加载。没有网络，因此无法安装外部 gem。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack ruby
```

## 示例 {#exemplo}

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

预期 `stdout`：`cba`。
