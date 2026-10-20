---
title: 'PHP'
description: 'Runner Codes 中的 PHP：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `php` |
| 源 OCI 镜像 | `php:8.3-cli-alpine` |
| 每个 job 的资源上限 | 1 vCPU, 512 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

无：官方镜像已经包含所需的一切。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）通过 `php <arquivo>`（CLI）执行。其余文件位于同一目录，可以用 `require` 引入。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁，没有网络。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack php
```

## 示例 {#exemplo}

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "php",
    "files": [
      {"name": "main.php", "content": "<?php\necho strtoupper(trim(fgets(STDIN))), PHP_EOL;\n"}
    ],
    "stdin": "abc"
  }'
```

预期 `stdout`：`ABC`。
