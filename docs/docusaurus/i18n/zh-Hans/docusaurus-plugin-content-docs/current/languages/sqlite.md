---
title: 'SQLite'
description: 'Runner Codes 中的 SQLite：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `sqlite` |
| 源 OCI 镜像 | `alpine:3.22` + `sqlite` |
| 每个 job 的资源上限 | 1 vCPU, 256 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

`apk add sqlite`：基础镜像不带 `sqlite3`。该软件包在构建 VM 中安装并写入 ext4。参见[镜像](/concepts/images)。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）的内容通过 stdin 传给 `sqlite3 :memory:`——一个内存数据库，job 结束即消失。如果 job 带有 stdin，它会在代码**之前**发送，后跟一个换行符。输出使用 `sqlite3` 的默认格式（列之间以 `|` 分隔）。一切都在一台全新的 VM 中运行，job 结束即销毁，没有网络。

这里是可执行的 SQLite。数据库语法校验器不在目录中；参见[数据库解析器](/guides/database-parsers-setup)。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack sqlite
```

## 示例 {#exemplo}

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

预期 `stdout`：`42`。
