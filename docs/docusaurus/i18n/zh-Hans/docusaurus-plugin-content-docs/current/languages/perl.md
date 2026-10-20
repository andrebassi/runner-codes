---
title: 'Perl'
description: 'Runner Codes 中的 Perl：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `perl` |
| 源 OCI 镜像 | `alpine:3.22` + `perl` |
| 每个 job 的资源上限 | 1 vCPU, 512 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

`apk add perl`：基础镜像不带 Perl。该软件包在构建 VM 中安装并写入 ext4。参见[镜像](/concepts/images)。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）通过 `perl <arquivo>` 执行。没有网络，因此无法安装 CPAN 模块；只能使用 Alpine 的 `perl` 软件包自带的内容。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack perl
```

## 示例 {#exemplo}

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

预期 `stdout`：`cba`。
