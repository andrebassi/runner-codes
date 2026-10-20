---
title: 'Rust'
description: 'Runner Codes 中的 Rust：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `rust` |
| 源 OCI 镜像 | `rust:1-alpine` |
| 每个 job 的资源上限 | 2 vCPU, 1024 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

无：官方镜像已经包含所需的一切。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）通过 `rustc -o <binário> <arquivo>` 编译，再以 job 的 stdin 运行生成的二进制。编译错误返回 `exception: "Compilation failed"`，`rustc` 的输出位于 `stderr`。没有网络，外部 crate 不可用。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack rust
```

## 示例 {#exemplo}

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "rust",
    "files": [
      {"name": "main.rs", "content": "fn main() {\n    println!(\"{}\", 6 * 7);\n}\n"}
    ]
  }'
```

预期 `stdout`：`42`。
