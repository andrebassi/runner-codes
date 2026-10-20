---
title: 'Node.js'
description: 'Runner Codes 中的 Node.js：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `nodejs` |
| 源 OCI 镜像 | `node:22-alpine` |
| 每个 job 的资源上限 | 1 vCPU, 512 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

无：官方镜像已经包含所需的一切。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）通过 `node <arquivo>` 执行。其余文件位于同一目录，可以用 `require` 加载。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁，没有网络——在 job 内 `npm install` 无法工作。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack nodejs
```

## 示例 {#exemplo}

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "nodejs",
    "files": [
      {"name": "main.js", "content": "let d = \"\";\nprocess.stdin.on(\"data\", c => d += c);\nprocess.stdin.on(\"end\", () => console.log(d.trim().toUpperCase()));\n"}
    ],
    "stdin": "abc"
  }'
```

预期 `stdout`：`ABC`。
