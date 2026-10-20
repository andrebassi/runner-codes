---
title: 'Python'
description: 'Runner Codes 中的 Python：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `python` |
| 源 OCI 镜像 | `python:3.12-alpine` |
| 每个 job 的资源上限 | 1 vCPU, 512 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

对 stdlib 执行 `python3 -m compileall`。官方镜像不带 `.pyc`；在镜像中预先编译，可以省掉第一次 import 的开销。镜像从 66 MiB 增至 85 MiB，warmup 耗时约 1 s。

warmup 只在构建 VM 中运行一次，结果写入 ext4。参见[镜像](/concepts/images)。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）通过 `python3 <arquivo>` 执行。其余文件位于同一目录，可以被 import。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁，没有网络。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack python
```

## 示例 {#exemplo}

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "python",
    "files": [
      {"name": "main.py", "content": "from util import dobro\nprint(dobro(int(input())))\n"},
      {"name": "util.py", "content": "def dobro(x):\n    return 2 * x\n"}
    ],
    "stdin": "21"
  }'
```

预期 `stdout`：`42`。

## 实测数据 {#números-medidos}

在 macOS 27.0.1、Apple Silicon 上：从启动到第 1 次响应 p50 为 260 ms，p95 为 266 ms；通过 CLI 冷启动运行 job，总计约 240 ms（执行 31–38 ms）；通过 API 且 pool 已满时为 26–43 ms。镜像构建约 8–10 s。参见[性能](/advanced/performance)。
