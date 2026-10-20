---
title: 'C'
description: 'Runner Codes 中的 C：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `c` |
| 源 OCI 镜像 | `alpine:3.22` + `build-base` |
| 每个 job 的资源上限 | 1 vCPU, 512 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

`apk add build-base`：基础镜像不带编译器。该软件包在构建 VM 中安装并写入 ext4；job 本身不需要网络。参见[镜像](/concepts/images)。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）通过 `gcc -o <binário> <arquivo>` 编译，再以 job 的 stdin 运行生成的二进制。编译错误返回 `exception: "Compilation failed"`，`gcc` 的输出位于 `stderr`。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁，没有网络。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack c
```

## 示例 {#exemplo}

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "c",
    "files": [
      {"name": "main.c", "content": "#include <stdio.h>\n\nint main(void) {\n    int n;\n    scanf(\"%d\", &n);\n    printf(\"%d\\n\", n * 2);\n    return 0;\n}\n"}
    ],
    "stdin": "21"
  }'
```

预期 `stdout`：`42`。
