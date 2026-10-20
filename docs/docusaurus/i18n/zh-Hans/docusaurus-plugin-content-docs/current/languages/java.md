---
title: 'Java'
description: 'Runner Codes 中的 Java：镜像、资源、warmup 与示例'
---

## 概要 {#resumo}

| 项目 | 值 |
|------|-------|
| API 中的名称 | `java` |
| 源 OCI 镜像 | `eclipse-temurin:21-jdk-alpine` |
| 每个 job 的资源上限 | 2 vCPU, 1024 MiB |

vCPU 和 RAM 的上限是为了防止滥用；libkrun 的内存是弹性的，host 只为 guest 实际触及的内存买单。

## Warmup

无：官方镜像已经包含所需的一切。

## 代码如何执行 {#como-o-código-é-executado}

主文件（`files` 中的第一个）通过 `javac` 编译，然后用 `java Main` 运行 `Main` 类。因此：

- 含有 `main` 的类必须命名为 `Main`；
- 文件必须命名为 `Main.java`。

在 `/api/v1/execute` 中直接传入 `code` 时，guest 会自动写入 `Main.java`。编译错误返回 `exception: "Compilation failed"`，`javac` 的输出位于 `stderr`。一切都在 `/tmp`（tmpfs）下的目录中运行，使用一台全新的 VM，job 结束即销毁，没有网络。

## 构建镜像 {#construir-a-imagem}

```bash
rc image build --stack java
```

## 示例 {#exemplo}

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "java",
    "files": [
      {"name": "Main.java", "content": "public class Main {\n    public static void main(String[] args) {\n        System.out.println(6 * 7);\n    }\n}\n"}
    ]
  }'
```

预期 `stdout`：`42`。
