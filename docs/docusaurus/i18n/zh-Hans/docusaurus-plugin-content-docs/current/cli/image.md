---
title: 'image'
description: '从官方 OCI 镜像为一门语言构建只读 ext4'
---

## 用法 {#uso}

```bash
rc image build --stack python
rc image build --stack python --stack go
rc image build --stack python,go
rc image build --all
```

| Flag | 说明 |
|------|-----------|
| `--stack` | 目录中的语言；可重复，也可用逗号分隔 |
| `--all` | 目录中的所有语言 |

既没有 `--stack` 也没有 `--all` 时，命令以 `informe --stack ou --all` 失败。不在目录中的语言以 E006 失败。

通过 Taskfile（会先编译）：

```bash
task image:build -- --stack python
task image:build -- --all
```

## image prune

```bash
rc image prune          # 清理不再被任何名称指向的旧构建（guest 或 start.sh 变了，键也就变了）
rc image prune --all    # 同时清理没有任何机器使用的镜像和 pack
```

目录之外的镜像（`rc machine create --image ...`）使用同样的构建流程，但没有 warmup。

## 输出 {#saída}

每门语言一行：

```
python <caminho do ext4> cached=false elapsed=<tempo> <métricas do build>
```

- 当缓存中已有该键、没有任何重建时，显示 `cached=true`。
- 指标来自构建 VM：解压、warmup 和 `mkfs` 的耗时，以及已用空间和磁盘大小。

## 构建做了什么 {#o-que-o-build-faz}

1. 用 go-containerregistry（不使用 Docker）拉取 OCI 镜像，并把各层展平为一个 tar。
2. 启动一台 Alpine 3.22.6 构建 VM（Alpine 官方基础 tarball，校验 sha256；e2fsprogs 和 GNU tar 通过 `apk` 安装）。
3. 在其中以 root 身份：把 tar 解压到 tmpfs，把 guest 和 `start.sh` 注入 `/opt/runner/`，在 chroot 中运行 warmup，并用 `mkfs.ext4 -d` 生成 ext4。
4. 把 `<lang>-<chave>.ext4` 写入缓存，并让符号链接 `<lang>.ext4` 指向它。

每一步的原因、缓存键以及每门语言的 warmup，见[镜像](/concepts/images)。

## 何时重建 {#quando-reconstruir}

缓存键包含镜像 digest、warmup、`build.sh`、`start.sh` 和 guest。修改其中任何一项之后——包括用不同的代码重新编译 guest——重新运行构建即可：键会自动变化，并生成一个新镜像。

## 耗时 {#tempo}

在测量所用的机器上（macOS 27.0.1，Apple Silicon），Python 镜像耗时约 8–10 s，其中拉取和展平占 2.7 s。Go 镜像
耗时 35 s，其中 28 s 花在 `mkfs.ext4` 上。构建需要网络（拉取和 warmup）；job 的 VM 没有网络。
