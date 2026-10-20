---
title: '镜像'
description: '官方 OCI 镜像如何变成只读 ext4：构建、缓存与 warmup'
---

## 这里的镜像是什么 {#o-que-é-uma-imagem-aqui}

目录中的每种语言都运行在一个只读 ext4 文件之上，它就是 VM 的 root。它由该语言的**官方** OCI 镜像（例如 `python:3.12-alpine`）生成，并额外加入三样东西：

- 位于 `/opt/runner/guest` 的静态 guest；
- `/opt/runner/start.sh`，VM 的第一个进程；
- 该语言 warmup 的产物（如果有）。

## 构建流程 {#como-o-build-funciona}

```bash
rc image build --stack python
rc image build --all
```

1. **不依赖 Docker 的 pull。** host 用 go-containerregistry 下载与 host 架构对应的镜像。
2. **在 host 上扁平化。** 各层合并成一个 tar。
3. **构建 VM。** 一台 Alpine 3.22.6 VM（Alpine 官方基础 tarball，已校验 sha256）接收这个 tar，并通过 `apk` 安装 e2fsprogs 和 GNU tar。在其中以 root 身份：
   - 把 tar 解压到 tmpfs，保留属主和权限（`--numeric-owner`）；
   - 把 guest 和 `start.sh` 复制到 `/opt/runner/`；
   - 在 chroot 中运行 warmup，联网，仅限这一步；
   - 用 `mkfs.ext4 -d` 生成 ext4。
4. ext4 进入缓存，符号链接 `<lang>.ext4` 指向它。

### 为什么需要构建 VM {#por-que-uma-vm-de-build}

- macOS 的 APFS 不区分大小写：`Case` 和 `case` 会变成同一个文件。VM 内是 Linux 文件系统，区分大小写。
- Homebrew 的 e2fsprogs 不能读取 tar，所以无法在 Mac 上直接从 tar 生成 ext4。
- 以 root 解压能保留镜像中的 uid、gid 和权限。

## 缓存 {#cache}

镜像的键是：

```
sha256(digest da imagem + warmup + build.sh + start.sh + guest)[:12]
```

| 系统 | 路径 |
|---------|---------|
| macOS | `~/Library/Caches/runner-codes/images/<lang>-<chave>.ext4` |
| Linux | `~/.cache/runner-codes/images/<lang>-<chave>.ext4` |

同一目录中有符号链接 `<lang>.ext4`。缓存根目录可通过 `RUNNER_CACHE_DIR` 修改。

结果：修改 guest、`start.sh`、构建脚本、warmup 或源镜像都会生成新键，并在下一次执行 `image build` 时强制重建。无需手动使任何东西失效。如果现有镜像是用旧 guest 构建的，只需重新构建即可。

## Warmup {#warmup}

warmup 是在 `languages.yaml` 中声明的一行 `sh`。它**在构建 VM 中**联网运行，产物写入镜像。warmup 的任何部分都不会在作业 VM 中运行。

| 语言 | Warmup | 原因 |
|-----------|--------|---------|
| python | 对标准库执行 `python3 -m compileall` | 官方镜像不带 `.pyc`；预编译去掉了首次 import 的开销。镜像从 66 MiB 增至 85 MiB，warmup 耗时约 1 s |
| go | 用 `GOCACHE=/opt/runner/gocache` 编译一个程序 | 填充构建缓存；由于 root 只读，`start.sh` 在启动时把缓存复制到 `/tmp/go-cache`（执行器强制使用的路径），这发生在 VM 进入 pool 之前 |
| c、cpp | `apk add build-base` | 基础镜像是 `alpine:3.22`，没有编译器 |
| perl | `apk add perl` | 同上 |
| jq | `apk add jq` | 同上 |
| sqlite | `apk add sqlite` | 同上 |

其余语言没有 warmup。

## 目录之外的镜像 {#imagens-fora-do-catálogo}

machine 可以使用任意 OCI 镜像（`rc machine create --image alpine:3.20`，或在
[runner.code](/cli/runner-code) 中写 `image = "..."`）。构建流程相同，warmup 为空，镜像像其他镜像一样进入缓存。解包后的
[pack](/cli/pack) 会成为镜像 `pack:<chave>`。

## 清理缓存 {#limpar-o-cache}

```bash
rc image prune          # 旧的构建产物，已没有任何名字指向它们
rc image prune --all    # 外加没有任何 machine 使用的镜像和 pack
```

## 实测耗时 {#tempos-medidos}

在测量用的机器上（macOS 27.0.1，Apple Silicon），Python 镜像构建耗时约 8–10 s，其中 pull 和扁平化占 2,7 s。在大镜像上
`mkfs.ext4` 占主导：Go 的 35 s 中占 28 s，因为 ext4 是通过 virtiofs 在 host 上写入的。各 stack 的数据见[测试报告](/advanced/test-report)。
