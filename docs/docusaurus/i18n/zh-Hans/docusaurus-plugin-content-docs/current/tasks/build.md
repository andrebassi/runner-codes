---
title: '构建'
description: '编译 host 和 guest、macOS 上的签名以及镜像构建'
---

## task build {#task-build}

```bash
task build
```

运行 `scripts/build.sh`，它会：

1. 以 `CGO_ENABLED=0` 为当前系统编译 host `bin/rc`（libkrun 在运行时通过 purego 加载）；
2. 从 `./cmd/runner-guest` 编译 guest `bin/rc-guest-linux-<arch>`：始终是 Linux、静态链接、与 host 同架构——libkrun 不模拟其他架构；
3. 如果 guest 是动态链接的就**判定失败**，因为动态二进制无法在 musl 镜像中运行（`not found`）；
4. 在 macOS 上，用 `codesign` 和 entitlement `com.apple.security.hypervisor`（`build/hv.entitlements`）为 host 签名。没有它，`krun_start_enter` 会失败。

写入二进制的版本来自 `VERSION` 或 `git describe`；没有 git 时为 `dev`。`GET /health` 中显示的就是它。

### 为什么是两个二进制 {#por-que-dois-binários}

purego 在 Linux 上强制对 glibc 动态链接。各语言镜像是 Alpine（musl）。因此 guest 是一个单独的 `main`，不使用 purego，静态编译。

## task image:build {#task-imagebuild}

```bash
task image:build -- --stack python
task image:build -- --all
```

依赖 `task build`（guest 必须存在才能被注入）。见 [image](/cli/image) 和[镜像](/concepts/images)。

## 安装 libkrun {#instalação-da-libkrun}

```bash
task krun:install
```

| 系统 | 方式 |
|---------|------|
| macOS | `task krun:install`：来自 `nix/` 中 derivation 的 libkrun 1.19.6 / libkrunfw 5.6.2 |
| Linux | `task krun:install`：来自 nixpkgs 的 libkrun / libkrunfw，开启 virtio-blk |

两者都把 `.#krun` 安装到 Nix profile（`~/.nix-profile/lib`）中，`rc` 默认在那里查找。

## Windows {#windows}

没有 Windows 构建。为 `windows/amd64` 编译会有意失败，并给出说明原因的消息（上游 libkrun 没有 WHP 后端）。gate `task test:targets` 会检查这种拒绝持续生效。
