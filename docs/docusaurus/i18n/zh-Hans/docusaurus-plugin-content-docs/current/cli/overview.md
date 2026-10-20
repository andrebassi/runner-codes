---
title: 'CLI 概览'
description: 'rc 的各个子命令'
---

## 二进制 {#o-binário}

`rc` 是 host 端的二进制。它运行在 macOS Apple Silicon 以及带 KVM 的 Linux x86_64/aarch64 上。

```bash
rc --help
```

| 子命令 | 作用 | 页面 |
|------------|-----------|--------|
| `run` | 在一台全新的 VM 中执行代码（冷启动），输出以 streaming 方式显示 | [run 与 shell](/cli/run) |
| `shell` | 在某个 stack 的全新 VM 中打开交互式 shell | [run 与 shell](/cli/run#shell) |
| `machine ...` | 持久化机器：`create`、`start`、`exec`、`shell`、`cp`、`logs`、`stop`、`delete`、`run`、`branch`、`pause`、`resume`、`sync`、`update`、`status`、`ls`、`data-dir`、`prune` | [machine](/cli/machine)、[runner.code](/cli/runner-code)、[网络与卷](/cli/network) |
| `pack ...` | `create`、`inspect`、`prune`：把镜像或机器打包成可移植制品 | [pack](/cli/pack) |
| `serve` | 带预启动 VM pool 和机器路由的 HTTP API | [HTTP API](/cli/http-api) |
| `image build`、`image prune` | 从官方 OCI 镜像为一个或多个 stack 生成 ext4；清理缓存 | [image](/cli/image) |
| `languages` | 列出目录，以及每个 stack 的镜像是否已构建 | 见下文 |
| `doctor` | 检查 host 能否运行 microVM，以及它提供哪些能力 | 见下文 |
| `vmm` | 单台 VM 的进程；内部使用，由 launcher 调用 | [libkrun](/concepts/libkrun) |

guest（`runner-guest`）是另一个二进制，只在 VM 内运行；不需要手动调用。

## languages

```bash
rc languages
```

目录中每个 stack 一行；ext4 已在缓存中时显示 `ready=true`，并给出源 OCI 镜像：

```
awk      ready=true  alpine:3.22
...
llama    ready=true  fedora:43
nodejs   ready=false node:22-alpine
python   ready=true  python:3.12-alpine
```

## doctor

```bash
rc doctor
```

每项一行，状态为 `ok`、`info`、`warn` 或 `FAIL`。在 Mac M1 Pro 上的真实输出：

```
ok    platform
ok    libkrun
ok    gpu: libkrun built with virtio-gpu (Venus); use --gpu on a stack with gpu
info  cuda: not supported (no CUDA remoting; --gpu is Vulkan)
info  nested: not supported on this host; --nested is refused
ok    guest
info  sandbox: macOS has no Landlock/seccomp; the VM process runs unconfined
ok    image awk
...
```

| 项 | 检查内容 |
|------|----------------|
| `platform` | Apple Silicon 上的 macOS，或可读写打开 `/dev/kvm` 的 Linux |
| `libkrun` | 能否从配置的路径加载该库 |
| `gpu`、`cuda` | libkrun 是否带 virtio-gpu（Venus）；不支持 CUDA |
| `nested` | host 是否支持嵌套虚拟化（`--nested`） |
| `guest` | guest 二进制是否位于 host 二进制旁边 |
| `sandbox` | Linux 上 Landlock 的 ABI 和 seccomp；ABI < 9 时给出警告 |
| `image <stack>` | 每个已构建的镜像一行 |

失败输出为 `FAIL <item> <motivo>`，只要有一项失败，命令就以错误退出。

## 环境变量 {#variáveis-de-ambiente}

| 变量 | 默认值 | 用途 |
|----------|--------|-----|
| `RUNNER_LISTEN` | `127.0.0.1:8080` | `serve` 的地址 |
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes`（macOS），`~/.cache/runner-codes`（Linux） | 镜像、构建 VM、机器、pack、socket |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib`（macOS）；Linux 上为 `~/.nix-profile/lib/libkrun.so.1` 或 soname `libkrun.so.1` | libkrun 的路径 |
| `RUNNER_GUEST_BIN` | 与 host 同目录的 `rc-guest-linux-<arch>` | 注入镜像的 guest |
| `RUNNER_SANDBOX` | 开启 | `off` 会关闭 Linux 上的 Landlock + seccomp，仅用于诊断 |

## 通过 Taskfile {#pelo-taskfile}

| Task | 等价于 |
|------|------------|
| `task run -- <flags>` | `./bin/rc run <flags>` |
| `task shell -- <flags>` | `./bin/rc shell <flags>` |
| `task serve -- <flags>` | `./bin/rc serve <flags>` |
| `task image:build -- <flags>` | `./bin/rc image build <flags>` |
| `task doctor` | `./bin/rc doctor` |
