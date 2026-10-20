---
title: '简介'
description: '在 libkrun microVM 中执行不可信代码：一次性 job 与持久化机器'
sidebar_position: 1
---

## Runner Codes 是什么 {#o-que-é-o-runner-codes}

Runner Codes 在 microVM 中执行不可信代码（通常由 LLM 生成）。它的二进制叫 `rc`，有两种工作方式：

- **一次性 job：** 每个 job 一台全新的 VM，没有网络，root 只读，结束即销毁。`rc run`、`rc shell` 以及
  `/api/v1/run` 和 `/api/v1/execute` 路由都是这种方式。
- **持久化机器：** 一台拥有独立磁盘的 VM，两次 start 之间会保留已安装和已写入的内容。
  即 `rc machine ...`，可以在 `runner.code` 文件中声明，也可以通过 API 控制。

**源代码：** https://github.com/andrebassi/runner-codes

## 组成部分 {#as-peças}

- **VMM：** [libkrun](/concepts/libkrun) 1.19.6，搭配 libkrunfw 5.6.2 的内核（Linux 6.12.109），通过 purego
  直接作为库加载（不使用 cgo）。每台 VM 是一个 `rc vmm` 进程。
- **Hypervisor：** macOS Apple Silicon 上使用 Hypervisor.framework；Linux x86_64 和 aarch64 上使用 KVM（`/dev/kvm`）。
- **镜像：** 每个 stack 由[官方 OCI 镜像](/concepts/images)生成一个只读 ext4，无需
  Docker。机器还可以使用任意 OCI 镜像（`--image`）。
- **与 guest 的通道：** 4 字节 big-endian 长度 + JSON，走 vsock 端口 5000
  （[协议](/api-reference/vsock-protocol)）。
- **延迟：** `rc serve` 的 [pool](/concepts/pool) 按语言维护已启动好的 VM，每台只使用一次。

## `rc` 能做什么 {#o-que-o-rc-faz}

| 功能 | 文档位置 |
|---------|-----------|
| 通过 CLI 运行临时 job，输出以 streaming 方式显示；在全新 VM 中打开交互式 shell | [run 与 shell](/cli/run) |
| 机器：`create`、`start`、`exec`、`shell`、`cp`、`logs`、`stop`、`delete`、`branch`、`pause`、`resume`、`sync`、`update`、`data-dir`、`prune` | [machine](/cli/machine) |
| 在文件中声明的机器 | [runner.code](/cli/runner-code) |
| 基于策略的网络（allow-host、CIDR、`--block-private`）、命名网络、ICMP、端口与端口范围 | [网络与卷](/cli/network) |
| 卷：host 目录、`:ro`、`:staged`、机器磁盘、S3 | [网络与卷](/cli/network#volumes) |
| 机器可以使用但无法读取的凭据、secret、SSH agent、socket 桥接 | [凭据](/cli/credentials)、[machine](/cli/machine) |
| 基于 Vulkan（Venus）的 GPU 以及 `llama` stack | [GPU](/advanced/gpu) |
| 在浏览器中显示 VM 屏幕，支持键盘和鼠标（`--display`） | [浏览器中的屏幕](/cli/display) |
| 把镜像或机器打包成可移植的制品 | [pack](/cli/pack) |
| 带 pool、SSE 和机器路由的 HTTP API | [HTTP API](/cli/http-api)、[API 概览](/api-reference/overview) |
| Go（`pkg/client`）与 Python（`sdk/python`）SDK | [SDK](/api-reference/sdks) |
| host 自检 | `rc doctor`，见 [CLI 概览](/cli/overview#doctor) |

## 平台 {#plataformas}

| 平台 | 状态 |
|------------|--------|
| macOS，Apple Silicon | 已支持，已端到端测试并完成测量 |
| 带 `/dev/kvm` 的 Linux x86_64 / aarch64 | 已支持；Landlock + seccomp 隔离已测试，但完整 microVM 在 Linux 上**尚未测量** |
| Windows | **不支持**：上游 libkrun 没有 WHP 后端，Windows 构建会被有意设为失败 |

## 实测数据 {#números-medidos}

macOS 27，Apple Silicon。Linux 尚未测量。

| 项目 | 值 |
|-------|-------|
| 从启动到第 1 次 Python 响应（30 次启动，只读 ext4 root） | p50 260 ms，p95 266 ms |
| 通过 CLI 运行 Python job，冷启动 | 总计约 240 ms |
| pool 已满时通过 API 运行 job | 完整 HTTP 请求 26–43 ms（单个 stack） |
| `machine start` 到 guest 响应 | 204–244 ms |
| 一台空闲 VM 的内存占用 | 44 MB |
| 镜像中构建缓存已预热时的 `go run` | 247 ms（无缓存时 3.4 s） |

细节与测量方法见[性能](/advanced/performance)和[测试报告](/advanced/test-report)。

## 语言 {#linguagens}

目录中共有 15 门语言：Python、Node.js、Go、Rust、Java、C、C++、Ruby、PHP、Perl、Bash、sh、AWK、jq 和 SQLite，
另有 `llama` stack（带 Vulkan 的 llama.cpp）。参见[语言](/languages/overview)。

## 下一步 {#próximos-passos}

- [快速开始](/quickstart)：安装 libkrun、编译、构建镜像、运行第一个 job 和第一台机器。
- [Agent 参考](/agents)：所有命令、flag、路由和错误集中在一页。
- [架构](/architecture)：各个组成部分，以及一个 job 和一台机器的执行路径。
- [示例与指南](/guides/examples)：`runner.code` 示例，以及在真实 VM 中验证每个流程的指南。
- [安全](/advanced/security)：哪些机制隔离了代码，哪些还欠缺。
