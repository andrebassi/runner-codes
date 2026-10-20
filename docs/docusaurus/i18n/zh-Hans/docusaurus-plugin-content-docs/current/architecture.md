---
title: '架构'
description: 'Runner Codes 的各个组成部分，以及一个 job 和一台机器从 HTTP 到 microVM 的执行路径'
sidebar_position: 3
---

## 概览 {#visão-geral}

```
 HTTP 客户端 / CLI / SDK
        │
        ▼
┌───────────────────────────────┐
│ rc  (host：macOS 或 Linux)    │
│ ├─ 领域层：校验 job/spec      │
│ ├─ pool：按语言预备的 VM      │
│ │   （仅用于 job）            │
│ ├─ 机器：注册信息、磁盘       │
│ └─ launcher：spawn vmm 进程   │
└──────────────┬────────────────┘
               │ spawn（绝不 fork）
               ▼
┌───────────────────────────────┐
│ rc vmm --spec   （每 VM 1 个）│
│ libkrun (purego) +            │
│ libkrunfw (Linux 内核)        │
│ macOS 上 HVF / Linux 上 KVM   │
│ 网络网关（带 --net 的机器）， │
│ 屏幕查看器                    │
└──────────────┬────────────────┘
               │ Unix socket  ⇄  vsock 端口 5000
               ▼
┌───────────────────────────────┐
│ microVM                       │
│ job：只读 ext4 root，         │
│      /tmp 为 tmpfs，无网络    │
│ 机器：镜像 overlay +          │
│      独立磁盘（/dev/vdb）     │
│ /opt/runner/guest（静态）     │
└───────────────────────────────┘
```

## 二进制 {#binários}

| 二进制 | 运行位置 | 作用 |
|---------|-----------|-----------|
| `rc` | host | `run`、`shell`、`machine ...`、`pack ...`、`serve`（API + pool）、`image build/prune`、`languages`、`doctor`，以及内部子命令 `vmm` |
| `runner-guest` | VM 内部 | 监听 vsock 端口 5000 并处理每个操作：运行代码、exec、文件复制、PTY、workload、端口发布、secret、SSH agent、S3 挂载。保存为 `bin/rc-guest-linux-<arch>` |

之所以分成两个二进制，是因为 purego 在 Linux 上会强制针对 glibc 动态链接，而各语言镜像都是 Alpine
（musl）。动态链接的 guest 在 VM 内会以 `not found` 失败；因此 guest 以静态方式编译，如果它被编译成动态链接，
`scripts/build.sh` 会判定失败。

## host 的分层 {#camadas-do-host}

host 代码采用六边形架构；`cmd/rc` 只负责把各部分组装起来。

| 层 | 包 | 职责 |
|--------|--------|------------------|
| 领域层 | `internal/domain` | job、结果、限制（stdin、文件、大小、timeout）以及 E00x 错误 |
| 应用层 | `internal/app` | job 的执行器与 pool；机器的生命周期、branch、staged 卷、凭据、secret |
| ports | `internal/ports` | 接口（`VMSpec` 及其校验、launcher、目录、镜像） |
| adapters | `internal/adapters/krun` | 加载 libkrun、配置 VM、launcher、`vmm` 进程以及 Linux 上的隔离 |
| | `internal/adapters/oci` | 拉取 OCI 镜像并在构建 VM 中生成 ext4 |
| | `internal/adapters/catalog` | 内嵌的 `languages.yaml` |
| | `internal/adapters/guestproto` | vsock 协议客户端 |
| | `internal/adapters/httpapi` | HTTP API（job 与机器） |
| | `internal/adapters/machinestore` | 每台机器的注册信息和磁盘（基础磁盘的克隆） |
| | `internal/adapters/runfile` | 读取 `runner.code` 文件（TOML） |
| | `internal/adapters/netgw` | 用户态网络网关：出站策略、DNS、端口、ICMP、命名网络、凭据拦截 |
| | `internal/adapters/screen`, `viewer` | 在浏览器中显示 VM 屏幕 |
| | `internal/adapters/hostfs` | 卷在 host 一侧的处理（`:staged` 文件的权限模式） |
| 其他 | `internal/credca`, `internal/objectstore`, `internal/s3fs`, `internal/pack` | 每台机器的 CA、S3 客户端、`s3://` 卷的核心、pack 格式 |
| guest | `internal/guest` | vsock 服务端以及按语言划分的执行器 |

SDK：`pkg/client`（Go）和 `sdk/python`（Python），只依赖标准库。

## 一个 job 经由 API 的路径 {#caminho-de-um-job-pela-api}

1. `POST /api/v1/run` 到达 `httpapi`，由它组装 job。
2. 领域层校验：语言在目录中（否则 E006），stdin ≤ 1.000.000 字节（否则 E005），最多 32 个文件，
   代码总量 ≤ 4 MiB，文件名合法，timeout 在 0 到 300 s 之间。被拒绝时返回 HTTP 400。
3. pool 交出一台该语言已启动好的 VM。如果 pool 为空，就当场启动一台 VM（冷启动）：
   绝不拒绝。
4. host 连接到与 VM 的 vsock 端口 5000 相连的 Unix socket，并以帧的形式发送 job（4 字节长度 + JSON）。
5. guest 把文件写入 `/tmp` 下的一个目录，如有需要先编译，然后以 job 的 stdin 和
   timeout 执行，再用另一个帧回复。
6. VM 被销毁（对进程组发送 SIGKILL），pool 在后台补充一台新的 VM。
7. API 返回 `status`、`stdout`、`stderr`、`exception`、`executionTime`。

## 一台机器的路径 {#caminho-de-uma-máquina}

1. `rc machine create` 把注册信息写入 `<cache>/machines/<nome>/machine.json`，并从基础磁盘克隆出一个 ext4 磁盘
   （APFS 上用 clonefile，btrfs/XFS 上用 FICLONE，其他情况用稀疏复制）。
2. `rc machine start` 以**分离**方式启动 `rc vmm`（独立会话，与父进程无关联）：命令退出后
   机器继续运行。pid 记录在 `vmm.pid` 中。
3. VM 的第一个进程是 `/opt/runner/machine.sh`：把磁盘挂载到 `/persist`，建立一个 overlayfs（镜像在
   下层，`upper/` 在上层），挂上 `/workspace`，并在 `chroot` 中运行 guest。
4. `exec`、`shell`、`cp`、`logs` 都通过同一个端口 5000 的 socket 与 guest 通信。带 `--net` 时，VM 获得 `eth0`，
   流量经过 `vmm` 进程自身的网关，由它执行出站策略。
5. `rc machine stop` 请求 guest 执行 `shutdown`，guest 执行 `sync` 后退出；如果在 `--timeout` 内没有结束，则 SIGKILL。

详见 [machine](/cli/machine) 和[网络与卷](/cli/network)。

## VM 的生命周期 {#ciclo-de-vida-de-uma-vm}

- launcher 通过 **spawn** 创建 `rc vmm --spec <arquivo>` 进程。多线程进程 fork 出来的子进程会被
  Hypervisor.framework 杀掉，所以绝不使用 fork。
- `vmm` 进程加载 libkrun，根据 spec 配置 VM，并调用 `krun_start_enter`，后者接管
  整个进程直到 VM 结束。
- job 超时：对进程组发送 SIGKILL。
- job 的 VM 随父进程一起退出：Linux 上靠 `Pdeathsig`；macOS 上靠 fd 3 上的监视管道。机器的 VM 是分离的。
- 如果 `vmm` 在启动阶段退出，start 会立即结束，并引用本次 start 写入 `vmm.log` 的最后一行。

详见 [libkrun](/concepts/libkrun)。

## 镜像 {#imagens}

每个 stack 都有一个由官方 OCI 镜像生成的只读 ext4，其中写入了 guest、`start.sh` 和 warmup 结果。
构建在一台专用的 Alpine VM 中运行。参见[镜像](/concepts/images)。

## 通过环境变量配置 {#configuração-por-ambiente}

| 变量 | 默认值 | 用途 |
|----------|--------|-----|
| `RUNNER_LISTEN` | `127.0.0.1:8080` | API 地址（`serve`） |
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes`（macOS），`~/.cache/runner-codes`（Linux） | 镜像、构建 VM、机器、pack 和 socket |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib`（macOS）；Linux 上为 `~/.nix-profile/lib/libkrun.so.1` 或 soname `libkrun.so.1` | libkrun 的路径 |
| `RUNNER_GUEST_BIN` | 与 host 同目录的 `rc-guest-linux-<arch>` | 注入镜像的 guest |
| `RUNNER_SANDBOX` | 开启 | `off` 会关闭 Linux 上的 Landlock + seccomp，仅用于诊断 |
