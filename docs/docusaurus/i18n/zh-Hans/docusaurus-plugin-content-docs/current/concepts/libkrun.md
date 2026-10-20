---
title: 'libkrun'
description: 'Runner Codes 如何使用 libkrun：启动、每台 VM 一个进程、关闭网络与只读 root'
---

## 是什么 {#o-que-é}

[libkrun](https://github.com/containers/libkrun) 是以库形式提供的 VMM：调用它的进程就变成 VM。内核来自 libkrunfw，同样以库的形式提供。使用的版本：

| 组件 | 版本 |
|------------|--------|
| libkrun | 1.19.6（macOS，`nix/` 中的自有 derivation）；nixpkgs 中为 1.19.5（Linux） |
| libkrunfw | 5.6.2，Linux 内核 6.12.109（macOS，`nix/`）；nixpkgs 中为 5.5.0（Linux） |

Runner Codes 直接调用 libkrun，没有中间层，并在运行时通过 purego 加载它——host 上没有 cgo。

| 平台 | Hypervisor |
|------------|-----------|
| macOS Apple Silicon | Hypervisor.framework（HVF） |
| Linux x86_64 / aarch64 | KVM（`/dev/kvm`） |
| Windows | 不支持：上游 libkrun 没有 WHP 后端 |

## 每台 VM 一个进程 {#um-processo-por-vm}

VM 运行期间 `krun_start_enter` 不会返回：它接管了整个进程。因此每台 VM 都是一个独立进程 `rc vmm --spec <arquivo>`，由 launcher 创建。

- **只用 spawn，绝不 fork。** fork 出来的 Go 进程（多线程）会在约 2 s 后被 HVF 杀掉。
- **超时：** 向 VM 的进程组发送 SIGKILL。
- **父进程退出，VM 随之退出**（作业 VM）：Linux 上用 `Pdeathsig`；macOS 没有对应机制，子进程监视在 fd 3 上继承的管道，管道关闭时退出。持久化 machine 的 VM 是例外：它以分离方式运行在自己的会话中，比启动它的 `rc` 活得更久。
- **macOS 上的库：** `vmm` 进程需要 `DYLD_LIBRARY_PATH` 指向 libkrun 所在目录，才能找到 `libkrunfw.5.dylib`；launcher 会设置它。
- **macOS 上的 entitlement：** 二进制需要 `com.apple.security.hypervisor`。没有它，`krun_start_enter` 返回 `-22`。

## Root：只读 ext4 磁盘 {#root-disco-ext4-read-only}

VM 把一个只读 ext4 磁盘挂载为 root：

- `krun_add_disk2` 挂接该语言的 ext4；
- `krun_set_root_disk_remount` 把 `/dev/vda` 以只读方式挂载为 root。

作业 VM 的第一个进程是 `/opt/runner/start.sh`，它会：

1. 在 `/tmp` 挂载一个 256 MiB 的 tmpfs；
2. 导出 OCI config 中的环境变量（`/opt/runner/env`：`PATH`、`JAVA_HOME`...），并设置 `HOME=/tmp/home`；
3. 对于 Go，把预热好的构建缓存从 `/opt/runner/gocache` 复制到 `/tmp/go-cache`；
4. 在端口 5000 上运行 guest。

所有写入都进入 tmpfs，随 VM 一起消失。在 machine 中，第一个进程是 `/opt/runner/machine.sh`，它挂载 machine 的磁盘，并在
镜像之上叠加一个 overlayfs；见 [machine](/cli/machine)。

**为什么用磁盘而不是 virtiofs：** 使用 virtiofs 时，共享无法阻止 guest 访问 host 的其他目录（见 libkrun 文档），而且并不更快。

## 网络：作业中关闭，machine 中按策略放行 {#rede-desligada-nos-jobs-por-política-nas-máquinas}

默认情况下 libkrun 会自动开启 TSI（transparent socket impersonation），借助 host 给 guest 提供网络。Runner Codes
把它关掉：

- `krun_disable_implicit_vsock` 阻止隐式 vsock；
- `krun_add_vsock(ctx, 0)` 添加一个不带任何 TSI 标志的 vsock。

结果：作业 VM 没有网络。端到端测试通过尝试连接 `1.1.1.1` 确认这一点，并且在故意打开网络时测试会不通过，从而证明测试本身有效。

带 `--net` 的 machine 同样不使用 TSI：libkrun 把一个 virtio-net 连接到 socketpair
（`krun_add_net_unixstream`），host 一侧是在 `vmm` 进程内运行的用户态网关，负责执行出站策略。见[网络与卷](/cli/network)。

## 与 guest 的通道 {#canal-com-o-guest}

`krun_add_vsock_port2` 以 `listen=true` 把 guest 的 vsock 端口 5000 绑定到 host 上的一个 Unix socket。协议见 [vsock 通信](/concepts/vsock-communication)。

## guest 进程的参数 {#argumentos-do-processo-do-guest}

libkrun 通过内核 cmdline 传递第一个进程的 argv。带空格的参数到 guest 中会被拆开。因此所有复合命令都写进文件（`start.sh` 和构建时的 warmup 就是这样），而不是作为参数传递。

## 内存与 CPU {#memória-e-cpu}

libkrun 的内存是弹性的：host 只为 guest 实际触及的页付出代价。测量中，配置 256 到 2048 MiB 并没有改变启动时间。目录中每种语言的 vCPU 和 RAM 值是防止滥用的上限，而不是预留。
