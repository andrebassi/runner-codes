---
title: '安全'
description: '是什么隔离了不可信代码，哪些已由测试证明，哪些仍有欠缺'
---

## 模型 {#modelo}

作业代码被视为恶意代码。第一道屏障是 VM；第二道是对运行 VM 的进程的约束。

libkrun 的文档说得很明确：**guest 和 VMM 处于同一个安全上下文中。** 逃出 VM 就会落入 `rc vmm` 进程内部，拥有该进程的权限。因此隔离由 VM **加上**对这个进程的约束共同构成——而目前这种约束因平台而异。

## 作业的各层防护（两个平台通用） {#camadas-de-um-job-nas-duas-plataformas}

| 层 | 实现方式 |
|--------|------|
| 每个作业一台 VM | 每个作业在一台新 VM 中运行，结束后销毁；作业之间不传递任何东西 |
| 只读 root | root 是以只读方式挂载的 ext4（`/dev/vda`）；只能写入 `/tmp` 的 tmpfs（256 MiB） |
| 临时状态 | tmpfs 随 VM 一起消失 |
| 无网络 | `krun_disable_implicit_vsock` + `krun_add_vsock(ctx, 0)`：没有 TSI，没有网络接口 |
| 无 virtiofs | root 是磁盘，而不是共享 host 目录；virtiofs 无法阻止 guest 访问 host 的其他目录 |
| 按语言设上限 | vCPU 和 RAM 在目录中限定；超时默认 10 s，最长 300 s，超时向进程组发送 SIGKILL |
| 输入限制 | stdin ≤ 1.000.000 字节，最多 32 个文件，代码 ≤ 4 MiB，文件名不能含 `/`、不能含 `..`、不能以 `.` 开头 |
| 父进程退出，VM 随之退出 | host 崩溃时不会留下孤儿 VM |

## 按平台 {#por-plataforma}

| 项目 | macOS | Linux |
|------|-------|-------|
| hypervisor | Hypervisor.framework | KVM |
| 创建 VM 的权限 | 二进制上的 entitlement `com.apple.security.hypervisor` | 对 `/dev/kvm` 的访问权限 |
| VM 随父进程退出 | fd 3 上的看守管道 | `Pdeathsig` |
| 超时 | 向进程组发送 SIGKILL | 向进程组发送 SIGKILL |
| VM 进程使用非特权 uid/gid | — | spec 支持（`krun_setuid`/`krun_setgid`） |
| Landlock | macOS 上不存在 | 作用于每台作业 VM 的 `vmm` 进程 |
| seccomp | macOS 上不存在 | denylist 作用于所有线程（TSYNC） |

在 macOS 上，对 `vmm` 进程的约束只有 entitlement 和进程组。

### Linux 上的进程约束 {#confinamento-no-linux}

在把进程交给 libkrun 之前，作业 VM（root 在磁盘上，无网络）的 `vmm` 会应用：

| 机制 | 放行 | 拒绝 |
|---|---|---|
| Landlock（文件） | 读取 root 磁盘和已加载的库、`/proc`、`/sys`；写入控制台和 VM 自己的目录（`run/<id>/`）；`/dev/kvm` 及其 ioctl | 文件系统的其余所有部分 |
| Landlock（网络，ABI ≥ 4） | 无 | 所有 TCP `bind` 和 `connect`（ABI 10 起包括 UDP） |
| Landlock（作用域，ABI ≥ 6） | — | 发往本进程以外的信号和抽象 socket |
| Landlock（Unix socket，ABI ≥ 9） | VM 自己的 socket | 对其他 VM socket 的 `connect` |
| seccomp | 其余 | `execve`、`ptrace`、`mount`、`unshare`/`setns`、内核模块、`kexec`、`bpf`、`perf_event_open`、密钥环、时钟、swap、reboot、`userfaultfd` 等（`EPERM`） |

libkrunfw 在 Landlock 之前加载，因为 libkrun 只在 `krun_start_enter` 内部才打开它，约束生效之后这个 `open`
会失败。seccomp 有意采用 denylist 而不是 allowlist：allowlist 需要在带 KVM 的 Linux 上逐一测量 libkrun 的每个
syscall，漏掉一个就会让所有 VM 起不来。

**已实测**：在 Ubuntu 26.04（内核 7.0，aarch64，Landlock ABI 8）上，对受约束的进程本身测试：读取策略外的文件、
在磁盘旁写入、TCP、`execve` 和 `personality` 均被拒绝；读取磁盘和创建自己的 socket 被允许。去掉约束后，同一个探针
全部成功（失败证明）。

**未实测：** 在带 KVM 的 Linux 上，完整的 microVM 在约束下运行（测试 VM 没有 `/dev/kvm`）。

**已知缺口：** Landlock ABI < 9 时，内核不控制按路径对 Unix socket 的 `connect`；一台 VM 的进程仍能访问另一台 VM 的
socket。`rc doctor` 会给出警告。`RUNNER_SANDBOX=off` 可关闭约束用于诊断，并在 stderr 中提示。

## 持久化 machine {#máquinas-persistentes}

machine 放弃了作业隔离的一部分（它保存状态，并且可以有网络），每一处放开都是显式声明的，并有底线：

| 层 | 实现方式 |
|--------|------|
| 网络默认关闭 | 不加 `--net`（或 `net`，单独 `-p`）时，machine 没有网卡；只有 `-p` 而没有 `--net` 时，host 能访问端口，但 guest 出不去 |
| 出站策略 | 在 `vmm` 进程自身中运行的用户态网关，没有 TSI：按域名 allow-host、CIDR、`--block-private`；名单外的域名返回 NXDOMAIN；IP 只有来自被允许的域名时才有效。见[网络与卷](/cli/network) |
| 任何规则都打不开的底线 | 云 metadata 及所有 link-local；host 的 loopback 只能通过 `--allow-cidr 127.0.0.1` 放开，永远不能通过 DNS 学到的 IP 放开（DNS rebinding） |
| 拒绝记录 | 每一次被拒绝的出站都记入 machine 目录下的 `egress-denials.log` |
| 不可读取的凭据 | guest 只看到占位符；网关只在 binding 主机的 TLS 中把它替换为真实值，使用 machine 自己的 CA（name constraints）。URL 中出现占位符会被以 403 拒绝。见[凭据](/cli/credentials) |
| secrets | `--secret-env`/`--secret-file` 在 start 时读取，仅在内存中发给 guest；不写入注册表，也不写入 machine 磁盘 |
| SSH agent | guest 通过转发的 socket 使用 host 的密钥；私钥不会进入 VM |
| `:ro` 卷 | 保护在设备层面，而不只是挂载选项：guest 的 root 执行 `mount -o remount,rw` 也打不开该卷（已测试） |
| 嵌套虚拟化 | 在 host 不支持嵌套的地方拒绝 `--nested`；通过 API 时，只有 `rc serve --allow-nested-virt` 才允许 |
| 屏幕 | 查看器只监听 host 的 `127.0.0.1`，无密码 |

machine **不**防护的内容：guest 的 root 能读取自身进程的环境变量，所以交给 machine 的 secret 对在其中运行的代码是
可见的。凭据替换也无法阻止服务器在响应体中把该值返回。

## 端到端测试证明了什么 {#o-que-o-teste-ponta-a-ponta-prova}

| 属性 | 证明 |
|-------------|-------|
| 无网络 | 在 VM 内连接 `1.1.1.1` 失败；故意打开网络时，测试不通过 |
| 只读 root | 写入 `/etc` 失败 |
| 作业之间无状态 | 一个作业写入 `/tmp` 的文件在下一个作业中不存在 |
| 内存上限 | 在 512 MiB 的 VM 中分配 2 GiB 失败，不影响 host |
| 超时 | 死循环的作业被杀掉，没有 VM 残留 |

## API {#a-api}

HTTP API **没有认证**。默认只监听 `127.0.0.1`。没有带认证的代理挡在前面时，不要暴露到 loopback 之外。

## 镜像构建 {#build-de-imagem}

镜像的 pull 在 host 上进行。其余构建步骤在一台单独的、联网的 VM 中运行，因为安装 e2fsprogs 和 GNU tar 以及 warmup（例如 `apk add`）都需要网络。构建 VM 的网络到不了作业 VM。构建 VM 使用的 Alpine 基础 tarball 在使用前会校验 sha256。各语言的镜像来自官方 OCI 镜像。

## 待办 {#pendências}

- 在带 KVM 的 Linux 上验证完整的 microVM 在 Landlock + seccomp 下运行；
- API 认证（目前交给代理）；
- 在 Landlock ABI < 9 的内核上实现 VM 之间的 Unix socket 隔离。
