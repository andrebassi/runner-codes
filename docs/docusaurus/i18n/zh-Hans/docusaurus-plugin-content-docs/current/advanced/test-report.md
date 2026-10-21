---
title: '各平台测试报告'
description: 'rc 在 macOS、Linux 和 Windows 上的端到端测试：环境、方法、实测数据、发现的缺陷以及未测试的内容'
---

# 各平台测试报告

最近一轮完整测试：**2026/10/20**，`rc` 二进制运行在 libkrun + libkrunfw 之上。基础测量（启动、内存、镜像构建、各
stack 的执行）来自 **2026/10/19**，在同一台 host 上。本页每个数字都经过实测；未测试的内容会明确标注为未测试。

## 2026/10/20 轮次（macOS 27，Apple M1 Pro） {#rodada-de-20102026-macos-27-apple-m1-pro}

| Gate | 结果 |
|---|---|
| `task test` | 20 个包，全部通过（单元测试带 `-race`） |
| `task test:cov` | 总计 **95,2%**，domain **100%**，guest **75%**（棘轮为 75），Python SDK 行与分支均为 **100%** |
| `task test:e2e` | **42/42** 测试通过，连续两轮 |
| `task runbook` | **126** 项检查，**0** 失败 |
| `task guides` | **9** 个目录，**175** 项检查，**0** 失败 |
| `task test:targets` | darwin/arm64、linux/amd64 和 linux/arm64 可编译；windows/amd64 被拒绝并说明原因 |

在 Linux 上，Landlock + seccomp 已在 Ubuntu 26.04 中对真实受约束进程完成测试，该 VM 没有 KVM；Linux 上完整的
microVM **尚未测量**。`--nested` 只测了**拒绝**路径（这台 Mac 是 M1）；开启的路径没有测量。

> **同日更新：** 已在带 KVM 的 x86_64 Linux（DigitalOcean droplet，嵌套 KVM）上**完成**完整 microVM 的测量，
> job 在 Landlock（按线程，ABI 4）+ seccomp 下运行；Kubernetes 运行时在 k3s 和 microk8s 上均通过 19/19。详见
> [带 KVM 的 Linux 与 Kubernetes](#linux-com-kvm-e-kubernetes)。

## 各平台概览 {#resumo-por-plataforma}

| 平台 | 环境 | 真实 microVM | 结果 |
|---|---|---|---|
| **macOS** | MacBook M1 Pro，macOS 27.0.1，Hypervisor.framework | **是** | e2e 42/42，连续两轮；runbook 和指南无失败 |
| **Linux** | VMPal 中的 Ubuntu 26.04.1 LTS，内核 7.0.0-30，aarch64 | **否**（没有 `/dev/kvm`） | Landlock + seccomp 约束已在真实进程上应用并证明；30 个测试连续两轮通过 |
| **带 KVM 的 Linux** | DigitalOcean droplet `s-4vcpu-8gb`，Ubuntu 24.04，内核 6.8，x86_64，嵌套 KVM | **是** | Python job 在 Landlock（ABI 4，按线程）+ seccomp 下运行；Kubernetes 在 k3s 1.36 和 microk8s 1.35 上 19/19，两轮 |
| **Windows** | VMPal 中的 Windows 11 Pro 24H2（build 26100.9457）ARM64 | **否** | 不在范围内：构建被有意拒绝；VM 中 hypervisor 起不来（`EL2 not present`） |

Linux 和 Windows 都是“否”的共同原因：两者都是运行在 **M1 Pro** 上的 VM。Virtualization.framework 从 **M3** 起才
提供 EL2 异常级别（在 VM 中运行 hypervisor 所必需）。没有 EL2，任何嵌套 VM 都起不来，无论是 KVM 还是 Hyper-V。

## macOS（端到端实测的目标平台） {#macos-alvo-medido-de-ponta-a-ponta}

### 环境 {#ambiente}

| 项目 | 值 |
|---|---|
| 硬件 | MacBook Pro M1 Pro（arm64） |
| 系统 | macOS 27.0.1 |
| VMM | libkrun 1.19.6（仓库中的 Nix derivation，`nix/`） |
| guest 内核 | libkrunfw 5.6.2 → Linux 6.12.109 aarch64 |
| 二进制 | `bin/rc`，以 `com.apple.security.hypervisor` 做 ad-hoc 签名 |
| guest | `bin/rc-guest-linux-arm64`，静态，2,6 MB |

### 启动与资源（spike，每项 30 次启动） {#boot-e-recursos-spike-30-boots-por-medida}

使用一个 harness 测量：它启动 VM 进程，并计时到通过 vsock 收到作业的第一个响应为止。

| 指标 | 值 |
|---|---|
| 启动到 Python 首次响应，只读 ext4 root | p50 **260 ms**，p95 **266 ms**（最小 254，最大 279） |
| 启动到 `sh` 首次响应，virtiofs root | p50 245 ms，p95 264 ms |
| 完整周期 spawn → `uname` → 关机 | 缓存热时 221–252 ms；首次 869 ms |
| 空闲 VM 的 footprint（配置 512 MiB） | **44 MB**（`footprint`）；RSS 172 MB 包含共享的干净页 |
| 配置 RAM 256 / 1024 / 2048 MiB | 启动 260 / 260 / 261 ms；RSS 总计 +40 MB |
| 向进程组发送 `SIGKILL` | 没有进程残留（所有测得的启动） |

由此得出的决策：root 使用只读 ext4 磁盘。virtiofs 没有明显更快，而且会把 host 的一个目录暴露给 guest。

### 网络 {#rede}

| 配置 | `wget http://1.1.1.1` | `nc 1.1.1.1 80` |
|---|---|---|
| 不带 TSI 的 vsock（作业 VM 的默认配置） | `Network unreachable` | rc=1 |
| 带 TSI 的 vsock（仅构建 VM） | 连通 | rc=0 |

双向证明：测试使用 IP，因为用域名的话 DNS 失败会掩盖差异。

### 镜像构建（15 个 stack，`rc image build --all`） {#build-das-imagens-15-stacks-rc-image-build---all}

| Stack | OCI 镜像 | 构建 | ext4 |
|---|---|---|---|
| awk | alpine:3.22 | 2,6 s | 45 MiB |
| bash | bash:5.2 | 2,9 s | 57 MiB |
| c | alpine:3.22 + build-base | 19,8 s | 332 MiB |
| cpp | alpine:3.22 + build-base | 19,5 s | 332 MiB |
| go | golang:1.25-alpine | 35,3 s | 377 MiB |
| java | eclipse-temurin:21-jdk-alpine | 34,0 s | 478 MiB |
| jq | alpine:3.22 + jq | 2,7 s | 47 MiB |
| nodejs | node:22-alpine | 14,1 s | 234 MiB |
| perl | alpine:3.22 + perl | 8,4 s | 98 MiB |
| php | php:8.3-cli-alpine | 9,4 s | 152 MiB |
| python | python:3.12-alpine | 8,2 s | 128 MiB |
| ruby | ruby:3.3-alpine | 8,2 s | 134 MiB |
| rust | rust:1-alpine | 37,4 s | 1,2 GiB |
| sh | alpine:3.22 | 2,2 s | 45 MiB |
| sqlite | alpine:3.22 + sqlite | 2,8 s | 49 MiB |

大镜像的耗时主要花在 `mkfs.ext4` 上（Go 的 35 s 中占 27,8 s）：ext4 通过 virtiofs 在 host 上写入。
`python:3.12-alpine` 的 pull 与扁平化：2,7 s，55 MB。

### 各 stack 的执行（`rc run`，冷启动） {#execução-por-stack-rc-run-boot-frio}

| Stack | 执行 | 总计（含启动） |
|---|---|---|
| python | 31–44 ms | ~240 ms |
| nodejs | 128–148 ms | 353–371 ms |
| go | 缓存预热时 **247 ms**（不预热 3.429 ms） | 510 ms |
| java（JDK 21.0.12） | 522 ms | 739 ms |
| c | 71–72 ms | 282 ms |
| sh | 2 ms | 199 ms |

### 预启动 pool 的 API {#api-com-pool-pré-bootado}

| 场景 | 完整 HTTP 请求 |
|---|---|
| 1 个带镜像的 stack，pool 为 2 | 26–43 ms |
| 15 个 stack，其他 stack 仍在补充 pool | 73–78 ms |
| 15 个 stack，所有 pool 均已满 | 49–175 ms |
| 引入全局启动上限之前（30 台 VM 同时启动） | 1,7–2,3 s（发现并修复的回归） |

### e2e 测试套件（`task test:e2e`，tag `e2e`） {#suíte-e2e-task-teste2e-tag-e2e}

42 个测试，连续运行两次，两次都通过。每个测试锁定一项实测行为。下表列出固定作业和 machine 契约的测试；其余的见
后面的表。

| 测试 | 证明了什么 |
|---|---|
| `TestStdoutAndExitCode` | 输出正确；程序的 `exit 3` 变为 CLI 的 rc≠0 |
| `TestTimeoutKillsVM` | 死循环约 2,2 s 后超时，没有残留 VM 进程 |
| `TestNoNetwork` | 连接 1.1.1.1:80 被阻断。**失败证明：** 在 pool 中植入 `Network: true` 后测试不通过（Python 探针打印出已连接；当时的消息是葡萄牙语，现在是 `guest reached the network: "CONNECTED\n"`）；恢复后通过 |
| `TestRootReadOnlyAndIsolation` | `/etc` 不可写；一个作业写入 `/tmp` 的文件在下一个作业中不存在 |
| `TestMemoryCeiling` | 在 512 MiB 的 VM 中分配 2 GiB 在 VM 内失败，不影响 host |
| `TestLanguageFamilies` | 13 个 stack 可执行（nodejs、go、java、c、cpp、rust、ruby、php、perl、bash、awk、jq、sqlite）；Java 编译失败时 rc≠0 |
| `TestShell` | 通过管道提供 stdin 的 `rc shell` 启动后 root 只读、`/tmp` 可写、无网络；在 bash stack 上确实是 bash 5.2 |
| `TestShellTTY` | 在真实伪终端下运行 `rc shell`（`creack/pty`，40×120）：guest 的 stdin 是终端，`stty size` 返回 `40 120`，Ctrl+C 能中断 `sleep 30` 而不会让 VM 退出（测试耗时约 1,4 s；没有中断则需要 30 s）。**失败证明：** host 不进入 raw 模式时测试不通过（`VM survived the shell exit`）；恢复后通过 |
| `TestServePool` | `serve` 填满 pool，处理 `/run`，收到 SIGTERM 时不留下存活 VM |
| `TestRunStreams` | `rc run` 运行一个先写、休眠 1,5 s 再写的 Python：第 1 行比第 2 行早 ≥ 1 s 到达 host 的 stdout（实测：开始后 0,25 s 和 1,75 s）。**失败证明：** 让 `rc run` 缓存输出时，两行相隔 3,8 µs 到达，测试不通过 |
| `TestServeStream` | 用真实 VM 调用 `POST /api/v1/execute/stream`：`text/event-stream`，`stdout` 事件中第 1 行比第 2 行早 ≥ 1 s，最后的 `result` 中 `exitCode` 为 0；被拒绝的作业在 stream 开始前返回 400 |
| `TestMachineLifecycle` | `rc machine`：create，重名时 CONFLICT，machine 停止时 exec 返回 INVALID_STATE，start（198–244 ms），写入 `/etc`、`/opt` 和 `/workspace`，exec 的 rc 被透传（7），stack 的 Python 在其中运行，stop，machine 停止时查看 logs，再次 start：一切都持久化，`/tmp` 被清空；delete 不留下 VM。**失败证明：** machine 不带 `machine.sh` 启动时，写入 `/etc` 失败，测试不通过 |
| `TestMachineShellTTY` | 在运行中的 machine 上，于真实伪终端（40×120）下运行 `rc machine shell`：shell 看到 TTY 和 `40 120`，Ctrl+C 中断 `sleep 30` 而不会让 shell 退出，shell 写入 `/workspace` 的内容对 exec 可见，`exit 4` 变为 rc 4，machine 仍在运行。**失败证明：** `machine.sh` 中没有 devpts 时（真实发现的缺陷：`open /dev/ptmx: no such device`），shell 打不开，测试不通过 |
| `TestRunnerCode` | 由 `runner.code` 创建的 machine，命令行 `--cpus 1` 覆盖文件中的值（flag 优先）；工作目录 `/app` 在 start 时创建；workload 带着 env 和 workdir 启动，并出现在 `logs --source workload` 中；init 只运行一次（stop+start 后计数器仍为 1）；stop 发送 `SIGINT`，workload 的 trap 在 sync 之前完成写入。**失败证明（植入两个缺陷）：** stop 发送 SIGTERM 而不是 `stop_signal` → trap 文件不存在；init 忽略标记 → 计数器为 2。两者都未通过 |
| `TestRunnerCodeStopOnExit` | 设置 `stop_on_exit` 时，workload 结束后 machine 自行关机（约 0,5 s），日志显示输出和 `exited with code 0` |
| `TestRunnerCodeRefusals` | `net = true` 被拒绝并给出原因，什么也不会创建；既没有 stack 也没有 image 时报错 |
| `TestRunnerCodeImage` | `--image alpine:3.20`（目录外的镜像）：构建、启动并运行 `cat /etc/alpine-release` |
| `TestMachineNoNetwork` | 没有 `--net` 的 machine 没有 `eth0`，出不去 |
| `TestMachineNetwork` | 带 `--net`：`eth0` 为 `100.96.0.2/30`，DNS 指向网关，`example.com` 有响应；即使网络开放，metadata `169.254.169.254` 也会被立即拒绝 |
| `TestMachineEgressPolicy` | `--allow-host example.com`：该域名放行，`iana.org` 在 1 s 内得到 `bad address` 并记入 `egress-denials.log`；`machine update` 在 machine 运行时放行和移除。**失败证明：** 网关忽略策略时，metadata 和被拦截的域名都通过了，测试不通过 |
| `TestMachinePortsAndVolumes` | `runner.code` 带 rw 和 ro 卷、发布端口、没有 `--net`：workload 通过端口提供 host 上的文件，guest 的写入出现在 host 上，`:ro` **即使在 guest 的 root 执行 `mount -o remount,rw` 之后**仍拒绝写入，guest 无法向外拨号，卷在每次 start 时重新挂上。**失败证明：** 若 `:ro` 只体现在挂载选项上（设备上没有标志），remount 就能打开该卷，测试不通过。在加入这项检查之前，这个缺陷一直没被发现 |
| `TestMachineCopy` | `rc machine cp` 把 5 MiB（多个分块）传入 machine 再传回：内外 sha256 一致 |
| `TestMachineAPI` | `/api/v1/machines` 路由：201/409/200/204/404，exec 返回 rc 4，exec/stream 中第 1 行比第 2 行早 ≥ 1 s 且最后有 `exit`，2,6 MB 的 PUT/GET 内容一致，machine 停止时返回 INVALID_STATE（409），stop 在 5 s 内完成。**失败证明：** VM 脱离后没有 Wait（`rc serve` 中的僵尸进程），stop 耗时 10 s，测试不通过 |
| `TestGPUVulkan` | 在 `llama` stack 上使用 `--gpu`：`vulkaninfo` 显示 `Virtio-GPU Venus (Apple M1 Pro)`；不加 `--gpu` 时没有 `/dev/dri`；在不支持 GPU 的 stack 上使用 `--gpu` 被以 E007 拒绝。**失败证明：** `Configure` 中去掉 `krun_set_gpu_options2` 调用后，测试不通过（`no Venus device with --gpu`） |
| `TestGPULlama` | 同一模型分别在 GPU（`-ngl 99 -dev Vulkan0`）和 CPU 上运行 `llama-bench`：使用的设备是 Venus，prompt 处理快 ≥ 2 倍（实测 893–1.365 t/s 对比 193–264 t/s，4 轮） |

随 story 7.4 到 7.6 以及指南加入的测试：

| 测试 | 证明了什么 |
|---|---|
| `TestMachineRun` | `machine run`：命令在一台结束后即消失的 machine 中运行，rc 就是命令的 rc，`-i` 透传 stdin，没有可运行的内容时在创建之前就报错 |
| `TestMachineRunDetached` | `-d` 让 machine 保持运行，workload 使用指定的名字 |
| `TestMachineRunTTY` | `-it` 在真实伪终端下打开带 TTY 的 shell；其 `exit` 成为 rc，machine 随之消失 |
| `TestMachineRunCallerKilled` | `machine run` 的 CLI 收到 SIGKILL 后，不会留下无主的存活 VM（修复前会留下） |
| `TestMachineBranchAndPause` | 暂停时 machine 拒绝 exec 并给出原因；branch 带走父 machine 刚写入的内容；父子两者相互独立；`resume` 恢复父 machine |
| `TestMachineEgressCIDROnly` | 只有 `allow_cidrs` 时，解析出的域名不能建立连接（2026/10/19 修复的漏洞）；放行的 CIDR 仍可访问 |
| `TestMachinePing` | ICMP 经过网关，遵循与 TCP 相同的策略；allowlist 之外的目标没有回应，并记入拒绝日志 |
| `TestNamedNetworkMachines` | 同一网络中的两台 machine 可通过 IP 和 `<machine>.<network>.internal` 互相访问，无需 `--net`；其他网络无法访问；停止的 machine 从 DNS 中消失 |
| `TestMachineCredential` | guest 只能看到占位符，HTTPS 服务收到真实值；URL 中的占位符被以 403 拒绝；binding 以外的主机收到原样的占位符 |
| `TestSecretsAndSSHAgent` | `--secret-env`/`--secret-file` 能传到 exec 且不写入注册表；变量缺失时 start 失败；`--ssh-agent` 能列出 host 的密钥，而私钥不会进入 VM |
| `TestExposeAndMountSockets` | `--expose-socket` 和 `--mount-socket`，双向 |
| `TestDockerSocketAndDiskVolume` | `docker_socket` 在 machine 目录中有响应；磁盘卷是真正的 ext4，位于 overlay 之外，并且持久化 |
| `TestStagedVolume` | `:staged`：sync 之前 host 不变；sync 做镜像同步；优雅 stop 时会同步；branch 出来的子 machine 不回写任何内容 |
| `TestMachineS3Volume` | 针对真实 S3 服务器的 `s3://` 卷：读取、逐字节写入（8 MiB）、`mv` 和 `rm`、`:ro`、stop/start 后重新挂载，凭据错误时 start 失败并给出原因 |
| `TestMachineDisplay` | guest 的画面以正确的颜色到达 host 的查看器；通过 `POST /input` 发送的按键和点击到达 guest 的 evdev |
| `TestPack` | stack 生成 stub + payload；单文件 machine 在源被删除后仍可读取；持久模式；`create --from` 和 `pack inspect`；拒绝场景 |
| `TestDevStdinLinks` | `/dev/stdin`、`/dev/stdout`、`/dev/stderr` 和 `/dev/fd` 在作业 VM 和 machine 中都存在 |

### `rc shell` {#rc-shell}

| 检查 | 结果 |
|---|---|
| python：`python3`，`/tmp` 可写，`/etc` 只读，无网络 | ok |
| go：使用镜像 PATH 的 `go version` | `go1.25.14 linux/arm64` |
| bash：提示符与 `[[ ]]` | `bash-5.2#`，ok |
| 在伪终端下 | guest 的 stdin = 基于 `hvc0` 的 `/dev/console`；`test -t 0` 为真；`stty size` = host 终端的尺寸 |
| Ctrl+C | 中断 guest 中的命令；VM 仍然存活 |

工作原理：stdin 是终端时，`rc shell` 在 spec 中开启 `tty`（`krun_disable_implicit_console` +
`krun_add_virtio_console_default(0, 1, 2)`；三个 fd 都是 TTY 时，libkrun 只创建一个控制台端口），并在会话期间把
host 终端切换为 raw 模式，退出前恢复。stdin 来自管道时行为与之前相同（隐式控制台，没有 TTY）。`tty` 命令仍会打印
`not a tty`，因为它找不到设备名；`test -t 0` 和 `stty` 表明这确实是终端。

**负载事件：** 一轮 `-count=2` 的测试中，在 host load average 约 24 时（VMPal 的 VM 占 88% CPU，`nix-daemon` 占
79%），9 个测试失败了 6 个；`TestServePool` 耗时 40 s，正常为 7–8 s。单独运行以及随后两轮中，同样的测试套件全部通过。

### 2026/10/20 批次的数据 {#números-do-lote-de-20102026}

| 指标 | 值 |
|---|---|
| pack | python 镜像 134 MiB → 31 MiB payload；安装了软件包的 machine，单文件：46 MiB |
| S3 卷 | guest 中写入的 8 MiB 到达 bucket 时 md5 一致（本地 S3 服务器） |
| 发现的安全缺陷 | 只设 `allow_cidrs` 时允许访问任意域名；已修复，并由 `TestMachineEgressCIDROnly` 锁定 |

## Linux（进程约束已测试；microVM 未测试） {#linux-confinamento-testado-microvm-não}

### 环境 {#ambiente}

| 项目 | 值 |
|---|---|
| VM | VMPal 中的 "Ubuntu VM 2"，4 vCPU，7,9 GB |
| 系统 | Ubuntu 26.04.1 LTS，内核 7.0.0-30-generic，aarch64 |
| 启用的 LSM | `lockdown,capability,landlock,yama,apparmor,ima,evm` |
| Landlock | **ABI 8**（`landlock_create_ruleset(NULL, 0, LANDLOCK_CREATE_RULESET_VERSION)`） |
| `/dev/kvm` | **不存在**（M1 Pro 上没有嵌套 EL2） |

### 方法 {#método}

`internal/adapters/krun` 包的测试二进制在 Mac 上为 `linux/arm64` 编译，然后在 VM 中运行：

```bash
CGO_ENABLED=0 GOOS=linux GOARCH=arm64 go test -c -cover -o krun.test ./internal/adapters/krun/
vmpal cp krun.test "Ubuntu VM 2:rc-sandbox/krun.test"
# 在 VM 中：
./krun.test -test.v -test.count=1 -test.coverprofile=cover.out
```

约束测试会把自身二进制重新作为**探针**启动。探针应用真实策略（`ApplySandbox`，与 `vmm` 进程调用的是同一个函数），
然后逐一尝试各项操作。测试进程本身从不受约束。

### 约束结果（受约束的探针） {#resultado-do-confinamento-sonda-confinada}

| 操作 | 预期 | 实测 |
|---|---|---|
| 读取 root 磁盘 | 允许 | 允许 |
| 创建 VM 自己的 socket（`bind`） | 允许 | 允许 |
| 读取策略外的文件 | 拒绝 | **拒绝**（Landlock） |
| 在 root 磁盘旁写入 | 拒绝 | **拒绝**（Landlock） |
| TCP `connect` | 拒绝 | **拒绝**（Landlock，网络） |
| `execve("/bin/true")` | 拒绝 | **拒绝**（seccomp，`EPERM`） |
| `personality(0xffffffff)` | 拒绝 | **拒绝**（seccomp，`EPERM`） |
| `connect` 到另一台 VM 的 Unix socket | ABI 9 起拒绝 | **允许**（内核为 ABI 8） |

**失败证明**（`TestProbeWithoutSandbox`）：同一个探针在没有约束时能读取被禁止的文件、在磁盘旁写入、连接另一台 VM
的 socket、建立 TCP 连接并调用 `personality`。也就是说，测试能区分两种情况。

`ptrace` 不作为 seccomp 的证明：在 Ubuntu 上，即使没有任何约束，Yama 也已经拒绝对父进程的 `ptrace`。

### Linux 上的测试套件结果 {#resultado-da-suíte-no-linux}

30 个测试，连续两轮通过（`TestLoadRealLibrary` 被跳过：VM 中没有 libkrun）。包覆盖率：81,9%。在受约束子进程中执行的
代码不计入测量。

### VM 中的 `rc doctor` {#rc-doctor-na-vm}

```
FAIL  platform     no access to /dev/kvm: open /dev/kvm: no such file or directory
ok    sandbox landlock ABI 8 + seccomp denylist
warn  landlock ABI 8 < 9: a VM process can still connect to another VM's unix socket
```

### Linux 上未测试的内容 {#não-testado-no-linux}

- 在 Landlock + seccomp 下运行完整的 microVM（启动、作业、超时）：需要 KVM；
- libkrun 在 Linux 上于 `krun_start_enter` 中打开的路径列表。策略放行已加载的库、`/proc`、`/sys`、`/dev/kvm` 和
  常见设备节点，并预先加载 libkrunfw。如果 libkrun 需要其他路径，VM 就起不来；`RUNNER_SANDBOX=off` 可关闭约束用于诊断；
- `linux/amd64`（仅编译）。

## 带 KVM 的 Linux 与 Kubernetes {#linux-com-kvm-e-kubernetes}

### 环境

| 项目 | 值 |
|---|---|
| 机器 | DigitalOcean droplet `s-4vcpu-8gb`（4 vCPU，8 GB），nyc3；创建测试节点前，`scripts/do-kvm-probe.sh` 已测得 `/dev/kvm` 和 `vmx` 标志 |
| 系统 | Ubuntu 24.04，内核 6.8.0-142，x86_64；Landlock **ABI 4** |
| libkrun | nixpkgs 的 1.19.5 和 libkrunfw 5.5.0（`task krun:install`） |
| Kubernetes | k3s v1.36.5（containerd 2.3.4，flannel）和 microk8s v1.35.6（containerd 2.1.6，Calico），各一台 droplet |

### 结果

| 内容 | 实测 |
|---|---|
| `rc run --stack python`，冷启动 | `status=success`，端到端约 1.4 秒（嵌套 KVM；Mac 上约 240 毫秒） |
| `rc image build --stack python` | 60 秒（`mkfs` 占 45 秒） |
| `runtimeClassName: rc` 的 pod 到 `Running` | 约 2 秒，镜像已在节点上 |
| `deploy/k8s/e2e.sh` | k3s 和 microk8s 均为 **19/19**，各连续两轮 |
| e2e 的失败证明 | 在 shim 中把退出码固定为 0：两项退出码检查失败 |
| `task k8s:defects` | 植入 20 个缺陷；第一轮发现 19 个，漏掉的那个变成了测试，之后 20/20 |
| `TestRestrictThreadBlocks`（root，真实内核） | 按线程的 Landlock 拦住了策略外的文件；去掉 `landlock_restrict_self` 测试即失败 |
| `TestAttach`（root，真实 netns） | tap0 创建在 pod 的 netns 中而不是 host 中，每个方向各一个 mirred 过滤器 |

### 只在真实 Linux 上发现的缺陷

| 现象 | 原因 | 修复 |
|---|---|---|
| `nix profile add .#krun` 报 `File exists` | nixpkgs 的 libkrun 中 `lib/` 和 `lib64/` 包含相同文件 | `flake.nix` 中改用 `ln -sf` |
| 所有 job 失败：`prctl(PR_SET_NO_NEW_PRIVS): operation not supported` | ABI 8 以下 go-landlock 使用 psx，而没有 cgo 时 `AllThreadsSyscall` 返回 ENOTSUP（purego 让运行时以为自己"带 cgo"） | 在 `krun_start_enter` 所在线程上应用规则集（`sandbox_rules.go`），libkrun 的线程会继承 |
| launcher 的测试在 Linux 上无法编译 | `syscall.Getsid` 只存在于 BSD/macOS | 改用 `unix.Getsid` |
| 复制 rootfs 时"失败"：`waitid: no child processes` | pkg/shim 在 SIGCHLD 时回收所有子进程 | shim 的子进程通过 `reaper.Default` 等待 |
| kubelet 摘要在整个节点上失效；namespace 卡在 Terminating | shim 的 `Stats` 为空（`failed to decode container metrics`） | 返回带 vmm 真实用量的 cgroup v2 指标 |
| microk8s：`unsupported protocol: Yunix` | containerd 2.1 把 protobuf 格式的 start 响应当成地址读取 | 对使用旧协议的调用方返回旧格式 JSON |
| microk8s：pod 没有网络（`Network is unreachable`） | Calico 分配 `/32`，网关 `169.254.1.1` 在子网之外 | 在默认路由之前加一条到网关的链路路由 |

### 在带 KVM 的 Linux 上未测试的内容

- 带 KVM 的 `linux/arm64`（只测了 x86_64）；
- 在真实 microVM 上使用 ABI ≥ 8 的 Landlock（TSYNC 路径）：droplet 是 ABI 4；
- 真正开启 `--nested`（droplet 的 `rc doctor` 显示 `supported`，但没有验证）；
- pod VM 的 vmm **没有** Landlock/seccomp（已声明的技术债）。

## Windows（不在范围内，已测量） {#windows-fora-do-escopo-medido}

### 环境 {#ambiente}

| 项目 | 值 |
|---|---|
| VM | VMPal 中的 "Windows VM 1" |
| 系统 | Windows 11 Pro 24H2，build 26100.9457，ARM64，CPU "Apple Silicon" |

### `rc` 的构建 {#build-do-rc}

```
GOOS=windows GOARCH=arm64|amd64 go build ./cmd/rc
undefined: runnerCodesDoesNotSupportWindows_libkrunUpstreamHasNoWHPBackend
```

这是有意的拒绝：上游 libkrun 只有 KVM 和 Hypervisor.framework 后端，没有 Windows Hypervisor Platform。

### VM 中的 hypervisor {#hypervisor-dentro-da-vm}

| 步骤 | 实测 |
|---|---|
| 初始状态 | `HypervisorPresent=False`；`VirtualizationFirmwareEnabled`、`VMMonitorModeExtensions` 和 `SLAT` 均为 `False`；未安装 WSL |
| `vmpal set --nested on` | Windows：`nested virtualization is for Linux VMs`；Ubuntu：`Nested virtualization needs a Mac with M3 or later` |
| VMPal 还原点 `antes-hypervisor-2026-10-19` | 已创建 |
| `Enable-WindowsOptionalFeature` HypervisorPlatform、VirtualMachinePlatform、Microsoft-Hyper-V-All | 已启用，`RestartNeeded=True` |
| 重启后 | `bcdedit hypervisorlaunchtype Auto`；`vmms` 和 `vmcompute` 在运行；**`HypervisorPresent=False`** |
| 系统日志 | `Microsoft-Windows-Hyper-V-Hypervisor id=43: Hypervisor launch failed; EL2 not present.` |
| 回滚到还原点 | 三项功能均为 `Disabled`；`vmms` 和 `vmcompute` 不存在 |

结论：没有 EL2，这台 VM 运行不了任何 hypervisor，也没有任何 Windows 配置能绕过这一点。即使 `rc` 支持 Windows，这台
VM 也没有地方创建 microVM。

## 补齐矩阵还缺什么 {#o-que-falta-para-fechar-a-matriz}

| 缺口 | 解锁条件 |
|---|---|
| 在 TSYNC（ABI ≥ 8）的 Landlock 下运行 microVM | 带 KVM 且 Landlock ABI ≥ 8 的 Linux（Ubuntu 26.04 的 7.0 内核为 8）（droplet 的 ABI 4 已通过按线程路径测量） |
| 带 KVM 运行 `linux/arm64` | 带 KVM 的 arm64 Linux（`linux/amd64` 已在 droplet 上测量） |
| VM 之间的 Unix socket 隔离 | Landlock ABI ≥ 9 的内核 |
| 真正开启 `--nested` | M3+ 的 Mac 或支持嵌套 KVM 的 Linux |
| 原生 Windows | 把上游 libkrun 换成带 WHP 的 fork（架构决策），并使用开启虚拟化的 Windows 机器 |
