---
title: 'Agent 参考'
description: 'rc 目前能做什么、每个命令怎么用、哪些功能尚未实现，以及演进计划'
---

这是一份简短而完整的参考，面向操作 `rc` 的人或 AI agent：有哪些功能、如何调用、每项功能保证了什么。
每一行描述的都是**目前已有**的功能。尚未实现的内容放在
[尚未实现的功能](#o-que-ainda-não-existe)一节和[演进计划](#plano-de-evolução)中。

`rc` 在**每个 job 一台全新的 microVM** 中执行不可信代码（由 LLM 生成），VM 拥有独立内核，没有网络，
root 只读，结束时销毁。VMM 是 libkrun，内核来自 libkrunfw。启动大约需要 260 ms。

## 平台 {#plataformas}

| Host | Guest | Hypervisor | 状态 |
|------|-------|-----------|----------|
| macOS Apple Silicon | Linux arm64 | Hypervisor.framework | 已端到端测试 |
| Linux x86_64 / aarch64 | 相同架构的 Linux | KVM（`/dev/kvm`） | 可编译；隔离已测试；完整启动未测量（没有带 KVM 的 host） |
| Windows | — | — | 不在范围内：上游 libkrun 没有 WHP。Windows 构建会被有意设为失败 |

## 安装 {#instalação}

```bash
task krun:install                    # 通过 Nix 安装 libkrun + libkrunfw（macOS 上还有 GPU）：nix profile add .#krun
task build                           # bin/rc（macOS 上已签名）+ bin/rc-guest-linux-<arch>
./bin/rc image build --stack python  # 或 --all；每个 stack 一个 ext4 镜像
./bin/rc doctor                      # 检查 host、libkrun、GPU、guest、sandbox 和镜像
```

不用 Homebrew，也不用 Docker：一切都通过 Nix。`nix develop` 提供开发所需的 Go、task、coreutils 和 Node。

## 速查 {#referência-rápida}

```bash
# 在全新 VM 中执行代码（冷启动，结束后 VM 消失）
./bin/rc run --stack python --code 'print(2**10)'
./bin/rc run --stack go --file main.go --stdin 'entrada' --timeout 30s

# 程序运行期间即可看到输出（streaming）
./bin/rc run --stack python --code $'import time\nprint("a")\ntime.sleep(2)\nprint("b")'

# 在全新 VM 中打开交互式 shell（真正的 TTY：Ctrl+C、窗口大小）
./bin/rc shell --stack bash

# 通过 Vulkan 使用 host 的 GPU（仅限支持 gpu 的 stack）
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'

# 持久化机器：写入的内容在命令之间、stop/start 之间都会保留
./bin/rc machine create --name dev --stack python
./bin/rc machine start  --name dev
./bin/rc machine exec   --name dev -- sh -c 'mkdir -p /workspace/out && echo pronto > /etc/motd'   # 没有网络：请用 --net 或 --allow-host 创建
./bin/rc machine cp ./app.py dev:/workspace/app.py
./bin/rc machine exec   --name dev -- python3 /workspace/app.py
./bin/rc machine stop   --name dev

# 带就绪 VM pool 的 HTTP API（以及机器相关路由）
./bin/rc serve --listen 127.0.0.1:8080 --pool-size 2
```

## 何时用哪个 {#quando-usar-o-quê}

| 目标 | 命令 |
|----------|---------|
| 运行一段隔离的代码并获取输出 | `rc run --stack S --code '...'` |
| 在运行过程中查看输出（构建、长时间测试） | `rc run`（本身就是 streaming）或 `POST /api/v1/execute/stream` |
| 手动探索某个 stack 的镜像 | `rc shell --stack S` |
| 需要保存状态的多步骤环境 | `rc machine create` → `start` → `exec`/`cp` → `stop`（[machine](/cli/machine)） |
| 以低延迟服务大量 job | `rc serve`：pool 把启动从关键路径上移除（每个 job 26–78 ms） |
| 使用 GPU（AI、Vulkan） | 在 `llama` stack 上使用 `--gpu`（[GPU](/advanced/gpu)） |
| 判断 host 是否就绪 | `rc doctor` |
| 列出 stack 以及哪些镜像已构建 | `rc languages` |

## CLI 结构 {#estrutura-da-cli}

```
rc run    --stack S (--code C | --file F) [--stdin T] [--timeout 10s] [--gpu]
rc shell  --stack S [--gpu]
rc machine create --name N --stack S [--cpus 2] [--memory 2048] [--disk-gib 10] [--gpu]
rc machine start|stop|status|delete [--name N]          # stop --timeout 10s; delete -f
rc machine ls [--json]
rc machine exec [--name N] [-e K=V] [-w DIR] [-i] [--timeout D] -- CMD ARGS...
rc machine shell [--name N] [-e K=V] [-w DIR] [-- CMD]  # 交互式终端（PTY）
rc machine web-terminal [--name N] [--port P] [--no-open] [-- CMD]  # 在浏览器中的同一个终端（127.0.0.1，带 token）
rc machine cp SRC DST                                    # 其中一侧是 nome:/caminho
rc machine logs [--name N] [--tail N]
rc serve  [--listen 127.0.0.1:8080] [--pool-size 2]
rc image build (--stack S ... | --all)
rc languages
rc doctor
rc vmm --spec <json> [--watch-fd 3] [--lib <libkrun>]   # 内部使用：每台 VM 一个进程
```

只有当程序以 0 退出时，`rc run` 才以 **0** 退出。非 0 退出、超时、编译失败以及 job 被拒绝，
都会得到 rc 1。

## Flag {#flags}

| Flag | 命令 | 作用 |
|------|---------|-----------|
| `--stack` | run, shell, image build | 目录中的 stack（`python`、`go`、`rust`、`java`、`nodejs`、`c`、`cpp`、`ruby`、`php`、`perl`、`bash`、`sh`、`awk`、`jq`、`sqlite`、`llama`） |
| `--code` / `--file` | run | 主文件的代码，直接给出或来自本地文件 |
| `--stdin` | run | 程序的标准输入（最多 1 MB） |
| `--timeout` | run | job 的时限，默认 10 s，最大 300 s；超时后 VM 被 `SIGKILL` 杀掉 |
| `--gpu` | run, shell | 通过 Vulkan（virtio-gpu/Venus）使用 host 的 GPU；仅限 `gpu: true` 的 stack，否则返回 E007 |
| `--listen` | serve | 监听地址；默认 loopback，因为 API 没有认证 |
| `--pool-size` | serve | 每个 stack 的就绪 VM 数量（默认 2） |
| `--all` | image build | 构建目录中的所有 stack |

## 环境变量 {#variáveis-de-ambiente}

| 变量 | 默认值 | 用途 |
|----------|--------|-----|
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes`（macOS） | 镜像、构建 VM、各 VM 的目录 |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib`（macOS）；Linux 上为 Nix profile 中的库或 `libkrun.so.1` | 加载哪个 libkrun |
| `RUNNER_GUEST_BIN` | 与 `rc` 同目录的 `rc-guest-linux-<arch>` | 注入镜像的 guest-runner |
| `RUNNER_SANDBOX` | 开启 | `off` 会关闭 Linux 上的 Landlock + seccomp（仅用于诊断；`vmm` 会发出警告） |
| `RC_MACHINE_NAME` | `default` | 缺少 `--name` 时使用的机器名（可用 direnv 按项目固定） |

## Stack 目录 {#catálogo-de-stacks}

位于 `internal/adapters/catalog/languages.yaml`，嵌入在二进制中。每个 stack 包含：

```yaml
- name: python               # CLI 和 API 接收的名称；必须存在于 guest 的执行器中
  image: python:3.12-alpine  # 源官方 OCI 镜像
  cpus: 1                    # 每个 job 的 vCPU 上限（防止滥用）
  memory_mib: 512            # RAM 上限；libkrun 的内存是弹性的
  warmup: python3 -m compileall -q -j0 /usr/local/lib/python3.12   # 构建时运行一次，有网络
  gpu: false                 # true 时允许 --gpu
  gpu_vram_mib: 0            # 带 GPU 时 guest 的 VRAM（0 = 4096）
```

构建过程不借助 Docker 拉取 OCI 镜像（go-containerregistry），在一台 Alpine 构建 VM 中解压，注入 guest，运行
warmup，并生成一个**只读 ext4**。缓存键包含镜像 digest、warmup、脚本以及 guest 的哈希，
因此其中任何一项变化都会触发重新构建。YAML 中出现未知字段会导致构建错误。

## HTTP API {#api-http}

`rc serve` 为每个 stack 维护一个已启动 VM 的 **pool**。每台 VM 只处理**一个** job 然后销毁，补充是
异步的，全局最多同时启动 4 台。

| 方法 | 路径 | 作用 |
|--------|---------|-----------|
| GET | `/health` | `{"status","version","pool":{"python":2,...}}` |
| GET | `/api/v1/languages` | stack、源镜像以及是否就绪 |
| POST | `/api/v1/run` | `{language, files[], stdin}`；`stdin` 为列表时，每一项运行一个 job（批处理） |
| POST | `/api/v1/execute` | `{language, code \| files[], stdin, timeout, gpu}` |
| POST | `/api/v1/execute/stream` | 请求体相同，响应为 Server-Sent Events |
| — | `/api/v1/machines/...` | 机器的生命周期：create、start、stop、exec、exec/stream、files、logs（[详情](/api-reference/overview#máquinas)） |

`/run` 和 `/execute` 的响应：

```json
{"status":"success","stdout":"1024\n","stderr":null,"exception":null,"executionTime":12}
```

- `status: "success"` 表示 API 执行了请求，**并不**表示程序执行成功：请查看 `stderr` 和 `exception`。
- `exception` 为 `null`、`"Timeout"`、`"Compilation failed"`，或某个基础设施故障的描述。

Streaming（`/execute/stream`）：

```
event: stdout
data: "a\n"

event: result
data: {"status":"success","exitCode":0,"exception":null,"executionTime":2034,"timedOut":false}
```

- `stdout`/`stderr` 的 `data` 是一个 JSON 字符串：换行符是转义后的。
- `result` 不会重复输出内容。
- 开始之后发生的故障会变成 `event: error`。

### 错误码 {#códigos-de-erro}

| 代码 | HTTP | 何时出现 |
|--------|------|--------|
| `E001: operation timed out` | 200 | guest 没有在 timeout 加上 host 余量（2 s）之内响应 |
| `E005: stdin too long` | 400 | stdin 超过 1.000.000 字节 |
| `E006: unsupported language X` | 400 | stack 不在目录中，或本机尚未构建其镜像 |
| `E007: stack X has no GPU support` | 400 | 在没有 `gpu: true` 的 stack 上请求了 `gpu` |
| 其他限制（timeout、文件、大小、名称） | 400 | `error` 中给出原因 |
| 基础设施故障 | 500 | `exception` 中给出原因（VM 未能启动、与 guest 的通道中断） |

## 重要模式 {#padrões-importantes}

- **每个 job 一台全新的 VM**：job 之间不会传递任何东西；pool 只是把启动从关键路径上移除。
- **没有网络**：vsock 创建时不带 TSI。如果不关闭隐式 vsock，libkrun 会自行开启网络。
- **root 只读**（通过 virtio-blk 提供的 ext4，而不是 virtiofs，后者会让 guest 触及 host 的其余部分）；`/tmp` 是一个
  256 MiB 的 tmpfs。
- **每个 job 的限制**：stdin ≤ 1 MB；最多 32 个文件；代码总量 ≤ 4 MiB；timeout 10 s（最大 300 s）。
- **每个 stack 的 CPU 和 RAM** 是防止滥用的上限；内存是弹性的，一台空闲 VM 约占 44 MB。
- **API 没有认证**：默认监听 `127.0.0.1`。

## 重要行为 {#comportamentos-importantes}

- **超时 = 对 VM 进程组发送 `SIGKILL`。** 不会残留进程；e2e 测试用 `pgrep` 验证。
- **如果 `rc` 退出，VM 也会随之退出**：Linux 上靠 `Pdeathsig`，macOS 上靠 fd 3 上的监视管道。
- **Streaming 传递的是程序写入管道的内容。** Python 以 `PYTHONUNBUFFERED=1` 运行。C（`printf`）、Ruby 和
  Perl 在输出不是终端时会缓冲 stdout，只在 `flush` 时才交付。
- **带终端的 `rc shell`** 会获得 virtio TTY 控制台，并把 host 终端切换到 raw 模式；使用管道时，保持普通
  控制台。
- **镜像过旧**：如果镜像中的 guest 早于 streaming 功能，错误信息会提示运行 `rc image build`。
- **Linux**：每台 job VM 的 `vmm` 进程都运行在 Landlock + seccomp 之下。Landlock ABI < 9 时，VM 之间没有
  Unix socket 隔离，`rc doctor` 会给出警告。带 GPU 的 VM 不在隔离范围内。
- **GPU**：带 GPU 的 VM 以冷启动方式运行，不经过 pool，同时最多 2 台，并共享 host 的 GPU（隔离粒度是
  进程，而不是 VM）。

## 尚未实现的功能 {#o-que-ainda-não-existe}

`rc` 使用 libkrun + libkrunfw、OCI 镜像，且没有任何 daemon。它执行不可信代码的**一次性 job**，
也管理持久化的**机器**；以下内容尚未实现，或只实现了一部分：

| 功能 | 当前 `rc` 中 | 说明 |
|---------|--------------|------------|
| `machine stats`、`machine monitor` | **无** | 阶段 D |
| API 中基于 WebSocket 的交互式 exec | **无** | `rc serve` 不提供终端；`rc machine web-terminal` 在浏览器中提供一个，仅监听 127.0.0.1 |
| `--error-format json` | 部分 | API 已返回稳定的 E00x 错误码 |
| RAM 和 CPU 的 checkpoint（Mac → Linux） | **无** | 上游 libkrun 没有 snapshot；`branch` 复制的是磁盘 |
| `--cuda` | **无** | 需要带 NVIDIA 的 Linux host |
| Windows（WHP） | **无** | 上游 libkrun 没有 WHP |

job stream 的最终事件是 `result`；机器 stream 的最终事件是 `exit`。HTTP API 带有预启动的 pool，并且在
Linux 上每台 job VM 都运行在 Landlock + seccomp 之下。

## 演进计划 {#plano-de-evolução}

每个阶段的收尾方式都与之前相同：AIOX story、连续两次 e2e、有意植入缺陷以证明测试能
发现它，以及 90% 的覆盖率 gate。**已完成：**阶段 A（story 7.1 和 7.2）、支持任意镜像的 `runner.code`
（story 7.3，属于阶段 D）以及阶段 C——带过滤的网络和卷（story 7.4）。剩下阶段 B 和阶段 D 的其余部分。

| 阶段 | 包含内容 | 设计决策 | 已知风险 |
|------|-------------|--------------------|-----------------|
| **A — 机器** ✅ | `rc machine create/start/stop/delete/ls/status/exec/shell/logs/cp`；路由 `/api/v1/machines/...` | 状态存放在 `<cache>/machines/<nome>/`；每台机器一个可写 ext4 磁盘；guest 中使用 overlayfs（镜像在下，磁盘在上），使安装的内容得以持久化；`/workspace` 位于磁盘上；`vmm` 以分离方式运行，记录 PID 并做存活检测 | 目前 `vmm` 被有意设计为随父进程退出，临时 job 必须继续保持这一点 |
| **B — secret 与 SSH** | `--secret-env`、`--secret-file`、`--ssh-agent` | secret 在 start 时解析，绝不写入注册信息；API 拒绝此类请求（否则等于任意读取 host 文件）；SSH 通过一个连接到 `SSH_AUTH_SOCK` 的 vsock 端口实现，私钥不会进入 VM | guest 中的 root 可以读取进程的环境变量；这是纵深防御，不是绝对保密 |
| **C — 带过滤的网络** ✅（7.4） | `--net`、`--allow-host`、`--allow-cidr`、`--block-private`、`-p`、`machine update`、卷 | virtio-net 搭配 host 上的用户态网络栈（例如 gvproxy），过滤由我们自己实现；TSI 仅保留给构建 VM | 这是成本最高、最敏感的部分；先测量再承诺 |
| **D — 便利功能** | ✅ `runner.code`（7.3）、`--image ./img.tar` 和 `-`、`machine stats`、`--error-format json` | go-containerregistry 已能读取 `docker save`/OCI layout | — |

建议顺序：A → B → D → C，C 放在最后，因为开放网络是对项目核心保证影响最大的改动。

## 延伸阅读 {#onde-aprofundar}

- [架构](/architecture)、[安全](/advanced/security)、[GPU](/advanced/gpu)
- [测试报告](/advanced/test-report)：每个平台上测量了什么
- `docs/prd.md`、`docs/architecture.md`、`docs/stories/`：需求、决策，以及每个 story 及其缺陷日志
