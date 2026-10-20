---
title: '示例与指南'
description: '可直接运行的 runner.code 示例，以及在真实 VM 中验证 rc 每个流程的指南'
---

## 示例 {#exemplos}

[`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples) 中每个目录一个示例，每个都在 macOS 27
（Apple M1 Pro）的真实 VM 中测试过。machine 类示例是一个 [`runner.code`](/cli/runner-code) 文件；`rc` 接受 `-s` 作为
`-f` 的同义词来指定它。

| 示例 | 展示内容 | 状态 |
|---------|--------------|--------|
| [`python-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/python-app) | 公共镜像，`init` 联网执行 `pip` | 可用 |
| [`node-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/node-app) | `init` 执行 `npm -g`，应用运行在发布的端口上 | 可用 |
| [`openclaw-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/openclaw-app) | LLM 网关作为 workload，通过端口访问 `/health` | 可用 |
| [`doom-web`](https://github.com/andrebassi/runner-codes/tree/main/examples/doom-web) | 从卷提供静态文件的服务器，在浏览器中玩游戏 | 可用 |
| [`docker-in-vm`](https://github.com/andrebassi/runner-codes/tree/main/examples/docker-in-vm) | VM 内完整的 Docker | 可用 |
| [`headless-browser`](https://github.com/andrebassi/runner-codes/tree/main/examples/headless-browser) | headless Chromium 和截图 | 可用，不使用 GPU |
| [`gpu-chrome`](https://github.com/andrebassi/runner-codes/tree/main/examples/gpu-chrome) | 带 Vulkan/Venus 和 Chromium 的 Fedora | Vulkan 可用；Chromium 不使用 GPU |
| [`local-llm`](https://github.com/andrebassi/runner-codes/tree/main/examples/local-llm) | 在 host GPU 上用 Vulkan 运行 llama.cpp | 可用：GPU 上 56,7 tokens/s，CPU 上 10,3（Qwen2.5 0.5B Q4_K_M） |
| [`desktop`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop) | [浏览器屏幕](/cli/display)中的 Wayland 桌面（Weston），支持键盘和鼠标 | 可用 |
| [`sdk-go`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-go) | [Go SDK](/api-reference/sdks)：针对 `rc serve` 运行作业和 machine 生命周期 | 可用 |
| [`sdk-python`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-python) | 同上，使用 [Python SDK](/api-reference/sdks) | 可用 |

```bash
rc machine run -s examples/python-app/runner.code -- python3 --version
rc machine run -d --name doom -s examples/doom-web/runner.code && open http://localhost:8080
rc machine run -d --name openclaw -s examples/openclaw-app/runner.code && curl localhost:18789/health
```

### 首次 start 的耗时（实测） {#tempo-do-primeiro-start-medido}

首次 start 会运行 `init`（安装软件包）；之后的 start 复用 machine 的磁盘。

| 示例 | 首次 start | 之后 |
|---------|----------|-----------|
| python-app | 6 s | < 1 s |
| node-app | 2 s | < 1 s |
| openclaw-app | ~73 s（包括转换镜像） | < 1 s |
| docker-in-vm | 4 s | 4 s（dockerd 每次 start 都要启动） |
| headless-browser | 15 s | < 1 s |
| gpu-chrome | 581 s（Fedora 的元数据和软件包） | < 1 s |
| local-llm | 488 s（Fedora + 构建 llama.cpp） | < 1 s |
| desktop | 12 s | 5 s（udev、seatd 和 Weston 每次 start 都要启动） |

每个示例的说明（为什么 Chromium 不用 GPU 渲染、VM 内 Docker 的坑、desktop 的 2D 屏幕）见
[`examples/README.md`](https://github.com/andrebassi/runner-codes/tree/main/examples)。

## 指南 {#guias}

[`examples/guides/`](https://github.com/andrebassi/runner-codes/tree/main/examples/guides) 中的每个目录都是一篇 `rc`
使用指南，带一个说明流程的 `README.md`，以及一个 `run.sh`：它在真实 VM 中证明指南所说的内容，并以失败数作为退出码。

```bash
task build
task guides                        # 所有目录
task guides DIRS="pack teardown"   # 部分目录
```

在 2026/10/20 的轮次中（macOS 27，Apple M1 Pro）：12 个目录，215 项检查，0 失败。

| 指南 | 证明了什么 |
|------|-------------|
| [install](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/install) | 真实启动：guest 的标记，以及与 host 不同的内核 |
| [throwaway-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/throwaway-machine) | 仓库 `:ro`、输出可写、无网络；一次放行一个主机；`--block-private`、host 的 loopback |
| [pack](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/pack) | 镜像和 machine 打成一个制品；源被删除后标记仍随之保留；缓存被复用 |
| [teardown](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/teardown) | 停止、删除、证明已清理干净；对 CLI 按 Ctrl-C 和 `kill -9` 都不会留下存活 VM |
| [credentials](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/credentials) | 真实值从不进入 VM（带反证的搜索）；只在 binding 主机上拦截；URL 中返回 403 |
| [dev-env](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/dev-env) | 软件包、`$HOME` 和磁盘的持久化；`/tmp` 清空；init 以 root 运行一次；`user` 与 `--user` |
| [docker-in-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/docker-in-machine) | 真实 ext4 上的 Docker、嵌套容器、host 上的 `docker.sock`、能存活下来的数据 |
| [coding-agent](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/coding-agent) | 由 init 安装的编码 agent；在挂载的仓库中使用 git；host 的 SSH agent，密钥不进入 VM；API 密钥作为 secret |
| [branch-and-checkpoint](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/branch-and-checkpoint) | 带保留和锁定的磁盘 checkpoint、恢复任意一代、pause/resume、从预热源派生子 machine、把 checkpoint 导出为 pack |
| [gpu-cuda](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/gpu-cuda) | 没有 CUDA：`doctor`、CLI、`runner.code` 和 API 都会说不支持并给出原因，且不创建任何东西 |
| [local-api](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/local-api) | `rc serve` 与生命周期的 JSON 请求体；响应体中的 `exitCode`；SSE 每行一个事件；未知字段被拒绝 |
| [reference](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/reference) | 每个主题一个带证明的步骤（`runner.code`、命名网络、nested、限制、socket、display、文件属主、S3），以及不在范围内的主题及其原因 |

该目录中的 `lib.sh` 汇集了所有 `run.sh` 都会用到的内容：`rc machine ls` 的格式、VM 的进程
（`rc vmm --spec <dir>/spec.json`）、machine 的目录（`rc machine data-dir`）以及空列表（`No machines found`）。

## Runbook {#runbook}

除了指南，[`docs/runbook.md`](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.md) 是手工测试的
步骤序列，逐步给出每一步的预期结果；`task runbook` 执行相同的步骤，任何一步结果不同就判定失败（2026/10/20：116 项检查，0 失败）。
