---
title: '示例与指南'
description: 'SDK 示例，以及在真实 VM 中验证 rc 每个流程的指南'
---

## 示例 {#exemplos}

[`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples) 中是 SDK 的示例，每个都在 macOS 27
（Apple M1 Pro）上对本地的 `rc serve` 测试过。要在受版本控制的文件中声明一台 machine，请参阅
[`runner.code`](/cli/runner-code) 格式。

| 示例 | 展示内容 | 状态 |
|---------|--------------|--------|
| [`sdk-go`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-go) | [Go SDK](/api-reference/sdks)：针对 `rc serve` 运行作业和 machine 生命周期 | 可用 |
| [`sdk-python`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-python) | 同上，使用 [Python SDK](/api-reference/sdks) | 可用 |
| [`desktop-claude`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude) | `runner.code` 中带 Claude Code 和 Firefox 的 XFCE 桌面，由 Go SDK 创建并核对 | 可用 |
| [`desktop-claude-agent`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude-agent) | 用三种方式向这台 VM 的 Claude 派发任务：`claude -p`、在窗口中输入、通过[屏幕 API](/cli/screen) | 可用 |

## 指南 {#guias}

[`examples/guides/`](https://github.com/andrebassi/runner-codes/tree/main/examples/guides) 中的每个目录都是一篇 `rc`
使用指南，带一个说明流程的 `README.md`，以及一个 `run.sh`：它在真实 VM 中证明指南所说的内容，并以失败数作为退出码。

```bash
task build
task guides                        # 所有目录
task guides DIRS="pack teardown"   # 部分目录
```

在 2026/10/21 的轮次中（macOS 27，Apple M1 Pro）：9 个目录，175 项检查，0 失败。

| 指南 | 证明了什么 |
|------|-------------|
| [install](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/install) | 真实启动：guest 的标记，以及与 host 不同的内核 |
| [throwaway-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/throwaway-machine) | 仓库 `:ro`、输出可写、无网络；一次放行一个主机；`--block-private`、host 的 loopback |
| [pack](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/pack) | 镜像和 machine 打成一个制品；源被删除后标记仍随之保留；缓存被复用 |
| [teardown](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/teardown) | 停止、删除、证明已清理干净；对 CLI 按 Ctrl-C 和 `kill -9` 都不会留下存活 VM |
| [credentials](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/credentials) | 真实值从不进入 VM（带反证的搜索）；只在 binding 主机上拦截；URL 中返回 403 |
| [branch-and-checkpoint](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/branch-and-checkpoint) | 带保留和锁定的磁盘 checkpoint、恢复任意一代、pause/resume、从预热源派生子 machine、把 checkpoint 导出为 pack |
| [gpu-cuda](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/gpu-cuda) | 没有 CUDA：`doctor`、CLI、`runner.code` 和 API 都会说不支持并给出原因，且不创建任何东西 |
| [local-api](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/local-api) | `rc serve` 与生命周期的 JSON 请求体；响应体中的 `exitCode`；SSE 每行一个事件；未知字段被拒绝 |
| [reference](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/reference) | 每个主题一个带证明的步骤（`runner.code`、命名网络、nested、限制、socket、display、文件属主、S3），以及不在范围内的主题及其原因 |

该目录中的 `lib.sh` 汇集了所有 `run.sh` 都会用到的内容：`rc machine ls` 的格式、VM 的进程
（`rc vmm --spec <dir>/spec.json`）、machine 的目录（`rc machine data-dir`）以及空列表（`No machines found`）。

## Runbook {#runbook}

除了指南，[`docs/runbook.md`](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.md) 是手工测试的
步骤序列，逐步给出每一步的预期结果；`task runbook` 执行相同的步骤，任何一步结果不同就判定失败（2026/10/24：146 项检查，0 失败）。
