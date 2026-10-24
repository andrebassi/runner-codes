---
title: '测试'
description: '单元测试、覆盖率 gate、多目标 gate 和端到端测试'
---

## task test {#task-test}

```bash
task test
```

对 `./internal/...` 运行带 race detector 的单元测试。不需要 libkrun。在 2026/10/20 的轮次中：20 个包，全部通过。

## task test:cov {#task-testcov}

```bash
task test:cov
```

覆盖率 gate 会**判定失败**（rc ≠ 0），而不只是报告：

- `./internal/...` 和 `./pkg/...` 的总覆盖率 ≥ 90%（下限可通过 `COVERAGE_FLOOR` 调整）；
- domain（`internal/domain`）为 100%；
- guest（`internal/guest`）高于棘轮（`GUEST_FLOOR`，目前为 75%）；
- Python SDK（`sdk/python`）行和分支 ≥ 90%（`task sdk:py:cov`）。

在 2026/10/20 的轮次中：总计 95,2%，domain 100%，guest 75%，Python SDK 100%。

不计入统计的部分，原因写在脚本中：

| 排除项 | 原因 |
|----------|--------|
| `cmd/rc` | wiring：只负责组装各部分；由 e2e 验证 |
| `internal/guest`（不计入总计） | 继承下来的执行器，只能在 VM 内配合各工具链运行；作为已声明的技术债单独测量，并有一旦下降就判定失败的棘轮 |

证明 gate 确实有效：`COVERAGE_FLOOR=101 task test:cov` 必须以 rc ≠ 0 退出。

## task test:targets {#task-testtargets}

```bash
task test:targets
```

| 目标 | 预期 |
|------|----------|
| darwin/arm64 | 可编译 |
| linux/amd64 | 可编译 |
| linux/arm64 | 可编译 |
| windows/amd64 | **失败**，并给出声明的原因 |

对于带 OS build tag、但没有针对已做出决策的目标的文件，也会判定失败。

## task test:e2e {#task-teste2e}

```bash
task image:build -- --all
task test:e2e
```

针对真实 libkrun、用真实 VM 运行。需要已构建的镜像（GPU 测试需要 `llama`）。共 42 个测试；在 2026/10/20 的轮次中，
42 个在连续两轮中全部通过。其中证明的部分内容：

| 领域 | 证明 |
|------|-------|
| 作业 | stdout 和退出码；超时杀掉 VM 且不留进程；无网络（带失败证明）；只读 root，作业之间不传递任何东西；内存上限；各语言家族 |
| `shell` | 真实 TTY，Ctrl+C 只中断命令 |
| `serve` | pool、第 1 行在结束前到达的 SSE、machine 路由 |
| machine | 生命周期与持久化、终端、`runner.code`、`machine run`、branch 与暂停、文件复制 |
| 网络 | 没有 `--net` 就没有 `eth0`；按主机和按 CIDR 的策略；拒绝 metadata；ICMP；命名网络 |
| 卷与桥接 | `:ro` 能抵御 remount、`:staged`、磁盘、S3、`docker.sock`、`--expose-socket`/`--mount-socket` |
| 凭据 | guest 中是占位符，服务收到真实值；secrets 与 SSH agent |
| GPU、屏幕、pack | 带 `--gpu` 时可见 Venus；通过屏幕传输画面和输入；保留磁盘的 pack |

逐个测试的完整列表见[测试报告](/advanced/test-report)。

## task runbook {#task-runbook}

```bash
task runbook
```

逐步执行 `docs/runbook.md`，并检查每个预期结果。每个新功能都要在同一次交付中，在 runbook 中新增一节，并在
`scripts/runbook.sh` 中加上对应步骤。在 2026/10/24 的轮次中：146 项检查，0 失败。

## task guides {#task-guides}

```bash
task guides
task guides DIRS="pack teardown"
```

`examples/guides` 中的每个目录都有一个 `run.sh`，它在真实 VM 中证明指南所说的内容，并以失败数作为退出码。在
2026/10/21 的轮次中：9 个目录，175 项检查，0 失败。见[示例与指南](/guides/examples)。

## task gates {#task-gates}

```bash
task gates
```

完整一轮：依次运行 `test:targets`、`test:cov`、`test:e2e` 和 `runbook`。

## 已测试的平台 {#plataformas-testadas}

端到端测试、runbook、指南和性能测量都在 macOS 27、Apple Silicon 上完成。在 Linux 上，多目标 gate 保证可以编译，
Landlock + seccomp 约束已在没有 KVM 的 Ubuntu 26.04 上测试；Linux 上完整的 microVM 尚未测量。详见[测试报告](/advanced/test-report)。
