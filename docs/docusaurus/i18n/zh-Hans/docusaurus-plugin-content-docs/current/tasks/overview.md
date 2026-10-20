---
title: 'Task 概览'
description: 'Runner Codes 的 Taskfile.yaml 中的 task'
---

## 列表 {#lista}

项目使用 [Task](https://taskfile.dev)。不带参数运行 `task` 会列出全部。

| Task | 作用 |
|------|-----------|
| `task krun:install` | 通过 Nix 安装 libkrun + libkrunfw（`.#krun`） |
| `task build` | 在 `bin/` 中编译 host（`bin/rc`）和 guest，并在 macOS 上为 host 签名 |
| `task image:build -- --stack <l>` | 生成该 stack 的 ext4（`-- --all` 表示全部）；会先编译 |
| `task run -- <flags>` | 在一台新 VM 中执行代码 |
| `task shell -- --stack <l>` | 在一台新 VM 中打开交互式 shell |
| `task serve -- <flags>` | 在 `127.0.0.1:8080` 启动带 pool 的 API；会先编译 |
| `task doctor` | 检查 host 能否运行 microVM |
| `task test` | 带 race detector 的单元测试 |
| `task test:cov` | 覆盖率 gate：总计低于 90%、domain 低于 100% 或低于 guest 的棘轮时判定失败；包含 Python SDK |
| `task sdk:py:cov` | 仅 Python SDK：行和分支各自 ≥ 90% |
| `task test:targets` | 多目标 gate：darwin/arm64、linux/amd64 和 linux/arm64 可编译；windows 被拒绝 |
| `task test:e2e` | 针对真实 libkrun 的端到端测试；需要已构建的镜像 |
| `task runbook` | 执行 `docs/runbook.md` 并检查每个预期结果 |
| `task guides` | 运行 `examples/guides` 中的指南，每个都在真实 VM 中（部分运行用 `DIRS="pack teardown"`） |
| `task site:demos` | 网站演示背后的真实执行（部分运行用 `SCENES="network pack"`） |
| `task gates` | 依次运行 `test:targets`、`test:cov`、`test:e2e` 和 `runbook` |
| `task ship -- "<título>" <arquivo>` | 发布当前分支，创建 PR 并合并 |
| `task clean` | 删除 `bin/`；镜像缓存保持不动 |

## 典型流程 {#fluxo-típico}

```bash
task krun:install
task build
task doctor
task image:build -- --stack python
task run -- --stack python --code 'print(1)'
./bin/rc machine run --stack python -- python3 -V
task serve
task runbook
```

详见[构建](/tasks/build)和[测试](/tasks/testing)。
