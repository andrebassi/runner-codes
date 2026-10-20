---
title: '性能'
description: '启动、执行、machine、内存和镜像构建的实测数据'
---

## 测量环境 {#onde-foi-medido}

本页所有数据均于 2026/10/19 和 10/20 在 **macOS 27、Apple Silicon（M1 Pro）** 上实测。**Linux 尚未测量**；不要假设
这些数值在 KVM 上同样成立。

## 启动 {#boot}

| 指标 | 数值 |
|--------|-------|
| 启动到 Python guest 首次响应，只读 ext4 root，30 次启动 | p50 260 ms，p95 266 ms |
| 配置的 RAM（256 到 2048 MiB）对启动的影响 | 无 |

“响应”指 guest 对就绪等待每 1 ms 发送一次的 `sh true` 作业给出的第一个回复。此时 VM 已经可以接收真正的作业。

## 单个作业的执行 {#execução-de-um-job}

| 路径 | 数值 |
|---------|-------|
| CLI，Python 作业，冷启动 | 总计约 240 ms，其中执行 31–38 ms |
| API，pool 已满，单个带镜像的 stack | 完整 HTTP 请求 26–43 ms |
| API，15 个 stack，所有 pool 均已满 | 49–175 ms |

CLI 与 API 的差别在于启动，[pool](/concepts/pool) 把启动移出了请求路径。pool 为空时，API 也会落到与 CLI 相同的冷启动。

各 stack 通过 CLI 冷启动的数据：

| Stack | 执行 | 总计（含启动） |
|-------|----------|------------------|
| python | 31–44 ms | ~240 ms |
| nodejs | 128–148 ms | 353–371 ms |
| go | 构建缓存预热时 247 ms（不预热 3,4 s） | 510 ms |
| java | 522 ms | 739 ms |
| c | 71–72 ms | 282 ms |
| sh | 2 ms | 199 ms |

## Machine {#máquinas}

| 指标 | 数值 |
|--------|-------|
| `machine start` 到 guest 响应（python） | 204–244 ms |
| 带网络的 machine 的 `start` | 190 ms |
| 新建的 10 GiB 磁盘在 host 上占用的空间 | 6,4 MB |
| 向 machine 写入 / 从 machine 读出 50 MB 的 `cp` | 620 ms（~84 MB/s）/ 594 ms（~88 MB/s） |
| 被策略拦截的域名到 guest 中报错 | < 1 s |

## 内存 {#memória}

| 指标 | 数值 |
|--------|-------|
| 一台空闲 VM 的 footprint | 44 MB |
| 同一台 VM 的 RSS | 172 MB，包含共享的干净页 |

libkrun 的内存是弹性的：host 只为 guest 实际触及的页付出代价。目录中每种语言的 RAM 上限用于防止滥用，但并不预留。

## 镜像构建与 pack {#build-de-imagem-e-pack}

| 指标 | 数值 |
|--------|-------|
| 构建 Python 镜像 | ~8–10 s |
| pull + 层扁平化（python） | 2,7 s |
| Python 的 warmup（`compileall`） | ~1 s |
| 构建 Go 镜像 | 35 s，其中 `mkfs.ext4` 占 28 s |
| python 镜像的 pack | 134 MiB → 31 MiB payload |
| 安装了软件包的 python machine 的 pack，单文件 | 46 MiB |

构建对每个缓存键只需一次；相同输入的第二次构建直接命中缓存。各 stack 的耗时见[测试报告](/advanced/test-report)。

## GPU {#gpu}

`llama-bench` 使用 Qwen2.5 0.5B：处理 prompt 时 GPU（Venus）达到 893–1.365 tokens/s，CPU 为 193–264。
详情及数据解读见 [GPU](/advanced/gpu)。

## 性能得益于哪些设计 {#o-que-o-desempenho-deve-ao-desenho}

- **root 是只读 ext4 磁盘**，而不是 virtiofs：virtiofs 并不更快，而且无法把 guest 限制在共享目录内。
- **warmup 写入镜像：** Python 标准库的 `.pyc` 和 Go 的构建缓存都已预先生成；对于 Go，复制到 tmpfs 的操作在启动时、
  VM 进入 pool 之前完成。
- **一次性 VM 的 pool：** 启动在后台进行，每种语言最多同时 2 个。
- **machine 磁盘克隆：** 在 APFS 上使用 clonefile；新磁盘不会复制那 10 GiB。

## 待测项 {#medições-pendentes}

- Linux 上的性能（x86_64 和 aarch64，KVM）；
- 在大于 0.5B 的模型上使用 GPU 的 machine。
