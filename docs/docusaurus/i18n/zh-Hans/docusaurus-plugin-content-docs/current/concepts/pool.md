---
title: 'VM pool'
description: '按语言预启动的 VM，只使用一次，并在后台补充'
---

## 用途 {#para-que-serve}

启动一台 VM 并等待 guest 响应大约要 260 ms（实测 p50）。有了 pool，这笔开销就移出了请求路径：API 直接取用一台已经就绪的 VM。

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
```

## 规则 {#regras}

- **每种语言 N 台 VM。** `--pool-size`（默认 2）决定每种已构建镜像的语言准备多少台 VM。没有镜像的语言不进入 pool。
- **一次性使用。** 每台 VM 处理一个作业后即被销毁。任何状态——`/tmp` 中的文件、进程、变量——都不会从一个作业传到下一个。
- **后台补充。** 一台 VM 离开 pool 后，紧接着启动另一台，每种语言最多同时启动 2 台。
- **从不拒绝。** 如果该语言的 pool 为空，作业会当场启动一台 VM（冷启动）并等待它。
- **关闭。** SIGINT 或 SIGTERM 会关闭服务器并杀掉 pool 中的所有 VM。

## 什么叫“就绪” {#o-que-é-pronta}

连上 Unix socket 并不够：libkrun 会在 guest 开始监听之前就创建 socket。就绪等待每 1 ms 发送一个 `sh true` 作业，直到收到响应。只有这时 VM 才进入 pool。

对于 Go，把预热的 `GOCACHE` 复制到 tmpfs 也在启动时、VM 进入 pool 之前完成，不会拖慢作业。

## 内存开销 {#custo-em-memória}

一台空闲 VM 实测 footprint 为 44 MB。报告的 RSS（172 MB）包含共享的干净页。libkrun 的内存是弹性的——host 只为 guest 实际触及的部分付出代价——因此目录中该语言的 RAM 上限并不会为 pool 中的每台 VM 预留。

## 数据 {#números}

| 路径 | 实测 |
|---------|--------|
| pool 已满时的 API | 完整 HTTP 请求 26–43 ms |
| CLI，冷启动 | 总计约 240 ms |

macOS 27.0.1，Apple Silicon，2026/10/19。Linux 尚未测量。

## CLI 不使用 pool {#a-cli-não-usa-pool}

`rc run` 总是启动一台新 VM（pool 大小为零），并在结束时销毁它。
