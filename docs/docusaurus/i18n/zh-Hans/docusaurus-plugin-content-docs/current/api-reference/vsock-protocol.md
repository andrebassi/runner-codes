---
title: 'vsock 协议'
description: 'host 与 guest 之间“长度 + JSON”的 frame，逐字节说明'
---

## 传输 {#transporte}

| 项目 | 值 |
|------|-------|
| guest 端 | 监听 vsock 端口 5000 |
| host 端 | Unix socket，通过 `krun_add_vsock_port2`（`listen=true`）绑定到端口 5000 |
| 握手 | 无：连上 Unix socket 就直接到达 guest |
| 每个连接的消息 | 一个作业和一个结果 |

## Frame {#frame}

![Message framing](/img/message-framing.svg)

```
offset 0      4                          4 + N
       ┌──────┬──────────────────────────┐
       │  N   │  JSON (N bytes, UTF-8)   │
       └──────┴──────────────────────────┘
       uint32 big-endian
```

- `N` 只计算 payload。
- frame 最大 64 MiB。host 的读取方在分配内存之前就拒绝超过这个大小的长度：没有这个限制，一个损坏的前缀（最大 4 GiB）会让读取方在读取正文之前就把内存全部分配出去。
- host 用一次写操作发送前缀和正文，保证 guest 永远不会读到没有对应正文的前缀。

## 示例 {#exemplo}

作业 `{"trace_id":"t1","lang":"python","code":"print(1)","timeout":10}` 的 JSON 有 64 字节：

```
00 00 00 40  7b 22 74 72 61 63 65 5f 69 64 22 3a ...
└─ 64 ─────┘ └─ {"trace_id": ...
```

## Payload {#payloads}

- host → guest：[Job request](/api-reference/job-request)。
- guest → host：[Job response](/api-reference/job-response)。

## Streaming {#streaming}

作业中带 `"stream": true` 时，guest 会用多个 frame 而不是一个来响应，格式全部相同（长度 + JSON），定义在
`internal/protocol` 中：

```json
{"type":"stdout","data":"early\n"}
{"type":"stderr","data":"warn\n"}
{"type":"result","result":{"status":"success","stdout":"early\n","stderr":"warn\n","exception":null,"executionTime":12}}
```

- 进程向管道的每次写入对应一个 frame（Linux 上最大 32 KiB），按 guest 读取的顺序发送。
- `result` 总是最后一个，携带完整结果，与非 streaming 模式相同。
- 不带 `stream` 时，线路上的内容与以前完全相同：一个结果 frame。就绪探测（`sh true`）使用这种模式。
- 如果 host 收到一个没有 `type` 的 frame，说明镜像中的 guest 早于 streaming 功能：`rc image build` 会重建镜像。

## 操作 {#operações}

作业的 `op` 字段选择操作。为空时是一直以来的代码作业，这样一次性作业的线路格式不会改变；其余操作服务于持久化
machine（`internal/protocol`）。

| `op` | 使用的字段 | 作用 |
|------|---------------|-----------|
| （空） | `lang`、`code`、`files`、`stdin`、`timeout`、`stream` | 运行某种语言的代码 |
| `exec` | `argv`、`env`、`cwd`、`user`、`timeout`、`stream` | 运行一个程序，没有隐式 shell |
| `put` / `get` | `path`、`offset`、`data`、`final`、`mode` | 以 1 MiB 分块复制文件（每个文件最大 4 GiB） |
| `pty` | `argv`、`rows`、`cols`、`user` | 交互式终端：连接变为事件通道，host 发送 `input`/`resize`，guest 发送 `output`/`exit` |
| `workload` | `argv`、`env`、`cwd`、`user`、`stop_on_exit` | 启动 machine 持续运行的进程 |
| `shutdown` | `signal`、`timeout` | 向 workload 发送信号，等待，执行 `sync` 后关机 |
| `dial` | `port` | 成功结果之后，连接变成一条通往 guest 内 `127.0.0.1:port` 的字节管道（发布的端口） |
| `set-env` | `env` | 把 secrets 保存在 guest 内存中，并附加到之后的每条命令上；从不写入磁盘 |
| `ssh-agent` | `path`、`port` | 在 guest 中打开一个 Unix socket，把每个连接转发到 host 的 vsock 端口（SSH agent） |
| `mount-s3` | `path`、`s3`、`env` | 确认 bucket 有响应后，在 guest 中通过 FUSE 挂载它 |

## 其他 vsock 端口 {#outras-portas-vsock}

| 端口 | 方向 | 用途 |
|-------|---------|-----|
| 5000 | host → guest | 上述所有操作 |
| 5001 | guest → host | SSH agent（`--ssh-agent`） |
| 5100 + i | guest → host | 第 i 个 `--mount-socket` |
