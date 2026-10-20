---
title: 'vsock 通信'
description: 'host 如何与 guest 通信：Unix socket、vsock 端口 5000 和“长度 + JSON”的 frame'
---

## 通道 {#o-canal}

host 与 guest 之间的通道是 vsock（作业 VM 甚至没有网络）：

- guest 监听 vsock 端口 **5000**；
- 在 host 上，`krun_add_vsock_port2` 以 `listen=true` 把这个端口绑定到缓存中某个工作目录下的一个 **Unix socket**；
- host 像连接任何本地 socket 一样连接这个 Unix socket。

没有文本握手：连上 Unix socket 就已经到达 guest。

## 消息格式 {#formato-da-mensagem}

![Message framing](/img/message-framing.svg)

两个方向上的每条消息都是：

```
+------------------------+----------------------+
| tamanho (4 bytes, BE)  | payload JSON (UTF-8) |
+------------------------+----------------------+
```

- 长度是一个 4 字节无符号整数，big-endian，只计算 payload。
- frame 最大 **64 MiB**；host 拒绝读取超过此大小的 frame。

同一种 frame 用于所有操作：一次性作业，以及 machine 的各种操作（`exec`、文件复制、终端、workload、发布的端口、
secrets、SSH agent、S3），由作业的 `op` 字段选择。

## 时序 {#sequência}

```
host                                guest
 │ connect(socket Unix) ─────────────▶ accept (vsock 5000)
 │ [tamanho][Job JSON] ──────────────▶ executa
 │ ◀────────────── [tamanho][Result JSON]
 │ fecha; a VM de job é destruída
```

## 就绪 {#prontidão}

libkrun 在 guest 开始监听之前就创建了 Unix socket，因此连接成功并不能证明 guest 已经就绪。就绪等待每 1 ms 发送一个 `sh true` 作业，直到收到响应。在此之后 VM 才会接收真正的作业。

## Payload {#payloads}

- 作业：`{trace_id, lang, code, files[{name, content}], stdin, timeout}`——见 [Job request](/api-reference/job-request)。
- 结果：`{trace_id, status, stdout, stderr, exception, executionTime, exit_code, error}`——见 [Job response](/api-reference/job-response)。

逐字节的细节见 [vsock 协议](/api-reference/vsock-protocol)。
