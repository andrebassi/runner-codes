---
title: 'HTTP API（serve）'
description: '启动带预启动 VM pool 的 HTTP API'
---

## 用法 {#uso}

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
```

| Flag | 默认值 | 说明 |
|------|--------|-----------|
| `--listen` | `127.0.0.1:8080`（或 `RUNNER_LISTEN`） | 监听地址 |
| `--pool-size` | `2` | 每门语言的就绪 VM 数量 |
| `--allow-nested-virt` | 关闭 | 在创建机器时接受 `nestedVirt`；不加此 flag 时，能访问该端口的人无法把 host 的虚拟化能力交给 guest |

启动时，`serve` 会打印地址、pool 大小以及镜像已就绪的语言。只有这些语言会进入 [pool](/concepts/pool)。

SIGINT 或 SIGTERM 会关闭服务器并杀掉所有 VM。

## 安全 {#segurança}

API **没有认证**。默认只监听 loopback。如需对其他机器开放，请在前面加一个带认证的代理；不要只是把 `--listen` 改成 `0.0.0.0`。

## Endpoint {#endpoints}

| 方法 | 路径 | 用途 |
|--------|---------|----------|
| GET | `/health` | 状态、版本以及每个 stack 的就绪 VM |
| GET | `/api/v1/languages` | 带 `ready` 的目录 |
| POST | `/api/v1/run` | 带 `files` 和 `stdin` 的 job（字符串，或列表：每项一台 VM） |
| POST | `/api/v1/execute` | 直接传入 `code`，以及 `timeout` 和 `gpu` 的 job |
| POST | `/api/v1/execute/stream` | 同上，但走 SSE：运行时输出，结束时给出 `result` |
| POST/GET/DELETE | `/api/v1/machines[/:name]` | 创建（通过字段或 `runner_code`）、列出、查看、删除机器 |
| POST | `/api/v1/machines/:name/{start,stop,pause,resume,sync,branch}` | 生命周期 |
| POST | `/api/v1/machines/:name/exec[/stream]` | 在运行中的机器上执行命令，返回 `exitCode` |
| PUT/GET | `/api/v1/machines/:name/files` | 写入和读取文件 |
| GET | `/api/v1/machines/:name/logs` | 控制台 |
| PUT | `/api/v1/machines/:name/egress` | 修改出站策略，机器运行中也可以 |

请求体、响应、错误码和限制见 [API 概览](/api-reference/overview)。现成的客户端见
[Go 与 Python SDK](/api-reference/sdks)。

## 示例 {#exemplos}

```bash
curl -s http://127.0.0.1:8080/health
```

```bash
curl -s http://127.0.0.1:8080/api/v1/languages
```

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{"language":"python","files":[{"name":"main.py","content":"print(int(input())*2)"}],"stdin":["1","2","3"]}'
```

stdin 为列表时，每一项都在一台全新的 VM 中运行，响应是一个列表，并回显对应的 stdin。

```bash
curl -s http://127.0.0.1:8080/api/v1/execute \
  -H 'Content-Type: application/json' \
  -d '{"language":"bash","code":"echo $((6*7))","timeout":5}'
```

```bash
curl -s -X POST http://127.0.0.1:8080/api/v1/machines -d '{"name":"dev","stack":"python"}'
curl -s -X POST http://127.0.0.1:8080/api/v1/machines/dev/start
curl -s -X POST http://127.0.0.1:8080/api/v1/machines/dev/exec -d '{"command":["python3","-c","print(42)"]}'
```

## 延迟 {#latência}

pool 已满时，在测量所用的机器上（macOS 27.0.1，Apple Silicon）完整的 HTTP 请求耗时 26–43 ms。pool 为空时，job 需要等待一次冷启动。
