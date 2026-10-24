---
title: 'API 概览'
description: 'HTTP 端点、请求与响应格式、错误和限制'
---

## 基础 {#base}

API 由 `rc serve` 启动，默认监听 `http://127.0.0.1:8080`。**没有认证。** 默认只监听 loopback；没有带认证的代理挡在前面时，不要暴露到 loopback 之外。

| 方法 | 路径 | 作用 |
|--------|---------|-----------|
| GET | `/health` | 状态和版本 |
| GET | `/api/v1/languages` | 目录，以及每种语言的镜像是否就绪 |
| POST | `/api/v1/run` | 执行文件，stdin 可以是单个或批量 |
| POST | `/api/v1/execute` | 执行单独的 `code` 或文件，可按请求设置超时 |

## GET /health {#get-health}

```json
{"status": "ok", "version": "dev"}
```

`version` 是发布时写入二进制的版本；本地构建时为 `dev`。

## GET /api/v1/languages {#get-apiv1languages}

```json
[
  {"name": "python", "image": "python:3.12-alpine", "ready": true},
  {"name": "nodejs", "image": "node:22-alpine", "ready": false}
]
```

`ready` 表示该语言的 ext4 是否已在这台 host 上构建。`ready: false` 的语言会被以 E006 拒绝，就像它不在目录中一样；用 [`image build`](/cli/image) 构建镜像。

## POST /api/v1/run {#post-apiv1run}

```json
{
  "language": "python",
  "files": [{"name": "main.py", "content": "print(input())"}],
  "stdin": "olá"
}
```

| 字段 | 类型 | 必填 | 说明 |
|-------|------|-------------|-----------|
| `language` | string | 是 | 目录中的名字 |
| `files` | `{name, content}` 列表 | 是 | 第一个文件是主文件 |
| `stdin` | string 或列表 | 否 | 列表时每一项运行一次，每次都在一台新 VM 中 |

单个 stdin 的响应：

```json
{"status": "success", "stdout": "olá\n", "stderr": null, "exception": null, "executionTime": 33}
```

使用 `"stdin": ["a", "b"]` 时，响应是一个列表，每一项回显对应的 stdin：

```json
[
  {"status": "success", "stdout": "a\n", "stderr": null, "exception": null, "executionTime": 32, "stdin": "a"},
  {"status": "success", "stdout": "b\n", "stderr": null, "exception": null, "executionTime": 31, "stdin": "b"}
]
```

上面的 `executionTime` 值仅作示意。

## POST /api/v1/execute {#post-apiv1execute}

```json
{
  "language": "python",
  "code": "import time; time.sleep(2); print('ok')",
  "timeout": 5
}
```

| 字段 | 类型 | 必填 | 说明 |
|-------|------|-------------|-----------|
| `language` | string | 是 | 目录中的名字 |
| `code` | string | 否 | 没有 `files` 时主文件的代码 |
| `files` | `{name, content}` 列表 | 否 | 文件；第一个是主文件 |
| `stdin` | string | 否 | 标准输入 |
| `timeout` | 整数（秒） | 否 | 默认 10，最大 300 |

响应格式与 `/api/v1/run` 相同。

## POST /api/v1/execute/stream {#post-apiv1executestream}

请求体与 `/api/v1/execute` 相同。响应是 `text/event-stream`（Server-Sent Events）：输出**在程序运行期间**陆续到达，
由一个 `result` 事件结束响应。

```bash
curl -N -X POST http://127.0.0.1:8080/api/v1/execute/stream -H 'content-type: application/json' \
  -d '{"language":"python","code":"import time\nprint(\"early\")\ntime.sleep(1.5)\nprint(\"late\")"}'
```

```
event: stdout
data: "early\n"

event: stdout
data: "late\n"

event: result
data: {"status":"success","exitCode":0,"exception":null,"executionTime":1534,"timedOut":false}
```

| 事件 | `data` |
|--------|--------|
| `stdout`、`stderr` | 输出片段，以 **JSON 字符串**表示（换行被转义：SSE 的 `data` 中不允许原始换行） |
| `result` | `status`、`exitCode`、`exception`、`executionTime`（ms）、`timedOut`，有错误时还有 `error`。不会重复 stdout/stderr |
| `error` | `{"error": "..."}`：stream 开始之后发生的基础设施故障（例如与 guest 的连接断开） |

- 校验时被拒绝的作业在打开 stream 之前返回 **400 JSON**，与 `/execute` 相同。第一个事件之前发生的基础设施故障返回
  500 JSON。
- 此路由不接受列表形式的 `stdin`（批量）：返回 400。
- 响应头 `X-Accel-Buffering: no` 用于关闭 nginx 代理的缓冲。
- 输出为管道时 stdout 按块缓冲的语言（C、Ruby、Perl）只有在程序 `flush` 时才会交付；Python 以
  `PYTHONUNBUFFERED=1` 运行。详见 [`rc run`](/cli/run)。

## 响应字段 {#campos-da-resposta}

| 字段 | 说明 |
|-------|-----------|
| `status` | API 执行了作业时为 `"success"`。**不**代表程序成功了：请查看 `stderr` 和 `exception` |
| `stdout` | 标准输出 |
| `stderr` | 错误输出，为空时为 `null` |
| `exception` | `null`、`"Timeout"`、`"Compilation failed"` 或基础设施故障的描述 |
| `executionTime` | VM 内的执行时间，单位 ms |
| `error` | 拒绝时出现（E005、E006 或原因），以及 guest 未能按时响应时出现（E001） |
| `stdin` | 仅在批量模式下：该项对应的 stdin |

## 错误与 HTTP 状态码 {#erros-e-status-http}

| 情况 | HTTP | 响应体 |
|----------|------|-------|
| 执行完成（包括程序失败或超时） | 200 | 正常响应 |
| JSON 无效或缺少必填字段 | 400 | `{"status": "failed", "error": "requisição inválida: ..."}` |
| stdin 超过 1 MB | 400 | `"error": "E005: stdin too long"` |
| 语言不在目录中，或镜像尚未在这台 host 上构建 | 400 | `"error": "E006: unsupported language <nome>"` |
| 违反其他 domain 限制（超时不在 (0, 300 s] 内、作业既无代码也无文件、文件重复、文件名无效、文件过多、代码过大） | 400 | `"error"` 中给出原因 |
| guest 在超时加上 host 余量时间内未响应 | 200 | `{"status": "failed", "error": "E001: operation timed out"}` |
| 基础设施故障（VM 没起来、与 guest 的通道出错） | 500 | `{"status": "failed", "exception": "..."}` |

## 限制 {#limites}

| 限制 | 值 |
|--------|-------|
| stdin | ≤ 1.000.000 字节 |
| 每个作业的文件数 | 最多 32 个 |
| 代码总量（所有文件之和） | ≤ 4 MiB |
| 文件名 | 不能含 `/`、不能含 `..`、不能以 `.` 开头，≤ 255 字节 |
| 超时 | 默认 10 s，最大 300 s |

## Machine {#máquinas}

[持久化 machine](/cli/machine) 的生命周期。

| 方法 | 路径 | 请求体 / query | 响应 |
|--------|---------|---------------|----------|
| POST | `/api/v1/machines` | `{"name","stack"\|"image","cpus","memory_mib","disk_gib","gpu","gpu_vram_mib","env","workdir","init","workload","stop_signal","stop_timeout","stop_on_exit","net","ports":["8080:80"],"egress":{"allow_hosts",...},"network":"lab"}` 或 `{"name","runner_code":"<TOML>"}`（非空字段优先于文件） | 201 + machine（`state: stopped`） |
| GET | `/api/v1/machines` | — | 带 `state` 和 `pid` 的列表 |
| GET | `/api/v1/machines/:name` | — | 该 machine |
| DELETE | `/api/v1/machines/:name` | `?force=true` 运行中则直接杀掉 | 204 |
| POST | `/api/v1/machines/:name/start` | — | 200 + machine（`state: running`） |
| POST | `/api/v1/machines/:name/stop` | `?timeout=N`，单位秒（0–300；0 立即杀掉）；不传时使用 machine 的 `stop_timeout`（或 10） | 204 |
| POST | `/api/v1/machines/:name/exec` | `{"command":["argv",...],"env":["K=V"],"workdir","stdin","timeout"}` | `{"exitCode","stdout","stderr","timedOut","executionTime"}` |
| POST | `/api/v1/machines/:name/exec/stream` | 请求体同上 | SSE，**每行一个事件，原始文本**：`event: stdout` / `data: line1`；被截断到达的行会等待剩余部分，没有 `\n` 的剩余内容会在 `exit`（`{"exitCode","timedOut","executionTime"}`）之前发出 |
| PUT | `/api/v1/machines/:name/files?path=/abs[&mode=644]` | 原始请求体 = 文件内容 | `{"path","bytes"}` |
| GET | `/api/v1/machines/:name/files?path=/abs` | — | 原始内容（`application/octet-stream`） |
| PUT/GET | `/api/v1/machines/:name/files/*path` | 同上，路径放在 URL 中（`files/%2Ftmp%2Fa.txt` 或 `files/tmp/a.txt`） | 同上 |
| GET | `/api/v1/machines/:name/logs?tail=N&source=all\|workload` | — | 文本形式的控制台输出；`workload` 只过滤出 workload 的输出，不带标记 |
| PUT | `/api/v1/machines/:name/egress` | `{"allow_hosts","allow_host_patterns","allow_cidrs","deny_cidrs","block_private","allow_private"}`（完整策略） | 200 + machine；machine 运行中时，从下一次连接起生效。运行中但没有网卡时返回 `INVALID_STATE` |
| POST | `/api/v1/machines/:name/pause` | — | 204；冻结运行中的 machine（`state: paused`），在 resume 之前拒绝 exec |
| POST | `/api/v1/machines/:name/resume` | — | 204；machine 已在运行不算错误 |
| POST | `/api/v1/machines/:name/sync` | — | 204；把 staged 卷的副本写回 host（停止状态下返回 `INVALID_STATE`） |
| POST | `/api/v1/machines/:name/branch` | `{"name":"filha"}` | 201 + 子 machine（`state: stopped`），磁盘的 copy-on-write 副本；运行中的父 machine 只在克隆期间被冻结 |
| GET | `/api/v1/machines/:name/screen` | — | 200，屏幕为 JPEG；尺寸在 `X-Screen-Width` 和 `X-Screen-Height` 中（带 display 的 machine） |
| POST | `/api/v1/machines/:name/screen/actions` | `{"actions":[{"action":"click","x":10,"y":20}]}` | 204；[屏幕控制](/cli/screen)的动作（click、move、drag、type、key、scroll、wait），最多 500 个 |

```bash
curl -s -X POST localhost:8080/api/v1/machines -d '{"name":"dev","stack":"python"}'
curl -s -X POST localhost:8080/api/v1/machines/dev/start
curl -s -X POST localhost:8080/api/v1/machines/dev/exec -d '{"command":["python3","-c","print(42)"]}'
# {"exitCode":0,"stdout":"42\n","stderr":"","timedOut":false,"executionTime":31}
curl -s -X PUT --data-binary @app.py 'localhost:8080/api/v1/machines/dev/files?path=/workspace/app.py'
```

`command` 是一个 argv，**没有隐式 shell**。要使用 shell，请发送 `["sh","-c","..."]`。

create 接受的替代写法：

| 替代字段 | 等价于 |
|---|---|
| `"network": true` / `false` | `net`（字符串仍表示网络名） |
| `"memoryMb"` | `memory_mib` |
| `"cmd"` | `workload` |
| `"allowedHosts"`、`"allowedCidrs"` | `egress.allow_hosts`、`egress.allow_cidrs`，并开启网络 |
| `"nestedVirt"`（或 `"nested"`） | `--nested`；除非服务器以 `rc serve --allow-nested-virt` 启动，否则以 400 拒绝 |

在 create、exec、egress 和 branch 路由中，未知字段会以 400 **拒绝**（`CONFIG_ERROR`，消息中会指出字段名）。如果悄悄
忽略，一个拼错的 `allowedHost` 就会创建出没有出站限制的 machine。

**作业**的 stream（`/api/v1/execute/stream`）没有变化：那里的 `data` 仍是 JSON 字符串，因为片段中可能带有不含换行的
字节。

machine 路由的错误：

```json
{"error":{"code":"INVALID_STATE","message":"INVALID_STATE: machine dev is not running; start it first"},"code":"INVALID_STATE"}
```

`code` 也出现在响应体顶层，方便在那里查找它的客户端。

| 代码 | HTTP |
|--------|------|
| `NOT_FOUND` | 404 |
| `CONFLICT`、`INVALID_STATE` | 409 |
| `CONFIG_ERROR` | 400 |
| `RUNNER_ERROR`（基础设施） | 500 |

## 内部协议 {#protocolo-interno}

在 host 与 guest 之间，作业以“长度 + JSON”的 frame 通过 vsock 传输。见 [Job request](/api-reference/job-request)、[Job response](/api-reference/job-response) 和 [vsock 协议](/api-reference/vsock-protocol)。
