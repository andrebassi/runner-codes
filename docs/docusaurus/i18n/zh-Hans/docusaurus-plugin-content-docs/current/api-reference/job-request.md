---
title: 'Job request'
description: 'host 通过 vsock 发送给 guest 的作业'
---

## 格式 {#formato}

host 把 HTTP 请求（或 CLI 调用）转换为一个作业，并以 vsock frame（4 字节 big-endian 长度 + JSON）的形式发送给 guest。

```json
{
  "trace_id": "tr-123",
  "lang": "python",
  "code": "",
  "files": [
    {"name": "main.py", "content": "import util\nprint(util.dobro(int(input())))"},
    {"name": "util.py", "content": "def dobro(x):\n    return 2 * x\n"}
  ],
  "stdin": "21",
  "timeout": 10
}
```

## 字段 {#campos}

| 字段 | 类型 | 说明 |
|-------|------|-----------|
| `trace_id` | string | 作业标识，会在结果中原样返回 |
| `lang` | string | 语言；必须存在于 guest 的执行器中 |
| `code` | string | 没有 `files` 时主文件的代码 |
| `files` | `{name, content}` 列表 | 作业的文件；**第一个**是主文件 |
| `stdin` | string | 标准输入；为空时省略 |
| `timeout` | 整数 | 秒 |
| `stream` | 布尔 | 要求在结果之前分块返回输出；见 [Streaming](/api-reference/vsock-protocol#streaming) |
| `op` | string | 操作；为空表示代码作业。machine 的操作（`exec`、`put`、`pty`...）见 [vsock 协议](/api-reference/vsock-protocol#operações) |

## 主文件 {#arquivo-principal}

- 有 `files` 时，每个文件按名字写入 `/tmp` 下的一个临时目录，第一个文件会被执行（或编译）。
- 只有 `code` 时，guest 写入 `script.<ext>`——Java 例外，会写成 `Main.java`，因为执行的类是 `Main`。

## 作业到达 guest 之前应用的规则 {#regras-aplicadas-antes-de-o-job-chegar-ao-guest}

host 上的 domain 层在以下情况下拒绝作业（API 返回 HTTP 400）：

| 规则 | 限制 |
|-------|--------|
| 语言 | 必须在目录中，且镜像已在这台 host 上构建（否则 E006） |
| stdin | ≤ 1.000.000 字节（否则 E005） |
| timeout | > 0 且 ≤ 300 s；缺省为 10 s |
| 内容 | `code` 或至少一个文件 |
| 文件 | 最多 32 个，不能重名 |
| 总大小 | `code` 与各文件内容之和 ≤ 4 MiB |
| 文件名 | 不能含 `/`、不能含 `..`、不能以 `.` 开头，≤ 255 字节 |

文件名规则防止在 guest 中写到作业目录之外，即使 VM 是一次性的。

## 就绪 {#prontidão}

在第一个真实作业之前，host 会反复（每 1 ms）向 VM 发送 `sh true` 作业，直到收到响应。host 由此得知 guest 已经在监听。
