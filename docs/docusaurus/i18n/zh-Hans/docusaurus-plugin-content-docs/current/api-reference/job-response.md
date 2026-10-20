---
title: 'Job response'
description: 'guest 通过 vsock 返回给 host 的结果'
---

## 格式 {#formato}

```json
{
  "trace_id": "tr-123",
  "status": "success",
  "stdout": "42\n",
  "stderr": null,
  "exception": null,
  "executionTime": 34,
  "exit_code": 0
}
```

`executionTime` 的值仅作示意。

## 字段 {#campos}

| 字段 | 类型 | 说明 |
|-------|------|-----------|
| `trace_id` | string | 与作业中的相同 |
| `status` | string | guest 执行了作业时为 `"success"`；拒绝作业（未知语言、stdin 过大）或无法准备目录时为 `"failed"` |
| `stdout` | string | 标准输出 |
| `stderr` | string 或 `null` | 为空时为 `null` |
| `exception` | string 或 `null` | `"Timeout"`、`"Compilation failed"` 或准备阶段的错误 |
| `executionTime` | 整数 | 在 guest 内测得的毫秒数，从编译/执行开始到结束 |
| `exit_code` | 整数 | 程序的退出码；为 0 时省略 |
| `error` | string | 拒绝原因；为空时省略 |

## 如何解读结果 {#como-ler-o-resultado}

`"status": "success"` 表示执行发生了，而不是程序成功了。

| 情况 | `status` | `exception` | `stderr` | `exit_code` |
|----------|----------|-------------|----------|-------------|
| 程序正常结束 | success | null | 程序写出的内容，或 null | 0 |
| 程序出错退出 | success | null | 错误信息 | ≠ 0 |
| 编译失败 | success | `"Compilation failed"` | 编译器输出 | — |
| 超时 | success | `"Timeout"` | 已有的输出 | — |

## API 如何处理结果 {#o-que-a-api-faz-com-o-resultado}

在 `/api/v1/run` 和 `/api/v1/execute` 中，API 转发 `status`、`stdout`、`stderr`、`exception`、`executionTime` 和 `error`；`exit_code` 不出现。它以 `exitCode` 的形式出现在 `/api/v1/execute/stream` 的 `result` 事件中，以及 machine exec 的响应中。CLI 在 `rc run` 的摘要行中显示它，且当它不为零时以 rc 1 退出；`rc machine exec` 则以命令本身的退出码退出。

如果 guest 在作业超时加上 host 的余量时间内没有响应，VM 会被杀掉，API 返回 `"status": "failed"` 和 `"error": "E001: operation timed out"`。
