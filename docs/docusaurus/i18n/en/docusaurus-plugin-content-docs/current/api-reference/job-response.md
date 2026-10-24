---
title: 'Job response'
description: 'The result the guest returns to the host over vsock'
---

## Format

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

The `executionTime` value is illustrative.

## Fields

| Field | Type | Description |
|-------|------|-------------|
| `trace_id` | string | the same as the job's |
| `status` | string | `"success"` when the guest ran the job; `"failed"` when it rejected it (unknown language, stdin too large) or could not prepare the directory |
| `stdout` | string | standard output |
| `stderr` | string or `null` | `null` when empty |
| `exception` | string or `null` | `"Timeout"`, `"Compilation failed"` or a preparation error |
| `executionTime` | integer | ms measured inside the guest, from the start of compilation/execution to the end |
| `exit_code` | integer | the program's exit code; omitted when 0 |
| `error` | string | rejection reason; omitted when empty |

## How to read the result

`"status": "success"` says the execution happened, not that the program succeeded.

| Situation | `status` | `exception` | `stderr` | `exit_code` |
|-----------|----------|-------------|----------|-------------|
| program finished fine | success | null | what the program wrote, or null | 0 |
| program exited with an error | success | null | the error | ≠ 0 |
| compilation failure | success | `"Compilation failed"` | compiler output | — |
| timeout exceeded | success | `"Timeout"` | whatever there is | — |

Go is the exception on the compilation row: it runs through `go run`, which compiles and executes in one step, so a
compilation error arrives only in `stderr`, with `exception` null.

## What the API does with the result

In `/api/v1/run` and `/api/v1/execute`, the API passes through `status`, `stdout`, `stderr`, `exception`, `executionTime` and `error`; `exit_code` does not appear. It appears as `exitCode` in the `result` event of `/api/v1/execute/stream` and in the machine exec response. The CLI shows it on the summary line of `rc run` and exits with rc 1 when it is non-zero; `rc machine exec` exits with the command's own code.

If the guest does not respond within the job's timeout plus a host grace period, the VM is killed and the API returns `"status": "failed"` with `"error": "E001: operation timed out"`.
