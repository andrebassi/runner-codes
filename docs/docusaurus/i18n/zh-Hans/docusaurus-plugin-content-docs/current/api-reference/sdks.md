---
sidebar_position: 2
title: Go 与 Python SDK
---

# Go 与 Python SDK

[HTTP API](/api-reference/overview)（`rc serve`）的两个客户端，方法相同，只依赖标准库。
它们覆盖一次性作业以及 machine 的完整生命周期。

| SDK | 位置 | 依赖 | 覆盖率（行 / 分支） |
|-----|------|--------------|----------------------------|
| Go | `runner.code/pkg/client` | 无 | 100%（语句；Go 不统计分支） |
| Python 3.9+ | `sdk/python/runner_code` | 无 | 100% / 100% |

**没有 Node SDK。** 按照项目规则，浏览器之外的新代码不使用 JavaScript/TypeScript 编写。使用 Node 的人可以直接用
`fetch` 调用 HTTP API：[参考文档](/api-reference/overview#máquinas)中的表格列出了每条路由和每个请求体。

## Go {#go}

```go
c := client.New("http://127.0.0.1:8080")
ctx := context.Background()

r, _ := c.Execute(ctx, client.Job{Language: "python", Code: "print(6*7)"})
fmt.Print(r.Stdout) // 42

c.CreateMachine(ctx, client.MachineSpec{Name: "dev", Stack: "python", Network: "lab"})
c.Start(ctx, "dev")
c.WriteFile(ctx, "dev", "/root/app.py", []byte("print('ola')\n"), 0o644)
out, _ := c.Exec(ctx, "dev", []string{"python3", "/root/app.py"}, client.ExecOptions{Timeout: 30})
fmt.Print(out.Stdout, out.ExitCode)

_, err := c.Machine(ctx, "nao-existe")
var e *client.Error
if errors.As(err, &e) {
	fmt.Println(e.Status, e.Code) // 404 NOT_FOUND
}
c.DeleteMachine(ctx, "dev", true)
```

完整示例：`examples/sdk-go/main.go`。

## Python {#python}

```python
from runner_code import Client, Job, RunnerCodeError

c = Client("http://127.0.0.1:8080")
print(c.execute(Job(language="python", code="print(6*7)")).stdout)  # 42

c.create_machine("dev", stack="python", network="lab", egress={"allow_hosts": ["pypi.org"]})
c.start("dev")
c.write_file("dev", "/root/app.py", b"print('ola')\n")
r = c.exec("dev", ["python3", "/root/app.py"], timeout=30)
print(r.stdout, r.exit_code)

try:
    c.machine("nao-existe")
except RunnerCodeError as e:
    print(e.status, e.code)  # 404 NOT_FOUND
c.delete_machine("dev", force=True)
```

无需安装即可使用：`PYTHONPATH=sdk/python`。完整示例：`examples/sdk-python/main.py`。

## 方法 {#métodos}

| Go | Python | 路由 |
|----|--------|------|
| `Execute` | `execute` | `POST /api/v1/execute` |
| `CreateMachine` | `create_machine` | `POST /api/v1/machines` |
| `Machines` / `Machine` | `machines` / `machine` | `GET /api/v1/machines[/:name]` |
| `Start` / `Stop` | `start` / `stop` | `POST .../start`、`.../stop?timeout=N` |
| `Exec` | `exec` | `POST .../exec` |
| `WriteFile` / `ReadFile` | `write_file` / `read_file` | `PUT`/`GET .../files?path=` |
| `Logs` | `logs` | `GET .../logs` |
| `UpdateEgress` | `update_egress` | `PUT .../egress` |
| `Pause` / `Resume` / `Branch` | `pause` / `resume` / `branch` | `POST .../pause`、`.../resume`、`.../branch` |
| `DeleteMachine` | `delete_machine` | `DELETE /api/v1/machines/:name[?force=true]` |

## 值得了解的细节 {#o-que-vale-saber}

- **API 错误是有类型的**（`*client.Error`、`RunnerCodeError`），包含 HTTP 状态码、machine 路由的稳定错误码
  （`NOT_FOUND`、`INVALID_STATE`、`CONFIG`...）以及消息。作业路由没有错误码：该字段为空。传输层故障（服务器不可用）
  以普通错误返回，而不是 API 错误。
- **空字段不会放进请求体。** 在 create 中，服务器把出现的字段当作对 `runner_code` 的覆盖；发送 `cpus: 0` 会抹掉文件
  中的值。
- **`Stop`**：负的超时（Go）或 `None`（Python）表示使用 machine 的 `stop_timeout`，`0` 表示立即杀掉。
- **作业中的 `status: "success"`** 表示 API 执行了它，而不是程序通过了。程序的结果在 `exception`（`Timeout`、
  `Compilation failed`）和 `stderr` 中。
- machine 名在路径中会被转义（`a/b` 变为 `a%2Fb`）。

## 测试 {#testes}

```bash
task test:cov      # 包含 pkg/client（下限 90%）和 Python SDK
task sdk:py:cov    # 仅 Python：行和分支各自 >= 90%，通过 nix shell 使用 coverage.py
```

两个 SDK 都经过了 12 个刻意植入的缺陷检验（名字转义、stop 的 query、错误读取、请求体中的空字段、八进制模式...）。
测试全部 12 个都抓到了。runbook 的第 10b 步会针对真实的 `rc serve`、用真实 VM 运行这两个示例。
