---
sidebar_position: 2
title: Go and Python SDKs
---

# Go and Python SDKs

Two clients for the [HTTP API](/api-reference/overview) (`rc serve`), with the same methods and only the standard
library. They cover the ephemeral job and the full machine lifecycle.

| SDK | Where | Dependencies | Coverage (line / branch) |
|-----|-------|--------------|--------------------------|
| Go | `runner.codes/pkg/client` | none | 100% (statement; Go does not measure branches) |
| Python 3.9+ | `sdk/python/runner_code` | none | 100% / 100% |

**Node is left out.** By project rule, new code is not written in JavaScript/TypeScript outside the browser. Node users
call the HTTP API directly with `fetch`: the table in the [reference](/api-reference/overview#machines) has every route
and every body.

## Go

In your own project, outside this repository:

```bash
go get runner.codes/pkg/client@latest
```

`runner.codes` answers `go get` by pointing to the GitHub repository, so there is no clone and no `replace`. The package
imports only the standard library; the module's `go.mod` asks for Go 1.27.2, and an older toolchain downloads it on its own.

```go
import "runner.codes/pkg/client"

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

Full example: `examples/sdk-go/main.go`.

## Python

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

To use it without installing: `PYTHONPATH=sdk/python`. Full example: `examples/sdk-python/main.py`.

## Methods

| Go | Python | Route |
|----|--------|-------|
| `Execute` | `execute` | `POST /api/v1/execute` |
| `CreateMachine` | `create_machine` | `POST /api/v1/machines` |
| `Machines` / `Machine` | `machines` / `machine` | `GET /api/v1/machines[/:name]` |
| `Start` / `Stop` | `start` / `stop` | `POST .../start`, `.../stop?timeout=N` |
| `Exec` | `exec` | `POST .../exec` |
| `WriteFile` / `ReadFile` | `write_file` / `read_file` | `PUT`/`GET .../files?path=` |
| `Logs` | `logs` | `GET .../logs` |
| `UpdateEgress` | `update_egress` | `PUT .../egress` |
| `Pause` / `Resume` / `Branch` | `pause` / `resume` / `branch` | `POST .../pause`, `.../resume`, `.../branch` |
| `DeleteMachine` | `delete_machine` | `DELETE /api/v1/machines/:name[?force=true]` |

## Good to know

- **API errors are typed** (`*client.Error`, `RunnerCodeError`), with the HTTP status, the stable code of the machine
  routes (`NOT_FOUND`, `INVALID_STATE`, `CONFIG`...) and the message. The job routes have no code: it stays empty. A
  transport failure (server down) arrives as a regular error, not an API error.
- **Empty fields are not sent in the body.** On create, the server treats a present field as an override of
  `runner_code`; sending `cpus: 0` would wipe the value from the file.
- **`Stop`**: a negative timeout (Go) or `None` (Python) uses the machine's `stop_timeout`, and `0` kills immediately.
- **`status: "success"` on a job** means the API ran it, not that the program passed. The program's result is in
  `exception` (`Timeout`, `Compilation failed`) and in `stderr`.
- The machine name is escaped in the path (`a/b` becomes `a%2Fb`).

## Tests

```bash
task test:cov      # includes pkg/client (90% floor) and the Python SDK
task sdk:py:cov    # Python only: line and branch, each >= 90%, coverage.py via nix shell
```

Both SDKs went through 12 defects planted on purpose (name escaping, the stop query, error parsing, empty field in the
body, octal mode...). The tests caught all 12. Runbook step 10b runs both examples against a real `rc serve`, with real
VMs.
