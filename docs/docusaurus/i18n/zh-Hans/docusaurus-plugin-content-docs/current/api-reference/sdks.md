---
sidebar_position: 2
title: Go 与 Python SDK
---

# Go 与 Python SDK

[HTTP API](/api-reference/overview)（`rc serve`）的两个客户端，方法相同，只依赖标准库。
它们覆盖一次性 job 以及 machine 的完整生命周期。

| SDK | 包 | 依赖 | 覆盖率（行 / 分支） |
|-----|------|--------------|----------------------------|
| Go | `runner.codes/pkg/client` | 无 | 100%（语句；Go 不统计分支） |
| Python 3.9+ | `runner_code`（发行包 `runner-code`） | 无 | 100% / 100% |

**没有 Node SDK。** 按照项目规则，浏览器之外的新代码不使用 JavaScript/TypeScript 编写。使用 Node 的人可以直接用
`fetch` 调用 HTTP API：[参考文档](/api-reference/overview#máquinas)中的表格列出了每条路由和每个请求体。

SDK 连接的是一个已经在运行的 `rc serve`，可以在你的机器上，也可以在服务器上：

```bash
rc serve --listen 127.0.0.1:8080
```

## 安装 {#instalar}

### Go {#go}

```bash
go get runner.codes/pkg/client@latest
```

`runner.codes` 会把 `go get` 指向 GitHub 上的仓库，因此无需克隆，也无需 `replace`。该包只导入标准库。模块要求
Go 1.27.2：`go get` 会把你的 `go.mod` 中的 `go` 行提升到该版本，较旧的 toolchain 会自动下载新版本。

### Python {#python}

```bash
pip install "git+https://github.com/andrebassi/runner-codes@v1.1.1#subdirectory=sdk/python"
```

安装 `runner_code` 包，没有任何依赖。不安装的话，在仓库的克隆中设置 `PYTHONPATH=sdk/python` 即可。

## 第一个程序 {#primeiro-programa}

一个一次性 job：代码在一台全新的 microVM 中运行，没有网络，结束后即被销毁。

```go title="Go"
package main

import (
	"context"
	"fmt"
	"log"

	"runner.codes/pkg/client"
)

func main() {
	c := client.New("http://127.0.0.1:8080")
	r, err := c.Execute(context.Background(), client.Job{Language: "python", Code: "print(6*7)"})
	if err != nil {
		log.Fatal(err) // API 没有执行：服务器不可用、400、500
	}
	fmt.Print(r.Stdout) // 42
}
```

```python title="Python"
from runner_code import Client, Job

c = Client("http://127.0.0.1:8080")
r = c.execute(Job(language="python", code="print(6*7)"))
print(r.stdout, end="")  # 42
```

## 示例 {#receitas}

下面每个示例都是 `examples/sdk-go-recipes/main.go` 和 `examples/sdk-python-recipes/main.py` 中的一个函数。这两个程序
在 runbook 的第 10d 步针对真实的 `rc serve` 运行全部七个示例。每个示例中展示的输出就是在该步骤中实测的结果。

```bash
go run ./examples/sdk-go-recipes http://127.0.0.1:8080
PYTHONPATH=sdk/python python3 examples/sdk-python-recipes/main.py http://127.0.0.1:8080
```

### 1. 运行外部代码（例如由 LLM 生成的代码） {#1-rodar-código-de-fora-gerado-por-um-llm-por-exemplo}

`status: "success"` 只表示 API 执行了这个请求。程序是否成功要看 `exception`（成功时为空）和 `stderr`。

```go title="Go"
code := "import sys\nprint(sum(range(10)))\nprint('aviso', file=sys.stderr)"
r, err := c.Execute(ctx, client.Job{Language: "python", Code: code, Timeout: 10})
if err != nil {
	return err // 传输层错误或 4xx/5xx：API 没有执行
}
if r.Exception != "" {
	return fmt.Errorf("o programa falhou: %s", r.Exception)
}
fmt.Printf("stdout=%s stderr=%s tempo=%dms\n",
	strings.TrimSpace(r.Stdout), strings.TrimSpace(r.Stderr), r.ExecutionTime)
```

```python title="Python"
code = "import sys\nprint(sum(range(10)))\nprint('aviso', file=sys.stderr)"
r = c.execute(Job(language="python", code=code, timeout=10))
if r.exception:
    raise RuntimeError(f"o programa falhou: {r.exception}")
print(f"stdout={r.stdout.strip()} stderr={r.stderr.strip()} tempo={r.execution_time}ms")
```

输出：`stdout=45 stderr=aviso tempo=58ms`（时间会变化；pool 满时在 26 到 69 ms 之间）。

### 2. 多个文件与标准输入 {#2-vários-arquivos-e-entrada-padrão}

第一个文件是入口；其他文件位于同一目录，可以被导入。

```go title="Go"
r, err := c.Execute(ctx, client.Job{
	Language: "python",
	Files: []client.File{
		{Name: "main.py", Content: "import util, sys\nprint(util.dobro(int(sys.stdin.read())))"},
		{Name: "util.py", Content: "def dobro(n):\n    return 2 * n"},
	},
	Stdin: "21",
})
```

```python title="Python"
r = c.execute(Job(
    language="python",
    files=[
        {"name": "main.py", "content": "import util, sys\nprint(util.dobro(int(sys.stdin.read())))"},
        {"name": "util.py", "content": "def dobro(n):\n    return 2 * n"},
    ],
    stdin="21",
))
```

输出：`42`。

### 3. 时限 {#3-prazo}

超过 `Timeout`（秒）后，整台 VM 都会被终止。结果照常返回，带有 `exception: "Timeout"`，而不是作为 SDK 错误。

```go title="Go"
r, _ := c.Execute(ctx, client.Job{Language: "python", Code: "while True: pass", Timeout: 2})
fmt.Println(r.Exception) // Timeout
```

```python title="Python"
r = c.execute(Job(language="python", code="while True: pass", timeout=2))
print(r.exception)  # Timeout
```

### 4. 编译错误 {#4-erro-de-compilação}

在编译型语言（C、C++、Rust、Java、Kotlin...）中，编译失败会返回 `exception: "Compilation failed"`，编译器的消息在
`stderr` 中。

```go title="Go"
r, _ := c.Execute(ctx, client.Job{Language: "c", Code: "int main(void) { return x; }"})
fmt.Println(r.Exception, r.Stderr != "") // Compilation failed true
```

```python title="Python"
r = c.execute(Job(language="c", code="int main(void) { return x; }"))
print(r.exception, bool(r.stderr))  # Compilation failed True
```

**Go 是例外：** 它通过 `go run` 运行，一步完成编译和执行。编译错误只出现在 `stderr` 中，`exception` 为空。

### 5. 带过滤网络的持久 machine {#5-máquina-persistente-com-rede-filtrada}

machine 在一条命令与下一条命令之间保持运行。这里对外网的访问只允许 `pypi.org`：同一个程序可以访问它，访问
`example.com` 则被拒绝。

```go title="Go"
name := "minha-maquina"
_, err := c.CreateMachine(ctx, client.MachineSpec{
	Name:   name,
	Stack:  "python",
	Net:    true,
	Egress: &client.Egress{AllowHosts: []string{"pypi.org"}},
})
if err != nil {
	return err
}
defer c.DeleteMachine(ctx, name, true) // force：即使仍在运行也删除，以防中途出错

c.Start(ctx, name)
c.WriteFile(ctx, name, "/work/app.py", []byte(script), 0o644)
out, err := c.Exec(ctx, name, []string{"python3", "app.py"},
	client.ExecOptions{Env: []string{"QUEM=sdk-go"}, Workdir: "/work", Timeout: 30})
if err == nil && out.ExitCode != 0 {
	err = fmt.Errorf("exit %d: %s", out.ExitCode, out.Stderr)
}
data, _ := c.ReadFile(ctx, name, "/work/saida.txt")
```

```python title="Python"
name = "minha-maquina"
c.create_machine(name, stack="python", net=True, egress={"allow_hosts": ["pypi.org"]})
try:
    c.start(name)
    c.write_file(name, "/work/app.py", script.encode())
    out = c.exec(name, ["python3", "app.py"], env=["QUEM=sdk-python"], workdir="/work", timeout=30)
    if out.exit_code != 0:
        raise RuntimeError(f"exit {out.exit_code}: {out.stderr}")
    print(c.read_file(name, "/work/saida.txt").decode())
finally:
    c.delete_machine(name, force=True)
```

`script` 尝试打开 `https://pypi.org/simple/` 和 `https://example.com`，并把每个的状态写入 `saida.txt`。
输出：`sdk-go pypi=200 example=bloqueado`。

`Exec` 在 `ExitCode`/`exit_code` 中返回进程的退出码：与 job 不同，machine 内失败的命令**不会**变成 SDK 错误。

### 6. Branch {#6-branch}

连同磁盘复制一台 machine。父 machine 会先被暂停，以保证磁盘一致；子 machine 带着父 machine 已写入的内容诞生，之后
各自独立运行。

```go title="Go"
c.WriteFile(ctx, "mae", "/root/estado.txt", []byte("feito antes do branch"), 0o644)
c.Pause(ctx, "mae")
c.Branch(ctx, "mae", "filha")
c.Start(ctx, "filha")
data, _ := c.ReadFile(ctx, "filha", "/root/estado.txt")
```

```python title="Python"
c.write_file("mae", "/root/estado.txt", b"feito antes do branch")
c.pause("mae")
c.branch("mae", "filha")
c.start("filha")
print(c.read_file("filha", "/root/estado.txt").decode())
```

输出：`feito antes do branch`。

### 7. API 错误 {#7-erros-da-api}

API 的拒绝以有类型的形式返回，包含 HTTP 状态码和一个稳定的错误码。请按错误码比较，永远不要按消息文本比较。

```go title="Go"
_, err := c.Machine(ctx, "nao-existe")
var apiErr *client.Error
if errors.As(err, &apiErr) && apiErr.Code == "NOT_FOUND" {
	fmt.Println(apiErr.Status, apiErr.Code) // 404 NOT_FOUND
}
```

```python title="Python"
try:
    c.machine("nao-existe")
except RunnerCodeError as e:
    if e.code == "NOT_FOUND":
        print(e.status, e.code)  # 404 NOT_FOUND
```

machine 路由的错误码有 `NOT_FOUND`、`INVALID_STATE`、`CONFLICT`、`CONFIG`，其余见
[参考文档](/api-reference/overview#máquinas)。传输层故障（服务器不可用）以普通错误返回，而不是 API 错误。

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
| `Screenshot` | `screenshot` | `GET .../screen`（JPEG 和尺寸） |
| `ScreenActions` / `Click` / `Type` / `Key` | `screen_actions` / `click` / `type` / `key` | `POST .../screen/actions`（[屏幕控制](/cli/screen)） |
| `DeleteMachine` | `delete_machine` | `DELETE /api/v1/machines/:name[?force=true]` |

## 值得了解的细节 {#o-que-vale-saber}

- **空字段不会放进请求体。** 在 create 中，服务器把出现的字段当作对 `runner_code` 的覆盖；发送 `cpus: 0` 会抹掉文件
  中的值。
- **`Stop`**：负的 timeout（Go）或 `None`（Python）表示使用 machine 的 `stop_timeout`，`0` 表示立即杀掉。
- **客户端时限**：两者都最多等待 10 分钟的响应（Go：`http.Client` 的 `Timeout` 为 10 分钟；Python：600 s），高于
  一个 job 最多可持续的 300 s，以便服务器先返回 job 的 `Timeout`。在 Go 中，时限更短的 `context` 优先；`Client` 的
  `HTTP` 字段可以接受另一个 `http.Client`。
- machine 名在路径中会被转义（`a/b` 变为 `a%2Fb`）。

## 测试 {#testes}

```bash
task test:cov      # 包含 pkg/client（下限 90%）和 Python SDK
task sdk:py:cov    # 仅 Python：行和分支各自 >= 90%，通过 nix shell 使用 coverage.py
```

两个 SDK 都经过了 12 个刻意植入的缺陷检验（名字转义、stop 的 query、错误读取、请求体中的空字段、八进制模式...）。
测试全部 12 个都抓到了。在 runbook 中，第 10b 步运行基础示例，第 10c 步在外部项目中通过公开的 `go get` 下载 Go SDK，
第 10d 步运行七个示例，全部针对真实的 `rc serve`、使用真实 VM。
