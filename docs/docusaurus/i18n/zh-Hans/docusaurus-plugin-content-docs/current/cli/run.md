---
title: 'run 与 shell'
description: '通过命令行在全新 microVM 中执行代码或打开 shell'
---

## 用法 {#uso}

```bash
rc run --stack <linguagem> (--code '<código>' | --file <arquivo>) [--stdin '<texto>'] [--timeout 5s]
```

| Flag | 默认值 | 说明 |
|------|--------|-----------|
| `--stack` | — | 目录中的语言 |
| `--code` | — | 主文件的代码 |
| `--file` | — | 从本地文件读取代码（替代 `--code`） |
| `--stdin` | 空 | 程序的标准输入 |
| `--timeout` | `10s` | Go 格式的时长（`500ms`、`5s`、`2m`）；最大 300 s |
| `--gpu` | 关闭 | 通过 Vulkan（virtio-gpu/Venus）让 VM 使用 host 的 GPU；仅限目录中带 `gpu` 的 stack，否则返回 E007。参见 [GPU](/advanced/gpu) |

## 执行过程 {#o-que-acontece}

`run` 不使用 pool：它启动一台全新的 VM（冷启动），执行 job，然后销毁 VM。在测量所用的机器上，一个 Python job 总计约 240 ms，其中执行 31–38 ms。

输出以 **streaming** 方式呈现：程序运行时，每一段输出都会立即出现在终端上，而不是等到结束。

- 程序的 stdout 输出到 stdout。
- 程序的 stderr 输出到 stderr，最后附加一行摘要：

```
[status=success exit=0 exec=33ms total=241ms timeout=false]
```

（数值仅作示意）

### 语言的缓冲 {#buffer-da-linguagem}

streaming 传递的是程序**写入**管道的内容。当输出不是终端时，很多语言会把 stdout 缓存在
缓冲区里，直到结束（或每满一块）才真正写出：

| 语言 | streaming 时的行为 |
|-----------|----------------------------|
| Python | 逐行：streaming 模式下 runner 会设置 `PYTHONUNBUFFERED=1` |
| Node.js、Go、Bash、sh | 逐行，无需配置 |
| C、C++（`printf`）、Ruby、Perl | 按块：在程序中调用 `fflush(stdout)`、`$stdout.sync = true` 或 `$\| = 1` |
| stderr | 所有语言都不缓冲 |

## 退出码 {#código-de-saída}

| 情形 | rc |
|----------|----|
| 程序以 0 退出 | 0 |
| 程序以非 0 退出、编译失败或超时 | 1 |
| job 被拒绝（语言、限制）或基础设施故障 | 1，并输出错误信息 |

## 示例 {#exemplos}

```bash
# 带 stdin 的 Python
rc run --stack python --code 'print(input().upper())' --stdin 'olá'

# 本地文件
rc run --stack go --file ./main.go

# 较短的超时
rc run --stack bash --code 'sleep 10' --timeout 2s
```

通过 Taskfile：

```bash
task run -- --stack python --code 'print(1)'
```

## shell

```bash
rc shell --stack python [--gpu]
task shell -- --stack python
```

在该 stack 的一台全新 VM 中打开交互式 shell，使用与 job 相同的镜像：root 只读、没有网络、`/tmp`
可写。`exit` 后 VM 即消失。

- **在终端中**，guest 会获得真正的 TTY（用 virtio 控制台替代隐式控制台），会话期间 host 的终端
  处于 raw 模式：Ctrl+C 只中断 guest 中的命令，窗口大小也会同步到
  guest（`stty size`）。
- **stdin 来自管道时**，使用普通控制台，没有 TTY：`echo 'python3 -V' | rc shell --stack python` 可以正常工作。
- 如果镜像中有 `bash` 就用 `bash`；Alpine 镜像中则是 `sh`。

如果需要一个能保留所装内容的 shell，请使用机器：`rc machine run -it --stack python`（结束即消失）或
在运行中的机器上使用 `rc machine shell`。参见 [machine](/cli/machine)。

## 前置条件 {#pré-requisito}

该语言的镜像必须已经构建（`rc image build --stack <l>`）；否则 job 会以 E006 被拒绝。
