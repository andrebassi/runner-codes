---
title: 'Guest runner'
description: '在 VM 内运行并执行作业代码的静态二进制'
---

## 是什么 {#o-que-é}

`runner-guest` 是在 microVM 内执行代码的进程。它为 Linux 静态编译，并以 `/opt/runner/guest` 的形式注入到每个镜像中。在 host 上，它位于 `bin/rc-guest-linux-<arch>`。

**为什么静态编译并与 host 分开：** host 通过 purego 加载 libkrun，这会在 Linux 上强制对 glibc 动态链接。各语言镜像是 Alpine（musl）；动态链接的二进制在那里会以 `/opt/runner/guest: not found` 失败。

## 如何启动 {#como-ele-sobe}

作业 VM 的第一个进程是 `/opt/runner/start.sh`：

```sh
mount -t tmpfs -o size=256m,mode=1777 tmpfs /tmp
# /dev/stdin、/dev/stdout、/dev/stderr 和 /dev/fd 作为指向 /proc/self/fd 的链接
...
# 镜像 OCI config 中的环境变量（PATH、JAVA_HOME...），每行一个
while IFS= read -r l; do [ -n "$l" ] && export "$l"; done < /opt/runner/env
mkdir -p /tmp/home
export HOME=/tmp/home
# 镜像中预热好的 Go 构建缓存，复制到执行器强制使用的路径
if [ -d /opt/runner/gocache ]; then
  cp -a /opt/runner/gocache /tmp/go-cache
fi
exec /opt/runner/guest --port 5000
```

（摘自 `internal/adapters/oci/scripts/start.sh`。）在持久化 machine 中，第一个进程是
`/opt/runner/machine.sh`，它挂载磁盘和 overlay，并在 `chroot` 中运行同一个 guest。

guest 监听 vsock 端口 5000。libkrun 把这个端口绑定到 host 上的一个 Unix socket。

## 它如何处理一个作业 {#o-que-ele-faz-com-um-job}

1. 读取一个 frame（4 字节 big-endian 长度 + JSON），其中是[作业](/api-reference/job-request)。
2. 检查语言和 stdin 的大小。
3. 在 `/tmp` 中创建临时目录并写入文件：
   - 有 `files` 时，按名字逐个写入；**第一个**是主文件；
   - 只有 `code` 时，写入 `script.<ext>`（Java：`Main.java`）。
4. 按语言类型执行：
   - **解释型**（Python、Node.js、Ruby、PHP、Perl、Bash、sh、AWK、jq）：`<interpretador> <arquivo>`；
   - **编译型**（C、C++、Rust）：编译成二进制再执行该二进制；
   - **JVM**（Java）：先 `javac`，再 `java Main`；
   - **Go：** `go run`；
   - **SQLite：** 代码通过 `sqlite3 :memory:` 的 stdin 传入。
5. 应用作业的超时（默认 10 s）。
6. 以一个[结果](/api-reference/job-response) frame 响应。

编译失败返回 `exception: "Compilation failed"`，编译器的错误在 `stderr` 中。超时返回 `exception: "Timeout"`。

## 其他操作 {#outras-operações}

除了运行代码，guest 还通过同一协议处理 machine 的操作：`exec`（不带隐式 shell 的 argv，可指定 env、目录和用户）、
`put`/`get`（以 1 MiB 分块复制文件）、`pty`（交互式终端）、`workload`（machine 持续运行的进程）、`shutdown`（向
workload 发信号、`sync` 并退出）、`dial`（发布的端口，拨向 guest 的 `127.0.0.1`）、`set-env`（仅在内存中的
secrets）、`ssh-agent`（host 的 SSH agent socket）和 `mount-s3`（通过 FUSE 挂载 `s3://` 卷）。详见 [vsock 协议](/api-reference/vsock-protocol)。

## 作业之间的隔离 {#isolamento-entre-jobs}

guest 不需要在作业之间清理任何东西：每台 VM 只处理一个作业，然后被销毁。tmpfs 随之消失。

## guest 认识的语言 {#linguagens-que-o-guest-conhece}

guest 的执行器（`internal/guest/executor.go`）包含的语言条目比目录公开的更多。只有 [`languages.yaml`](/languages/overview) 中的 15 种有镜像，并被 API 接受。
