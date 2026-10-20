---
title: 'machine'
description: '持久化机器：一台有名称、有独立磁盘的 VM，在命令之间以及 stop 与 start 之间保留你写入的内容'
---

`rc run` 为每个 job 创建一台新 VM，结束时销毁。对于多步骤的工作（安装、编译、测试、
修复），请使用**机器**：一台有名称、有独立磁盘的 VM，在一条条命令之间、在 `stop` 与 `start` 之间
都持续存在。

## 一屏看懂 {#em-uma-tela}

```bash
rc machine create --name dev --stack python      # 注册并创建磁盘（不启动）
rc machine start  --name dev                     # 启动；命令退出后 VM 继续运行
rc machine exec   --name dev -- python3 -V       # 运行一条命令（输出为 streaming）
rc machine shell  --name dev                     # 交互式终端（Ctrl+C、vi、resize）
rc machine cp ./app.py dev:/workspace/app.py     # 上传文件
rc machine exec   --name dev -- python3 /workspace/app.py
rc machine cp dev:/workspace/result.json .       # 下载文件
rc machine logs   --name dev --tail 20           # 机器的控制台（停止状态下也可用）
rc machine stop   --name dev                     # 写回磁盘并关机
rc machine ls                                    # 列出全部机器及其状态
rc machine delete --name dev                     # 删除机器和磁盘（运行中需加 -f）
```

所有命令中 `--name` 都是可选的：默认为 `$RC_MACHINE_NAME`，没有该变量时为 `default`。借助 direnv 可以为
每个项目固定一台机器。

如果目录中有 [`runner.code`](/cli/runner-code)，`create` 会从中读取基础（stack 或镜像）、资源、
环境变量、init 和 workload；命令行传入的 flag 优先于文件。

## 哪些内容会持久化 {#o-que-persiste}

| 路径 | 是否持久化 | 存放位置 |
|---------|-----------|-----------|
| `/`（软件包、`/etc`、`/opt`、`/root` 等） | **是** | 机器磁盘上的可写层（overlay），叠加在 stack 镜像之上 |
| `/workspace` | **是** | 机器磁盘上的目录；适合存放代码、数据和结果 |
| `/tmp` | **否** | 256 MiB 的 tmpfs，每次 start 都为空 |

`HOME` 是 `/root`，同样会持久化。

## 工作原理 {#como-funciona}

```
host (macOS)                                      │ guest
                                                  │
~/Library/Caches/runner-codes/machines/dev/       │
  machine.json   注册信息（stack、cpus 等）       │
  disk.ext4      机器磁盘 ── /dev/vdb ────────────┼─▶ /persist ─┬─ upper/      （已写入的内容）
  spec.json      VM 的 spec                       │             ├─ work/       （overlayfs 使用）
  console.log    控制台（rc machine logs）        │             └─ workspace/  ─▶ /workspace
  v.sock         与 guest 的通道（vsock 5000）    │
  vmm.pid        VM 进程                          │  stack 镜像 ── /dev/vda（只读）
                                                  │  overlay：镜像在下 + upper 在上 ─▶ /
```

- **磁盘**：第一次时，构建 VM 会格式化一个空的稀疏 ext4（`disks/data-10g.ext4`）。每台机器
  得到它的一个**克隆**：APFS 上用 clonefile，btrfs/XFS 上用 FICLONE，其他情况用稀疏复制。在 macOS 上实测：
  一块 10 GiB 的磁盘刚创建时在 host 上只占 6.4 MB。
- **启动**：机器的第一个进程是 `/opt/runner/machine.sh`，取代 job 使用的 `start.sh`。它把
  磁盘挂载到 `/persist`，建立一个 overlayfs（镜像在下，`upper/` 在上），挂上 `/workspace`，并在那里的
  `chroot` 中运行 guest-runner。
- **分离的 VM**：机器的 `rc vmm` 进程运行在独立会话中（`setsid`），没有监视管道，也没有
  `Pdeathsig`。因此它在启动它的 `rc` 退出后依然存活。pid 记录在 `vmm.pid` 中，"运行中"即表示该 pid 存活。
  临时 job 不受影响：它们的 VM 依然随父进程一起退出。
- **停止**：host 请求 guest 执行 `shutdown`，guest 执行 `sync`（把缓存中的内容写入 ext4），回复后退出，
  VM 随即关机。如果在 `--timeout`（默认 10 s）内没有结束，就会收到 `SIGKILL`。`--timeout 0` 会立即杀掉。
- **终端（`shell`）**：guest 打开一个 PTY（devpts 由 `machine.sh` 挂载）并在其中运行 shell。连接变成
  双向的帧通道：host 发送 `input`（按键，用 base64 编码以免丢失控制字节）和 `resize`；guest 发送
  `output` 和 `exit`。host 的终端处于 raw 模式，`SIGWINCH` 被转换为 `resize`。关闭
  终端会杀掉 guest 中的程序；机器继续运行。
- **镜像更新**：start 使用 stack 的当前镜像。如果镜像被重新构建，机器会在自己写入的内容之下
  看到新镜像，就像一次系统更新。

## 临时机器：`machine run` {#máquina-efêmera-machine-run}

一步完成创建、启动和使用：

```bash
rc machine run --net --image alpine -- uname -a          # 运行后删除
rc machine run --net -it --image alpine -- /bin/sh       # 终端；退出时删除
echo dados | rc machine run -i --stack python -- sh -c 'read x; echo $x'
rc machine run -d -s runner.code                         # 带着 workload 保持运行
docker save meuapp | rc machine run --image - -- ./app   # 本地镜像，无需 registry
rc machine run --image ./meuapp.tar -- ./app             # docker save 的 tar 或 OCI layout 目录
```

| 要点 | 行为 |
|-------|--------|
| 运行什么 | `--` 之后的命令；没有则用 `runner.code` 的 `cmd`/`entrypoint`；再没有且带 `-t` 时，打开一个 shell |
| 不加 `-d` | 名称为 `run-<hex>`；结束时删除机器，Ctrl+C（rc 130）和 SIGTERM 时也会删除 |
| `-d` | 名称为 `default`（或 `--name`）；命令成为 workload，机器保持运行 |
| `-t` / `-it` | 真正的终端（guest 中的 PTY）；只有 `-i` 没有 `-t` 时仅转发 stdin；两者都没有时不读取 stdin |
| 本地镜像 | 以 `/`、`./` 或 `../` 开头的路径：`docker save` 的 tar（或 OCI tar）或 OCI layout 目录；架构与 host 不同会被拒绝 |
| `--image -` | 从 stdin 读取 tar 并保存到 `<cache>/imports/sha256-<hash>.tar`：相同的镜像会复用缓存 |
| init | 会运行（磁盘总是新的） |

声明类的 flag 与 `create` 相同，包括 `-s`/`-f`、`-v`、`-p`、`--net`、网络策略以及
`--display LxA`（参见[浏览器中的屏幕](/cli/display)）。

## Branch 与暂停 {#branch-e-pausa}

```bash
rc machine pause  --name dev                    # 冻结：进程、内存和连接保持原样
rc machine resume --name dev
rc machine branch --from dev --name try-1       # 磁盘的 copy-on-write 副本（运行中或已停止均可）
```

| 要点 | 行为 |
|-------|--------|
| pause | 对 VM 进程发送 SIGSTOP：vCPU、网络网关和查看器一起停止；`exec`、`cp`、`update` 和 `start` 以 `INVALID_STATE` 拒绝；`stop` 会先恢复再关机 |
| branch | 子机器的磁盘是父机器磁盘的 copy-on-write 克隆（APFS 上用 clonefile，Linux 上用 FICLONE）：瞬间完成，在内容分叉之前不占空间。已安装的软件、`/root` 和 `/workspace` 一并带过来，init 不会再次运行 |
| 父机器运行中或已暂停 | guest 执行 `sync`，VM 只在克隆期间冻结；磁盘的一致性相当于一次断电之后 |
| 不会带过来的 | **执行状态**：进程和内存。RAM 和 CPU 的 checkpoint 需要 snapshot，而上游 libkrun 没有 |
| 端口与屏幕 | 子机器继承相同的端口和屏幕端口；不修改其中一个的话，两者不能同时启动 |

## 命令 {#comandos}

| 命令 | Flag | 说明 |
|---------|-------|------------|
| `create` | `--name`、`-f FILE`、`--stack` 或 `--image`、`--cpus`（4）、`--memory` MiB（8192，弹性）、`--disk-gib`（10）、`--gpu`、`--gpu-vram`、`-e K=V`、`-w DIR`、`-u USER`、`--init CMD`、`--stop-signal`、`--stop-timeout`、`--stop-on-exit`、`-v`、`-p`、`--docker-socket`、`--expose-socket`、`--mount-socket`、`--secret-env`、`--secret-file`、`--ssh-agent`、`--nested`、`-- WORKLOAD` | 不启动机器。不加 `-f` 时，如果存在 `./runner.code` 就读取它。参见 [runner.code](/cli/runner-code) |
| `start` | `--name` | 等待 guest 响应（最多 30 s）；如果没有响应，就杀掉 VM 并显示控制台末尾。如果 VM 进程提前退出（配置被拒绝，例如在不支持嵌套的 host 上使用 `--nested`），会立即报错并附上其日志的最后一行。之后运行 init（磁盘的第 1 次）并启动 workload |
| `stop` | `--name`、`--timeout`（默认：机器的 `stop_timeout`，或 10s） | 在 sync 之前向 workload 发送 `stop_signal`；停止一台已停止的机器不算错误 |
| `status` | `--name`、`--json` | `running` 或 `stopped`，附带 pid |
| `ls` | `--json` | 所有机器 |
| `delete` | `--name`、`-f` | 运行中的机器必须加 `-f` |
| `exec` | `--name`、`-e K=V`（可重复）、`-w DIR`、`-i`（转发 stdin）、`--timeout`（0 = 不限） | argv **不经过隐式 shell**：需要 shell 时用 `-- sh -c '...'`。`rc` 的退出码就是命令的退出码（超时为 124） |
| `shell` | `--name`、`-e K=V`、`-w DIR`、可选的 `-- CMD` | 真正的终端（guest 中的 PTY）：Ctrl+C 只中断当前命令，窗口大小跟随 host。不带命令时，镜像中有 `bash` 就打开 `bash`，否则打开 `sh`。`rc` 的退出码就是 `exit` 的退出码 |
| `cp` | `SRC DST`，其中一侧为 `nome:/caminho` | 以 1 MiB 分块；两端都是原子操作；最大 4 GiB |
| `logs` | `--name`、`--tail N`、`--source all\|workload` | VM 控制台：guest、`machine.sh` 和 workload 写出的内容；`workload` 只过滤出它的输出 |
| `sync` | `--name` | 把 staged 卷的副本写回 host（参见[网络与卷](/cli/network#volume-staged--v-hostgueststaged)） |
| `data-dir` | `--name` | 机器在 host 上的目录（注册信息、磁盘、发布的 socket、日志） |
| `prune` | `--all` | 与 `rc image prune` 相同：释放旧的镜像构建；加 `--all` 时，还会释放没有任何机器使用的引用镜像 |

## 错误 {#erros}

每次拒绝都带有一个稳定的错误码。请根据错误码而不是错误信息来判断：

| 代码 | 何时出现 |
|--------|--------|
| `NOT_FOUND` | 机器（或 `cp` 中的文件）不存在 |
| `CONFLICT` | `create` 使用了已存在的名称 |
| `INVALID_STATE` | 对运行中的机器 `start`；对已暂停的机器 `start`（`is paused; resume it ... or stop it first`）；对已停止的机器 `exec`/`cp`；对运行中的机器 `delete` 但未加 `-f`；对已停止且带 staged 卷的机器 `sync` |
| `CONFIG_ERROR` | 名称无效（小写字母、数字和 `-`，最多 63 个字符）、stack 没有镜像、在不支持 GPU 的 stack 上使用 `--gpu`、`cp` 中使用相对路径 |

## 实测数据 {#números-medidos}

macOS 27，Apple M1 Pro，python stack：

| 指标 | 值 |
|--------|-------|
| `start` 到 guest 响应 | 204–244 ms |
| 新建 10 GiB 磁盘在 host 上占用的空间 | 6.4 MB |
| 向机器 `cp` 50 MB | 620 ms（约 84 MB/s） |
| 从机器 `cp` 50 MB | 594 ms（约 88 MB/s） |

## 用户、secret、SSH agent 与 socket {#usuário-segredos-agente-ssh-e-sockets}

| Flag | 作用 |
|------|-----------|
| `-u`、`--user NOME\|uid[:gid]` | workload、exec 和 shell 以该用户运行；init 以 root 运行。`exec --user` 和 `shell --user` 只改变那一条命令 |
| `--secret-env VAR=HOST_VAR` | host 上该变量的值，在 start 时读取，出现在每条命令的环境中；只保存在 guest 内存中，绝不写入注册信息 |
| `--secret-file VAR=/abs/arquivo` | 同上，但内容来自 host 上的一个文件。与某个凭据同名的 secret 会供给对应的 binding，不会进入 VM |
| `--ssh-agent` | 执行 start 一方的 agent（`SSH_AUTH_SOCK`）出现在 guest 中；私钥留在 host 上 |
| `--docker-socket` | guest 的 `/var/run/docker.sock` 变为机器 `data-dir` 中的 `docker.sock` |
| `--expose-socket GUEST[:HOST]` | workload 监听的一个 Unix socket，可从 host 访问（默认：位于 `data-dir` 中、与其同名） |
| `--mount-socket HOST:GUEST` | 把 host 的一个 socket 放进机器；guest 中缺失的目录会被创建。每台机器最多 64 个桥接 |
| `--nested` | 暴露 host 的虚拟化扩展（guest 中的 `/dev/kvm`）。需要 Linux 上的嵌套 KVM，或 macOS 15+ 的 M3 及更新的 Mac；`rc doctor` 会说明本机是否支持嵌套。`runner.code` 无法开启它 |

socket 桥接只存在于 CLI 中：如果通过 API 提供，它们就是服务器磁盘上的任意路径。

## API {#api}

`rc serve` 提供同样的操作。参见 [API 概览](/api-reference/overview#máquinas)。

## 本版本不包含 {#fora-desta-versão}

API 的交互式 WebSocket（目前终端只能通过 CLI 使用）、`machine stats` 以及 RAM 的 checkpoint（上游 libkrun
没有 snapshot）。
网络、端口和卷见[网络与卷](/cli/network)。演进计划见
[Agent 参考](/agents#plano-de-evolução)。
