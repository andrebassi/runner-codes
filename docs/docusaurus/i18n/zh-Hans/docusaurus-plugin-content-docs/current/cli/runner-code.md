---
title: 'runner.code'
description: 'runner.code 文件：用 TOML 声明的机器（stack 或镜像、资源、env、init 与 workload）'
---

`rc machine create` 接受 flag，但一台带有环境变量、准备命令和常驻进程的机器，会变成
一行太长、记不住也无法审查的命令。**`runner.code`** 就是把这份声明写进一个 TOML 文件，
与代码一起放进仓库。

## 一屏看懂 {#em-uma-tela}

```toml
# runner.code
stack = "python"                 # 或：image = "redis:7-alpine"
cpus = 2
memory = 2048                    # MiB
storage = 10                     # 可写磁盘的 GiB

env = ["APP_ENV=dev"]
workdir = "/app"

init = [                         # 只运行一次，在该磁盘第一次 start 时
  "pip install --user flask",
]

entrypoint = ["python3", "-m"]   # workload：entrypoint + cmd，
cmd = ["http.server", "8000"]    # 每次 start 都会启动
stop_signal = "SIGINT"
stop_timeout = 30

[dev]                            # 开发 profile，叠加在上面的配置之上
env = ["DEBUG=1"]
```

```bash
rc machine create --name web               # 读取 ./runner.code
rc machine create --name web -f infra/runner.code
rc machine start  --name web               # init（第 1 次）+ workload
rc machine logs   --name web --source workload
rc machine stop   --name web               # 向 workload 发送 SIGINT，最多等 30 s，sync，关机
```

## 键 {#chaves}

| 键 | 类型 | 默认值 | 作用 |
|-------|------|--------|-----------|
| `stack` | 文本 | — | 以目录中的 stack 为基础（`python`、`go` 等）。使用带 warmup 的镜像 |
| `image` | 文本 | — | 以**任意** OCI 镜像为基础（`redis:7-alpine`）。首次使用时构建，没有 warmup |
| `cpus` | 整数 | 4 | vCPU 数（空闲时在 hypervisor 中休眠） |
| `memory` | 整数，MiB | 8192 | 内存上限；libkrun 只分配 guest 实际触及的部分 |
| `storage` | 整数，GiB | 10 | 机器的可写磁盘（稀疏：在 host 上只占用实际写入的部分） |
| `gpu` | 布尔 | `false` | 通过 Vulkan（Venus）使用 host 的 GPU。配合 `stack` 时，该 stack 必须声明 GPU；配合 `image` 时，镜像必须带有支持 Venus 的 Mesa |
| `gpu_vram` | 整数，MiB | stack 的值 | GPU 共享内存窗口；没有 `gpu = true` 时被忽略 |
| `env` | `K=V` 列表 | — | init、workload、`exec` 和 `shell` 的环境变量 |
| `workdir` | 绝对路径 | — | init、workload 以及未加 `-w` 的 `exec`/`shell` 的工作目录。如果不存在，会在 start 时**创建** |
| `init` | 命令列表 | — | 每一项用 `sh -c` 按顺序运行，**每块磁盘只运行一次** |
| `entrypoint` | 列表 | — | workload argv 的开头部分 |
| `cmd` | 列表 | — | workload argv 的其余部分 |
| `stop_signal` | 文本 | `SIGTERM` | stop 时发送给 workload 的信号：任意 Linux 信号名（`SIGINT`、`SIGWINCH` 等），带不带 `SIG` 均可，`SIGRTMIN+N`（glibc 编号：`SIGRTMIN+3` = 37，即 systemd 使用的那个）或数字（1–64）。使用的是 Linux 的信号表，而不是 host macOS 的，后者有不少编号不同 |
| `stop_timeout` | 整数，s | 10 | workload 退出的宽限时间；超时后 `SIGKILL`。最大 300 |
| `stop_on_exit` | 布尔 | `false` | workload 退出时，机器写回磁盘并自行关机。需要有 workload |
| `volumes` | `HOST:GUEST[:ro\|:staged]` 或 `/GUEST` 列表 | — | 通过 virtiofs 挂载 host 目录，每次 start 时挂载；`:staged` 会复制到磁盘并在 sync 时写回；只写 `/GUEST` 则是机器磁盘上的一个目录。相对路径以文件所在目录为基准；`~` 表示 HOME。参见[网络与卷](/cli/network#volumes) |
| `net` | 布尔 | `false` | 通过 host 网关的出站网络，带有基础封锁规则 |
| `ports` | `PORTA`、`HOST:GUEST`、范围 `A-B:C-D`、`/udp` 列表 | — | 发布在 host 的 `127.0.0.1` 上的端口；范围会展开为每个编号一个端口（总共最多 64 个） |
| `[network]` | 表 | — | `allow_hosts`、`allow_host_patterns`、`allow_cidrs`、`deny_cidrs`、`block_private`、`allow_private`；任意一项都隐含 `net`。`name` 让机器加入一个[命名网络](/cli/network#rede-nomeada)（不隐含 `net`） |
| `[dev]` | 表 | — | `env` 会**追加**到上层配置；`workdir`、`init`、`volumes` 和 `ports` 会**替换**（空列表不替换） |

## 优先级 {#ordem-de-precedência}

**flag > 文件 > 默认值。** 只有真正输入的 flag 才算数：`--cpus 0` 与什么都不写并不相同。

| Flag | 键 | 合并方式 |
|------|-------|--------------|
| `-f`、`--file` | — | 不指定时，如果存在 `./runner.code` 就使用它 |
| `--stack`、`--image` | `stack`、`image` | 一个会清除另一个 |
| `--cpus`、`--memory`、`--disk-gib`、`--gpu`、`--gpu-vram` | `cpus`、`memory`、`storage`、`gpu`、`gpu_vram` | 替换 |
| `-e`、`--env`（可重复） | `env` | **追加**到文件中的值；在 VM 内，同一个键以最后一次定义为准 |
| `-w`、`--workdir` | `workdir` | 替换 |
| `--init`（可重复） | `init` | 替换整个列表 |
| `--stop-signal`、`--stop-timeout`、`--stop-on-exit` | 同名键 | 替换 |
| `-v`、`--volume`（可重复） | `volumes` | **追加**（相对于当前目录） |
| `--net`、`--block-private` | `net`、`block_private` | 替换 |
| `--network` | `[network] name` | 替换 |
| `-p`、`--port`、`--allow-host`、`--allow-host-pattern`、`--allow-cidr`、`--deny-cidr`、`--allow-private`（可重复） | `ports`、`[network]` | **追加** |
| `-- CMD ...` | `entrypoint` + `cmd` | 替换整个 workload |

文件中有意不提供 `--name`：同一个 `runner.code` 可以用于多台机器。

## 生命周期 {#ciclo-de-vida}

```
rc machine start
  │
  ├─ VM 启动，guest 响应                           （与没有 runner.code 的机器相同）
  ├─ mkdir -p <workdir>                            （仅当有 init 或 workload 时）
  ├─ 该磁盘上 init 已运行过？  ── 是 ──┐           （标记 /var/lib/runner/init-done）
  │        └─ 否：每项用 sh -c 运行，  │
  │           带 env + workdir；最后打标记
  ├─ workload 在后台启动  ◀────────────┘           （独立的进程组）
  │     输出 → 控制台，每行带 "[workload] " 前缀
  └─ start 返回

rc machine stop
  ├─ 向 workload 进程组发送 stop_signal
  ├─ 最多等待 stop_timeout  ── 未退出 ──▶ SIGKILL
  ├─ 磁盘 sync
  └─ guest 退出，VM 关机
```

容易踩坑的地方：

- **init 失败时机器保持运行**，并且**不会**给磁盘打标记。`start` 会返回错误，附带失败的命令和 stderr；
  可以用 `rc machine shell` 查看状态，下一次 `start`（在 `stop` 之后）会从头重新运行 init。
  请把 init 命令写成可以重复运行两次的形式（`mkdir -p`、`pip install`）。
- **init 是每块磁盘运行一次，而不是每个文件。** 修改 `runner.code` 中的 `init` 不会在已创建的
  机器上运行任何东西；文件只在 `create` 时读取。如需重做：`rc machine delete` 后再 `create`。
- **workload 在每次 `start` 时都会启动**，包括在 `stop_on_exit` 之后。
- stop 会给 guest `stop_timeout` + 5 s 的时间，之后才由 host 杀掉 VM：workload 能用满整个时限，
  磁盘 sync 也还有余量。
- `rc machine stop` 的 `--timeout` 只对那一次 stop 生效，并覆盖 `stop_timeout`。

## 日志 {#logs}

机器的控制台包含 guest 写出的内容以及 workload 的输出，后者每行带有 `[workload] ` 标记。

```bash
rc machine logs --name web                        # 全部
rc machine logs --name web --source workload      # 只看 workload，不带标记
rc machine logs --name web --source workload --tail 20
```

workload 退出时，最后一行是 `exited with code N`。

## 任意镜像 {#imagem-qualquer}

```toml
image = "redis:7-alpine"
cmd = ["redis-server", "--save", "60", "1"]
```

镜像在 host 上拉取、展平，再由与 stack 相同的构建 VM 转换成 ext4，注入 guest 和
`start.sh`，并保留镜像的 `Env`。内部名称是 `img-` + 引用 sha256 的 12 位十六进制：相同的
引用会复用缓存中的 ext4。

与 `stack` 的区别：没有 warmup，也不在目录中，并且镜像必须是与 host 同架构的 Linux 镜像
（Mac 上为 arm64，Linux 上为 amd64 或 arm64）。

## 会被拒绝的内容 {#o-que-é-recusado}

`runner.code` 能识别但尚未实现的键会**报错并说明原因**，绝不会被静默
忽略：如果 `[network] allow_hosts` 被忽略，就会让人误以为机器有网络过滤。

| 键 | 情况 |
|-------|----------|
| `net_backend = "tsi"` | 所有网络都经过带过滤的网关；接受 `"virtio-net"` |
| `allow_system_mounts` | 挂载 host 的系统目录（`/etc`、`/var/log`） |
| `nested`、`nested_virt` | 是否嵌套由启动机器的人决定：在 CLI 中使用 `--nested` |
| `cuda` | rc 不支持 CUDA（`--gpu` 是 Vulkan） |
| `overlay`、`health`、`restart`、`[artifact]`、`[pack]`、`rosetta` 等 | 未实现 |

`user`、`docker_socket`、`[secrets]` 和 `[auth] ssh_agent` 是被接受的，效果与对应的 flag 相同
（`--user`、`--docker-socket`、`--secret-env`/`--secret-file`、`--ssh-agent`）。
| 任何其他键 | `unknown key(s): ...`（拼写错误不会被放过） |

## API {#api}

`POST /api/v1/machines` 在 JSON 中接受同样的字段（`image`、`env`、`workdir`、`init`、`workload`、
`stop_signal`、`stop_timeout`、`stop_on_exit`、`gpu_vram_mib`），或在 `runner_code` 中传入整个文件。两者同时提供时，
以文件为基础，JSON 中每个非空字段优先，就像 CLI 中的 flag 一样。

```bash
curl -s localhost:8080/api/v1/machines -d "$(jq -n --rawfile rc runner.code '{name:"web", runner_code:$rc}')"
curl -s 'localhost:8080/api/v1/machines/web/logs?source=workload&tail=20'
curl -s -X POST localhost:8080/api/v1/machines/web/stop          # 使用机器的 stop_timeout
```
