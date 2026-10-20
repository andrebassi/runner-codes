---
title: '网络与卷'
description: '基于策略的出站网络（allow_hosts、CIDR、封锁私有地址）、发布在 127.0.0.1 上的端口，以及通过 virtiofs 挂载的 host 目录'
---

一台机器诞生时**没有网络，也没有任何来自 host 的东西**。两者都要通过声明逐台机器地开放，而每一处
开放都意味着 VM 内的代码多了一样可以使用的东西。只给不可信的 workload 它真正需要的。

## 一屏看懂 {#em-uma-tela}

```bash
rc machine create --name web --stack python --net                       # 开放互联网（带基础封锁规则）
rc machine create --name api --stack python --allow-host api.openai.com # 只允许该名称及其子域名
rc machine create --name srv --stack python -p 8080:8000                # 发布端口，不允许出站
rc machine create --name dev --stack python -v ./src:/app -v ~/models:/models:ro
rc machine update --name api --allow-host pypi.org                      # 机器运行中也可以
```

## 网络如何工作 {#como-a-rede-funciona}

```
guest (eth0 100.96.0.2/30)                         host（机器的 `rc vmm` 进程）
  │ virtio-net                                     ┌──────────────────────────────────────┐
  └──── socketpair（4 字节 + 以太网帧） ──────────▶│ 网关：gVisor 的 netstack             │
                                                   │  DNS  ─ 名称过滤       ─▶ host 解析器
                                                   │  TCP  ─ SYN 时检查策略 ─▶ host 连接
                                                   │  UDP  ─ 策略           ─▶ host socket
                                                   │  -p   ◀─ 127.0.0.1:HOST ─ 入站连接
                                                   └──────────────────────────────────────┘
```

- **用户态网关，位于 VM 进程内部。** gVisor 的 netstack（与 gvproxy 相同的协议栈）位于一个 socketpair 的
  host 一侧，libkrun 把这个 socketpair 连接到 guest 的 virtio-net（`krun_add_net_unixstream`）。它与 VM 同生
  共死。
- **NAT：** guest 的每个 TCP 连接或 UDP 流都会变成一个从 host 发往同一目标的连接。guest 的任何数据包
  都不会原样到达 host 的网络。
- **guest 在启动时自行配置网络：** 通过 netlink 设置 `eth0` `100.96.0.2/30`、默认路由，以及位于 `100.96.0.1`（即网关）
  的 DNS。镜像中不需要有 `ip`。
- **不使用 TSI。** 所有带网络的机器都经过网关，即使只有 `net = true`，这样下面的基础封锁规则
  才始终有效：TSI 不做任何过滤。

## 出站策略 {#política-de-saída}

| 声明 | 效果 |
|------------|--------|
| 无 | 没有网卡 |
| `--net` / `net = true` | 开放互联网，带基础封锁规则 |
| `--allow-host example.com` | `example.com` 及其子域名；其他名称无法解析 |
| `--allow-host =example.com` | 只允许精确名称 |
| `--allow-host '*.example.com'` | 只允许子域名 |
| `--allow-host-pattern registry.npmjs.org` | 精确名称（或用 `*.x` 表示子域名） |
| `--allow-cidr 1.1.1.1/32` | 该 IP 或地址段 |
| `--deny-cidr 10.0.0.0/8` | 在任何允许规则之前先行拒绝 |
| `--block-private` | 封锁 RFC 1918、CGNAT（100.64/10）和 ULA；互联网仍然开放 |
| `--allow-private 192.168.1.0/24` | 重新开放某个私有地址段（隐含 `--block-private`） |
| 只有 `-p`，没有 `--net` | host 可以访问该端口；guest **无法**出站，也无法解析任何名称 |

任何 `allow` 都会反转默认行为：只有被允许的才能出站。任何出站规则都隐含 `--net`。

**名称如何变成被允许的 IP。** guest 向网关的 DNS 查询，然后：
- 不在列表中的名称得到 **NXDOMAIN**：程序立即看到 `bad address`，无需等待超时；
- 被允许的名称交给 host 的解析器，响应中的 IP 在其 TTL 内被允许，TTL 被限制在 60 s–1 h 之间；
- 任何 UDP/53 查询，无论发往哪个 IP（例如代码中写死的 `8.8.8.8`），都会落入同一个过滤器。

连接到并非来自被允许名称的 IP 会被拒绝。

**只有 `--allow-cidr`、没有 host 列表时：** DNS 会解析任何名称（没有可过滤的对象），但连接只能发往
列出的 CIDR。解析并不等于授权：在 2026/10/19 之前，所有解析出的 IP 都被视为"已学习"而放行，一台
带 `--allow-cidr 127.0.0.1/32` 的机器可以访问 `example.com`。这个漏洞已被修复，并有单元测试和 e2e 测试
（`TestMachineEgressCIDROnly`）。

**基础封锁规则，任何规则都无法打开：**
- 云 metadata（`169.254.169.254`、`fd00:ec2::254`、`100.100.100.200`）以及整个 link-local 地址段。这是窃取账号凭据的
  经典 SSRF 攻击。
- **host 的 loopback**（`127.0.0.1`，guest 可以通过网关 IP `100.96.0.1` 访问）默认
  关闭。只有 `--allow-cidr 127.0.0.1` 才能打开，而且绝不会通过 DNS 学习到的 IP 打开：被允许的域名无法
  指向 loopback（DNS rebinding）。

**拒绝很快。** 被拒绝的 SYN 在 host 上创建 socket 之前就会收到 RST，程序在几毫秒内失败。
每次拒绝都会写入 `<cache>/machines/<nome>/egress-denials.log`：

```
2026-10-19T22:44:24Z deny dns google.com: name not in the allow list
2026-10-19T22:45:02Z deny tcp 1.2.3.4:443: not in the allow list
```

## 在机器运行时修改策略 {#mudar-a-política-com-a-máquina-ligada}

```bash
rc machine update --name api --allow-host pypi.org --allow-cidr 1.1.1.1
rc machine update --name api --remove-allow-host pypi.org
```

- 修改会写入注册信息，并通过控制 socket（`egress.sock`，权限 0600）交给网关。
- 从下一次连接或查询开始生效。被移除名称所学习到的 IP 会被立即遗忘。
- 运行中且**没有**网卡的机器会以 `INVALID_STATE` 拒绝 update：先停止，再更新，然后重新启动。
- 通过 API：`PUT /api/v1/machines/:name/egress`，请求体中包含完整的策略。

## 发布端口 {#portas-publicadas}

```bash
rc machine create --name srv --stack python -p 8080:8000 -p 5353:53/udp
```

- `PORTA`、`HOST:GUEST`、两侧大小相同的端口范围（`5173-5180:5173-5180`，或只写 `5173-5180`），以及
  `/udp` 后缀。范围会展开为每个编号一个端口；每台机器最多 64 个，逐个计数。
- **只监听 host 的 `127.0.0.1`**。是否暴露到 host 的网络，由运维 host 的人决定，而不是由编写
  `runner.code` 的人决定。
- 出站策略不适用于端口（那是入站流量）。没有 `--net` 的端口让 guest 可以提供服务，却无法向外
  拨号。
- 发布的 UDP 端口为每个客户端建立一个会话，60 s 无流量后关闭。

## 卷 {#volumes}

```bash
rc machine create --name dev --stack python -v ./src:/app -v ~/models:/models:ro
```

| 要点 | 行为 |
|-------|--------|
| 格式 | `HOST:GUEST`、`HOST:GUEST:ro`、`HOST:GUEST:rw`（默认 rw） |
| 传输 | virtio-fs（`krun_add_virtiofs3`），每个卷一个 tag（`vol0`、`vol1`...） |
| `:ro` | 由 libkrun 在 **device** 层面强制：guest 的 root 无法通过 remount 解除 |
| 何时挂载 | 每次 `start` 时，在 workdir 和 init 之前（workdir 可以是一个卷） |
| host 路径 | 必须是已存在的目录；`create` 时记录真实路径（解析符号链接后） |
| 相对路径 | 在 CLI 中相对于当前目录；在 `runner.code` 中相对于文件所在目录 |
| `~` | 展开为 HOME |
| 禁止的目标 | `/`、`/proc`、`/sys`、`/dev`、`/tmp`、`/opt/runner`、`/persist` 及其下的任何路径 |
| 上限 | 每台机器 16 个 |
| 通过 API | **拒绝**：通过 API 提供卷，等于让能访问该端口的人任意读写服务器磁盘 |
| guest 新建的文件 | 在 macOS 上，在 host 中为 `0600`（目录为 `0700`）：libkrun 的 virtio-fs 把 guest 的权限模式保存在 xattr `user.containers.override_stat` 中（实测）。被覆盖的已有文件保持原有模式 |

### 磁盘卷 {#volume-de-disco}

`-v /var/lib/docker`（只写 guest 路径）是机器 ext4 上的一个目录，位于 overlay 之外，每次 start 时
绑定到该路径。这正是 Docker 的 overlay2 所需要的，并且与磁盘其余部分一样持久化。

### Staged 卷：`-v HOST:GUEST:staged` {#volume-staged--v-hostgueststaged}

```bash
rc machine create --name dev --stack python -v ./repo:/work:staged
rc machine start --name dev        # 第 1 次 start：把 ./repo 复制到机器磁盘
rc machine sync --name dev         # 把副本写回 host
```

| 要点 | 行为 |
|-------|--------|
| 存放位置 | 机器的 ext4 上（与 `GUEST` 磁盘卷相同的位置）：workload 不必为每个文件往返 virtio-fs |
| 初始填充 | 只在第一次 start 时进行；之后的 start 不会覆盖 workload 所做的修改 |
| sync | 镜像同步：副本中消失的内容在 host 上也会消失，host 在第一次 start 之后新建或编辑的内容会被覆盖。请只通过机器进行编辑 |
| 何时同步 | `rc machine sync`、`POST /api/v1/machines/{name}/sync`、正常的 stop 以及 `machine run` 结束时。`stop --timeout 0` 不会同步 |
| 同步失败时 | stop 不会关闭机器：副本仍保留在机器磁盘上 |
| 权限模式 | sync 会在 host 上应用 guest 中的权限模式（新脚本到达时为 `755`，而不是 `0600`） |
| branch | 子机器把副本继承为普通的磁盘卷，不会向 host 写回任何内容 |

## S3 卷（AWS、R2、MinIO） {#volume-s3-aws-r2-minio}

```bash
rc machine create --name etl --stack python --net \
  -e AWS_ACCESS_KEY_ID=... -e AWS_SECRET_ACCESS_KEY=... \
  -e AWS_ENDPOINT_URL=https://<conta>.r2.cloudflarestorage.com \
  -v s3://meu-bucket/datasets:/data:ro
```

| 要点 | 行为 |
|-------|--------|
| 格式 | `s3://BUCKET[/PREFIX]:/GUEST[:ro]`，可用于 `-v`、`runner.code` 的 `volumes` 以及 API（通过 API 挂载 host 目录仍会被拒绝；bucket 不会触及服务器磁盘） |
| 由谁挂载 | **guest**，通过 FUSE（`go-fuse`，直接调用 `mount(2)`）：镜像中不需要 FUSE 工具 |
| 凭据 | 来自机器的 `-e`，绝不来自 host：`AWS_ACCESS_KEY_ID`、`AWS_SECRET_ACCESS_KEY`、`AWS_SESSION_TOKEN`、`AWS_REGION`（默认 `us-east-1`）和 `AWS_ENDPOINT_URL`（R2、MinIO 等）。没有密钥对时为匿名读取 |
| 网络 | 需要 `--net`（或某种出站策略）：流量从 guest 经网关出站，受策略约束。S3 位于 host 上时：`AWS_ENDPOINT_URL=http://100.96.0.1:PORTA` 加 `--allow-cidr 127.0.0.1/32` |
| 协议 | path-style，自行实现的 SigV4 签名（测试中包含 AWS 官方测试向量） |
| start | guest 在挂载前先列出前缀：bucket 不存在或密钥错误会**导致 start 失败**，并给出 S3 返回的原因 |
| 读取 | 按需、按区间（`Range`）读取；挂载时不会整体下载任何内容 |
| 写入 | 写入 guest tmpfs 中的本地副本；在 `close`/`fsync` 时整体上传（stop 时卸载也会上传） |
| `mv` | `CopyObject` + `Delete`：不是原子操作；复制失败时源对象保持不变 |
| `:ro` | 任何写入都返回 `EROFS`，不会触及 bucket |

限制：不支持符号链接；属主、权限和时间不会持久化；不支持目录
重命名；`mkdir` 在挂载中有效，目录在其中出现第一个文件时才会真正存在于 bucket 中。之所以放弃
写入 `x/` 标记，是因为某个兼容 S3 的后端（`rclone serve s3`）会把它当成文件写入，导致
目录损坏。VM 在 `close` 之前丢失的写入不会到达 bucket。

## 命名网络 {#rede-nomeada}

```bash
rc machine run -d --name db --stack python --network lab -- python3 -m http.server 8080
rc machine create --name app --stack python --network lab && rc machine start --name app
rc machine exec --name app -- wget -qO- http://db.lab.internal:8080/
```

| 要点 | 行为 |
|-------|--------|
| 地址 | 从 `100.98.0.10` 开始，在 `create` 时分配，只要机器存在就固定不变 |
| 名称 | `<máquina>.<rede>.internal` 可在机器的 DNS 中解析 |
| 隔离 | 名称不同的网络之间既不能互访也不能互相解析；每个网络独立分配 IP |
| `--net` | **不**隐含：网络内部的流量不算出站，没有 `--net` 时机器只能与同一网络中的机器通信 |
| 协议 | TCP；服务端看到的是连接方在网络中的 IP |
| 已停止的机器 | 立即从网络中消失：连接被拒绝，名称返回 NXDOMAIN；即使经历 `SIGKILL`，stop 和 delete 也会完成清理 |
| 在 `runner.code` 中 | 在 `[network]` 表中写 `name = "lab"`；CLI 的 `--network` 优先 |
| 通过 API | 在 `POST /api/v1/machines` 中使用字段 `"network": "lab"` |

内部实现：每个网关在网络目录（`~/Library/Caches/runner-codes/n/<hash do nome>`，Linux 上为
`~/.cache/runner-codes/n/...`）中通过一个 Unix socket `<ip>.sock` 和一个文件 `<nome>.ip` 发布本机器。发往
`100.98.x.y` 的连接不经过出站策略：直接发往拥有该 IP 的机器的 socket，附带 6 字节的头部
（源 IP 和端口）。

## 在 `runner.code` 中 {#no-runnercode}

```toml
stack = "python"
net = true
ports = ["8080:8000"]
volumes = ["./src:/app", "~/models:/models:ro"]

[network]
allow_hosts = ["pypi.org", "files.pythonhosted.org"]
block_private = true

[dev]
ports = ["3000"]          # 替换上面的列表
volumes = [".:/workspace"] # 替换上面的列表
```

接受 `net_backend = "virtio-net"`；`"tsi"` 会被拒绝并说明原因。

## 实测数据（macOS 27，M1 Pro） {#números-medidos-macos-27-m1-pro}

| 指标 | 值 |
|--------|-------|
| 带网络的机器 `start` | 190 ms |
| 被封锁的名称到 guest 中出错 | < 1 s（NXDOMAIN） |
| metadata 被拒绝 | 立即（RST） |

## 已知限制 {#limites-conhecidos}

- **仅支持 IPv4。** guest 不会获得 IPv6 地址，IPv6 连接无法出站。
- **ICMP：** `ping` 通过 host 上的数据报 ICMP socket 发出（macOS 上不需要 root；Linux 上用户必须位于
  `net.ipv4.ping_group_range` 中），遵循与 TCP 相同的策略，只有 host 收到自己的回复后，回复才会返回
  guest。被拒绝的会以 `deny icmp` 写入 `egress-denials.log`。每台机器同时最多 32 个 ping。
- 命名网络只支持 TCP：网络中机器之间的 UDP 不通。对网络中某个 IP 的 `ping`，只要拥有该 IP 的机器
  在该网络中处于运行状态就会响应（另一端没有内核来转发 echo；网络中的流量是基于连接的）。
- 在 Linux 上，带网络的机器在没有 Landlock/seccomp 的情况下运行：同一进程中的网关需要打开 socket。
