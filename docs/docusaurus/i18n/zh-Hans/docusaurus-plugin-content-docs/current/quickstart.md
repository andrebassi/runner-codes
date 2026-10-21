---
title: '快速开始'
description: '安装 libkrun、编译、构建 Python 镜像，运行第一个 job 和第一台机器'
sidebar_position: 2
---

## 前置条件 {#pré-requisitos}

- Apple Silicon 上的 macOS，**或**对 `/dev/kvm` 有读写权限的 Linux x86_64/aarch64。
- 用 [Nix](https://nixos.org) 安装 libkrun；用 Go 和 [Task](https://taskfile.dev) 编译。
- 需要访问互联网以下载各语言的 OCI 镜像（仅在构建镜像时需要；job 的 VM 没有网络）。

不支持 Windows。

## 1. 安装 libkrun 和 libkrunfw {#1-instalar-a-libkrun-e-a-libkrunfw}

两个系统都通过 Nix 安装（不使用 Homebrew，也不使用 Docker）：

```bash
task krun:install    # nix profile add .#krun
```

- **macOS：** libkrun 1.19.6 和 libkrunfw 5.6.2，来自仓库自带的 derivation（`nix/`）。第一次会编译
  libkrun（约 10 分钟）。
- **Linux：** 使用 nixpkgs 中的 libkrun 和 libkrunfw，并启用 virtio-blk。

两个库都会安装到 `~/.nix-profile/lib`，`rc` 默认在这里查找。

## 2. 编译 {#2-compilar}

```bash
task build
```

在 `bin/` 下生成：

- `rc`：host 端（CLI、API、镜像构建、机器，以及每台 VM 的进程）；
- `rc-guest-linux-<arch>`：在 VM 内运行的静态 guest。

在 macOS 上，构建会给 `rc` 签上 `com.apple.security.hypervisor` entitlement。没有它，`krun_start_enter` 会失败。

## 3. 检查 host {#3-conferir-o-host}

```bash
./bin/rc doctor
```

每项一行：`platform`、`libkrun` 和 `guest` 显示 `ok`；本机不提供的能力（CUDA、
嵌套虚拟化、macOS 上的隔离）显示为 `info` 行；每个已构建的镜像对应一行 `ok image <stack>`。失败输出为
`FAIL <item> <motivo>`。

## 4. 构建语言镜像 {#4-construir-a-imagem-da-linguagem}

```bash
task image:build -- --stack python
```

拉取 `python:3.12-alpine`，生成写入了 guest 和 warmup 的只读 ext4，并存入缓存。在测量所用的机器上，
Python 镜像耗时约 8–10 s。输入相同的第二次构建直接命中缓存。要构建目录中的所有
语言：`task image:build -- --all`。

## 5. 运行一个 job {#5-rodar-um-job}

```bash
./bin/rc run --stack python --code 'print(1)'
./bin/rc shell --stack python      # 在全新 VM 中打开交互式 shell；exit 后即消失
```

每次 `run` 都会启动一台新 VM（冷启动），在程序运行时显示输出，并在结束时销毁 VM。如果程序失败或超时，
命令以 rc 1 退出。参见 [run 与 shell](/cli/run)。

## 6. 启动一台持久化机器 {#6-ligar-uma-máquina-persistente}

```bash
./bin/rc machine create --name demo --stack python
./bin/rc machine start --name demo
./bin/rc machine exec --name demo -- sh -c 'echo persistiu > /etc/minha-config'
./bin/rc machine stop --name demo
./bin/rc machine start --name demo
./bin/rc machine exec --name demo -- cat /etc/minha-config     # persistiu
./bin/rc machine delete --name demo --force   # 不加 --force 时，运行中的机器会被拒绝（INVALID_STATE）
```

写入 `/` 和 `/workspace` 的内容在 stop 之后依然保留；`/tmp` 会恢复为空。不加 `--net` 时，机器没有
网络。参见 [machine](/cli/machine)、[runner.code](/cli/runner-code) 和[网络与卷](/cli/network)。

## 7. 启动 API {#7-subir-a-api}

```bash
task serve
```

监听 `127.0.0.1:8080`，并为每门已构建镜像的语言维护一个包含 2 台就绪 VM 的 [pool](/concepts/pool)。
在另一个终端中：

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{"language":"python","files":[{"name":"main.py","content":"print(input()[::-1])"}],"stdin":"abc"}'
```

```json
{"status":"success","stdout":"cba\n","stderr":null,"exception":null,"executionTime":35}
```

`executionTime` 的值每次执行都不同。API 没有认证：请只在 loopback 上使用。参见
[HTTP API](/cli/http-api) 和 [SDK](/api-reference/sdks)。

## 8. 一次性检查全部 {#8-conferir-tudo-de-uma-vez}

```bash
task runbook
```

端到端执行 `docs/runbook.md`，只要有任何一步与预期不符就判定失败。最近一轮
（2026/10/21，macOS 27）共 126 项检查，0 项失败。
