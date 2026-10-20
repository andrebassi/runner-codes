---
sidebar_position: 8
title: Pack（可移植制品）
---

# Pack：把一台机器装进一个文件 {#pack-uma-máquina-num-arquivo}

`rc pack create` 把一个镜像，或一台连同已安装内容的机器，变成一个制品：复制到
另一台 host 上即可运行，无需 pull、无需构建、无需下载。

```bash
rc pack create --stack python --output ./py312         # stub ./py312 + payload ./py312.rcmachine
./py312 run -- python3 --version

rc pack create --from-vm dev --output ./dev --single-file   # 单个可执行文件
./dev run -- python3 -c 'import meu_pacote'
```

| 输出 | 是什么 |
|-------|---------|
| `py312` | stub：就是这个 `rc`，在 macOS 上带 hypervisor entitlement 签名 |
| `py312.rcmachine` | payload：ext4 镜像（如果来自机器，还有它的磁盘）以及 manifest |
| `--single-file` | payload 附加在 stub 之后，末尾带一个 16 字节的 trailer |

两个文件要一起分发。单独运行 stub 只会显示用法并退出。

## 来源 {#origens}

| Flag | payload 中包含什么 |
|------|----------------------|
| `--stack python` | 该 stack 的镜像（如果尚不存在，会先构建） |
| `--image alpine:3.20` | 构建好的 OCI 镜像 |
| `--from-vm NOME` | 机器的镜像**以及它的磁盘**：已安装和已写入的内容一并带走。机器必须处于停止状态（运行中的磁盘不一致） |

机器相关的 flag（`--cpus`、`--mem`、`-e`、`--workdir`、`--net`、`--allow-host`、`-p`、`--` workload...）会写入
manifest，成为制品的默认值。以下内容不会被带走：
- host 目录卷（路径属于另一台机器），会被拒绝并说明原因；`s3://` 卷会被带走；
- 凭据，因为 CA 和占位符属于源机器；
- 命名网络。

## 制品的子命令 {#subcomandos-do-artefato}

```bash
./dev run [-- COMMAND...]    # 临时机器，结束后删除（返回退出码）
./dev start                  # 以文件名命名的持久化机器（第 1 次时创建）
./dev exec -- pip install x  # 在该机器中执行
./dev shell | logs | status | stop
./dev inspect                # 查看 manifest，无需解压
```

另外还有：`rc machine create --name x --from dev.rcmachine`（或 `--from ./dev`）、`rc machine run --from ...` 以及
`rc pack inspect ARQUIVO`。

## 内部结构 {#como-é-por-dentro}

- **自有的稀疏格式。** 一块机器磁盘逻辑上有 10 GiB，几乎全是空洞，而 Go 的 `archive/tar` 无法
  以稀疏方式写入。payload 只保存有数据的区段（通过 `SEEK_DATA`/`SEEK_HOLE` 找到），放在一个 gzip 流中；
  解压时重建空洞。实测：python 镜像（134 MiB）变为 31 MiB；一台装了软件包的 python 机器，
  连同 stub 打成单个文件，为 46 MiB。
- **只解压一次**，解压到 `<cache>/packs/<chave>`，其中键是 manifest 的指纹，并注册为镜像
  `pack:<chave>`。第二次 `run` 不会再次解压。
- **payload 来自外部**：带 `/` 或 `..` 的文件名、超出文件范围的区段、被截断的数据、超过
  1 MiB 的 manifest 以及未来版本的格式都会被拒绝（已测试）。

## 验证：检查一个值，而不只是能启动 {#prova-conferir-um-valor-não-um-boot}

一个丢失了磁盘的 pack 依然能启动并以 0 退出。e2e 测试（`TestPack`）在一台机器中写入一个标记和一个 Python
模块，打包，**删除源机器**，再通过制品读取该标记。植入的缺陷（一个不带
磁盘的 pack）会被发现。

## 制品不包含什么 {#o-que-o-artefato-não-leva}

stub 不内嵌 VM 运行时：它使用**目标 host 上的 libkrun**（`task krun:install`；
`rc doctor` 会检查）：macOS 上 Nix 的 libkrun 以 `/nix/store` 的绝对路径链接 virglrenderer 和 MoltenVK，
要把这套闭包一起带走就得改写库的路径。真正被带走的是镜像和机器状态，
也就是在目标端无法复现的部分。stub 属于创建它时所在的系统和架构
（manifest 中的 `host_platform`）。
