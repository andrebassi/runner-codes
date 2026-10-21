---
title: 'Kubernetes（k3s 和 microk8s）'
description: 'rc 作为 containerd 运行时：每个 pod 成为一台 libkrun microVM，拥有 pod IP、exec、日志和 Service'
---

使用 `rc` 运行时后，每个设置了 `runtimeClassName: rc` 的 pod 都运行在一台**拥有独立内核的 libkrun microVM**
中，而不是共享节点内核。它在 **k3s** 和 **microk8s** 上都可用，两者都用同一套 19 项检查验证过。

## 工作原理

```
kubelet ─CRI─▶ containerd ─▶ containerd-shim-rc-v2（每个 pod 一个）
                                 │  sandbox（pause）→ 启动 pod 的 VM
                                 │  应用容器 → rootfs 复制到共享目录，在 VM 内 chroot
                                 ▼
                         rc vmm（libkrun，KVM）  ◀── virtio-fs（pod 共享目录）和 virtio-net（pod netns 中的 TAP）
```

- **每个 pod 一台 VM。** pause 容器启动 VM；应用容器在其中运行，因此同一 pod 的两个容器看到同一个内核和同一个
  emptyDir。
- **使用 CNI 网络，无 NAT。** shim 在 CNI 为 pod 创建的 netns 中建立一个 TAP，并用 `tc mirred` 双向连接
  `eth0`。guest 接管 `eth0` 的 IP、MAC 和网关：对集群来说 VM 就是 pod（DNS、Service、pod 之间互通）。网关在子网内
  （k3s 的 flannel）和子网外（microk8s 的 Calico，`169.254.1.1`）都可用。
- **复制的 rootfs。** containerd 挂载的快照被复制（`cp -a --reflink=auto`）到 pod 的 virtio-fs；通过 virtio-fs
  重新导出 overlay 会导致 guest 内无法写入。
- **exec、日志和 TTY。** 每个进程都是与 guest 之间的一条流：stdin、stdout、stderr、resize 和信号。
  `kubectl logs` 与任何运行时一样来自 containerd 的 FIFO。

## 安装

需要带 `/dev/kvm`（KVM）的 Linux 节点。在仓库中执行 `task krun:install` 和 `task build` 之后：

```bash
sudo deploy/k3s/install.sh        # k3s：运行时写入 config-v3.toml.d/rc.toml，重启 k3s，创建 RuntimeClass rc
sudo deploy/microk8s/install.sh   # microk8s：在 containerd-template.toml 中加入配置块，重启 snap 的 containerd
```

两个安装脚本都会调用 `deploy/install-node.sh`（rc 和 libkrun 放在 `/opt/rc`，pod VM 镜像、
`/usr/local/bin/containerd-shim-rc-v2` 和 `/etc/rc/shim.json`），给节点打上 `runner.codes/runtime=true` 标签，
并应用 `deploy/k8s/runtimeclass.yaml`。可以安全地重复执行；发行版升级后需要再运行一次。

## 使用

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: rc-hello
  annotations:
    runner.codes/cpus: "1"          # 可选：VM 的 vCPU 数（1 到 16，默认 2）
    runner.codes/memory-mib: "512"  # 可选：VM 内存上限（默认 1024；内存是弹性的）
spec:
  runtimeClassName: rc
  containers:
    - name: hello
      image: busybox:1.36
      command: ["sh", "-c", "uname -sr; sleep 3600"]
```

`kubectl logs rc-hello` 显示的是 VM 的内核（`Linux 6.12.91`），而不是节点的内核。

## 实测数据（DigitalOcean droplet，4 vCPU，嵌套 KVM）

| 指标 | 数值 |
|------|------|
| pod 到 `Running`（镜像已在节点上） | 约 2 秒 |
| `deploy/k8s/e2e.sh` 测试 | k3s 1.36（containerd 2.3）和 microk8s 1.35（containerd 2.1）均为 19/19 |
| 在运行时代码中植入的缺陷（`task k8s:defects`） | 20 个，全部被测试发现 |

## 设计决策

| 决策 | 原因 |
|---|---|
| **每个 pod 一个 shim**，按 `io.kubernetes.cri.sandbox-id` 注解分组 | 不分组的话，containerd 会为每个容器启动一个 shim，应用容器就找不到 pod 的 VM |
| **containerd 库 v2.3.7**，而不是 2.4 | 2.4 只接受新的 start 协议（containerd ≥ 2.3）；microk8s 1.35 自带 containerd 2.1.6。2.3.7 两种都能读，只对走旧路径的调用方把响应转换成旧的 JSON |
| **复制 rootfs**，而不是重新导出 | 通过 virtio-fs 重新导出 snapshotter 的 overlay 会破坏 guest 内的 copy-up；在 btrfs/xfs 上 `cp -a --reflink=auto` 是瞬时的 |
| **VM 内不用 crun** | rc 的 guest 在容器 rootfs 上用 chroot 运行每个进程；整台 VM 本身就是 pod 的隔离边界 |
| **TAP + `tc mirred`**，不用 NAT 也不用 gVisor | guest 接管 pod `eth0` 的 IP、MAC 和网关，因此集群其余部分（bridge、ARP、kube-proxy）察觉不到差别 |
| **`Stats` 返回 vmm 的真实用量** | 空的 `Stats` 会让整个节点的 kubelet 统计摘要失效；VM 的用量全部计入第一个应用容器，避免重复计算 |
| **shim 的子进程通过 containerd 的 reaper 等待** | pkg/shim 在 SIGCHLD 时回收所有子进程，普通的 `cmd.Wait` 会得到 `waitid: no child processes` |

## 如何测试

- **带 KVM 的真实节点：** 两台 DigitalOcean droplet（4 vCPU，8 GB，Ubuntu 24.04），一台装 k3s 1.36，另一台装
  microk8s 1.35。事先测量了 `/dev/kvm`（`task k8s:kvm-probe`）。
- **端到端：** `deploy/k8s/e2e.sh`（19 项检查）在两者上都是 **0 失败**，各连续两轮。在退出码上植入的缺陷恰好让两项对应的检查失败。
- **单元测试：** shim（`internal/adapters/k8sshim`）覆盖率 95.4%，pod 网络（`internal/adapters/podnet`）100%；真实的
  Attach（netns、tap、tc）和按线程的 Landlock 在 Linux 上以 root 运行。
- **植入缺陷：** `task k8s:defects` 在运行时代码中植入 20 个缺陷，测试全部发现。
- 只在真实 Linux 上出现的问题以及修复方式，见[测试报告](./test-report.md#linux-com-kvm-e-kubernetes)。

## 复现

```bash
K8S_NODE=k3s task k8s:node -- create         # 带 KVM 的 droplet（存在期间约 US$ 0.07/小时）
K8S_NODE=k3s task k8s:node -- provision      # 在节点上安装 Nix、libkrun 并编译
K8S_NODE=k3s task k8s:node -- ssh 'curl -sfL https://get.k3s.io | sh -'
task k8s:e2e -- k3s                          # 重新安装运行时并运行 19 项检查
K8S_NODE=k3s task k8s:node -- destroy        # 删除 droplet 和密钥
```

microk8s 同理，使用 `K8S_NODE=microk8s` 和 `snap install microk8s --classic`。带注释的完整步骤见
[`deploy/README.md`](https://github.com/andrebassi/runner-codes/blob/main/deploy/README.md)。

## 限制

| 内容 | 原因 |
|------|------|
| `kubectl port-forward` | containerd 在 pod 的 netns 中连接 `127.0.0.1`，而程序在 VM 内监听 |
| `hostNetwork`、`hostIPC`、挂载传播 | VM 拥有独立内核 |
| 之后会变化的卷（serviceaccount token、configmap） | 卷在创建容器时被复制到共享目录 |
| 按容器的限制、按容器的 `kubectl top` | 限制作用于整台 VM；VM 的用量计入第一个容器 |
| pod vmm 的 Landlock/seccomp | 尚未支持：pod 的 vmm 没有 job VM 那样的隔离 |

完整的测试步骤见 [runbook 第 8x 节](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.zh.md)。
