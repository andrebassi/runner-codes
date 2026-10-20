---
title: 'GPU (Vulkan)'
description: 'microVM 如何通过 virtio-gpu/Venus 使用 host 的 GPU：架构、Nix 构建、llama stack、实测数据与诊断'
---

本页从头到尾说明 microVM 内的程序如何使用 Mac 的 GPU。只想使用的读者看第 1、2 节，需要理解原理的看第 3 到 6
节，要排查问题的看第 7、8 节。

## 1. 一句话概括 {#1-em-uma-frase}

加上 `--gpu` 后，VM 会获得一块说 **Vulkan** 的虚拟显卡（**virtio-gpu**）。guest 发出的每个 Vulkan 调用都会穿过 VM
边界，由 host 上的真实 GPU 执行。在 macOS 上，这一路要经过 **Metal**。

## 2. 用法 {#2-uso}

```bash
# 一次性：带 guest Vulkan 驱动、llama.cpp 和模型的镜像（约 6 分钟，1.4 GiB）
./bin/rc image build --stack llama

# VM 能看到 GPU 吗？
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'
# deviceName = Virtio-GPU Venus (Apple M1 Pro)

# 在 GPU 上运行的语言模型
./bin/rc run --stack llama --gpu --timeout 120s --code \
  'llama-cli -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -ngl 99 -dev Vulkan0 -no-cnv -n 64 -p "O que é uma microVM?"'

# 带 GPU 的 shell，用于探索
./bin/rc shell --stack llama --gpu
```

通过 API 使用时，在 `/api/v1/execute` 或 `/api/v1/execute/stream` 的请求体中加上 `gpu` 字段：

```bash
curl -N -X POST http://127.0.0.1:8080/api/v1/execute/stream -H 'content-type: application/json' -d '{
  "language": "llama", "gpu": true, "timeout": 120,
  "code": "llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -ngl 99 -dev Vulkan0"
}'
```

| 请求 | 结果 |
|--------|-----------|
| 在目录中标记了 `gpu: true` 的 stack 上使用 `--gpu` | VM 带 virtio-gpu；guest 中有 `/dev/dri/card0` 和 `/dev/dri/renderD128` |
| 在没有 `gpu` 的 stack 上使用 `--gpu` | 启动 VM 之前就被拒绝：`E007: stack <nome> has no GPU support`（HTTP 400） |
| 不加 `--gpu` | 与平常的 VM 完全相同；连 `/dev/dri` 都不存在 |

## 3. 一次 Vulkan 调用的路径 {#3-o-caminho-de-uma-chamada-vulkan}

```
 guest (Linux 6.12, aarch64)                    │ host (macOS, Apple Silicon)
                                                │
 programa (llama.cpp, vulkaninfo)               │
   └─ libvulkan (loader)                        │
       └─ libvulkan_virtio (Mesa "Venus")       │
           │  serializa os comandos Vulkan      │
           ▼                                    │
   /dev/dri/renderD128  (driver virtio-gpu) ────┼──▶ libkrun: dispositivo virtio-gpu (processo `rc vmm`)
                                                │      └─ virglrenderer (Venus): desserializa e chama Vulkan
                                                │           └─ MoltenVK: traduz Vulkan para Metal
                                                │                └─ Metal ─▶ GPU do Apple Silicon
```

有两条通道穿过边界：

- **命令**：共享内存中的一个队列（*ring*），Mesa 向其中写入 Vulkan 命令，virglrenderer 从中读取；
- **内存**：GPU 使用的缓冲区和纹理都是 *blob*。由 host 分配，libkrun 通过 Hypervisor.framework 的 `hv_vm_map`
  把它们映射到 guest 的 VRAM 窗口中。guest 直接读写，无需拷贝。

**为什么只有 Vulkan：** MoltenVK 翻译的是 Vulkan，而不是 OpenGL。因此 VM 用 `VIRGLRENDERER_VENUS | VIRGLRENDERER_NO_VIRGL`
这组标志打开 Venus、关闭 *virgl*（OpenGL 路径）。

## 4. host 端组件（Nix） {#4-as-peças-do-host-nix}

所有组件都来自仓库的 `flake.nix`，通过 `task krun:install`（`nix profile add .#krun`）安装。不使用 Homebrew。

| 组件 | 来源 | 原因 |
|------|-------------|---------|
| **virglrenderer** `0.10.4e-krunkit` | `nix/virglrenderer-darwin.nix`，基于 libkrun 作者的 fork（`gitlab.freedesktop.org/slp/virglrenderer`），校验 sha256 | nixpkgs 中的版本在 macOS 上以 `-Dvenus=false` 构建，而给 guest 提供 Vulkan 的恰恰是 Venus。该 fork 带有让 Venus 运行在 MoltenVK 之上的调整 |
| **MoltenVK** 1.4.2 | nixpkgs | Vulkan → Metal。virglrenderer 直接链接 `libMoltenVK.dylib`，host 上不需要 loader 或 ICD |
| **libepoxy** | nixpkgs | virglrenderer 的依赖 |
| **libkrun** 1.19.6，带 `GPU=1` | `nix/libkrun-darwin.nix` | 没有 `GPU=1` 就没有 virtio-gpu 设备，也没有 `krun_set_gpu_options2` |

构建中已经解决的两个坑，供要改动的人参考：

- Nix 的 meson hook 会打开所有 `auto` *feature*（`-Dauto_features=enabled`）。virglrenderer 的 `drm` 是 `auto`，
  会要求 libdrm，而 libdrm 只存在于 Linux，所以 derivation 传入了 `-Ddrm=disabled`；
- virglrenderer 运行在 **`rc vmm` 进程内部**（`-Drender-server=false`）。独立的 *render server* 是 Linux 上的做法。

检查已安装的内容：

```bash
./bin/rc doctor
# ok    gpu: libkrun built with virtio-gpu (Venus); use --gpu on a stack with gpu
```

## 5. 请求 `--gpu` 时 `rc` 做了什么 {#5-o-que-o-rc-faz-quando-você-pede---gpu}

1. **校验**（`internal/domain`）：`Job.CheckGPU` 检查该 stack 在目录中是否有 `gpu: true`。没有则返回 `E007`，
   对应 HTTP 400。这种拒绝是有意的：请求 GPU 的人是在测时间，悄悄退回 CPU 运行只会得到错误的数字。
2. **冷启动 VM，不走 pool**（`internal/app/pool.go`，`TakeGPU`）：带 GPU 的 VM 不会闲置在 pool 中，因为它占用一块
   VRAM 窗口和 host GPU 的一个名额。同一时间最多 **2** 台带 GPU 的 VM 存活（`DefaultMaxGPUVMs`）；第三个请求会
   等待名额，直到作业的截止时间。名额在 `Destroy` 时归还。
3. **VM spec**（`ports.VMSpec`）：`GPU: true`，`GPUVRAMMiB` 取自目录（`gpu_vram_mib`，llama stack 为 4096）。
4. **libkrun**（`internal/adapters/krun/config.go`）：在 `krun_set_exec` 之前，`Configure` 调用
   `krun_set_gpu_options2(ctx, VENUS|NO_VIRGL, vram_em_bytes)`。
5. **不带 GPU 的 libkrun**（Linux 上 nixpkgs 的版本）：根本没有 `krun_set_gpu_options2` 这个符号。binding 在注册前
   用 `Dlsym` 检查，所以库照常加载。只有请求 GPU 的 VM 会失败，返回 `ENOTSUP`。

**VRAM** 是 host 上的一段地址窗口，而不是预留的 RAM：host 只分配 guest 实际用到的部分。每台 VM 有自己的窗口。

## 6. guest 需要什么：`llama` stack {#6-o-que-o-guest-precisa-a-stack-llama}

镜像是 **Fedora 43**，不是 Alpine。原因见第 7 节。目录中的 warmup（`internal/adapters/catalog/languages.yaml`）
在构建 VM 中联网运行一次，完成以下工作：

1. **带 16 KiB 补丁的 Mesa**：`dnf copr enable slp/mesa-libkrun-vulkan` 和 `dnf install mesa-vulkan-drivers`。
   该 COPR 由 libkrun 作者维护。
2. **带 Vulkan 后端的 llama.cpp**，就地编译：tag `v0.7.0`，tarball 校验 sha256，使用
   `-DGGML_VULKAN=ON -DBUILD_SHARED_LIBS=OFF`。Fedora 的 `llama-cpp` 包没有 Vulkan 后端（只有 CPU 和 HIP）。
   只有 `llama-cli` 和 `llama-bench` 会放进 `/usr/local/bin`。
3. **模型** Qwen2.5 0.5B Instruct Q4_K_M（491 MB），放在 `/opt/models`，校验 sha256。之所以打进镜像，是因为作业
   VM 没有网络。

作业是一个 `sh` 脚本。llama.cpp 中关键的 flag：

| Flag | 作用 |
|------|--------|
| `-ngl 99` | 模型所有层都放到 GPU 上（`-ngl 0` = CPU） |
| `-dev Vulkan0` | 使用 Venus。Mesa 还会暴露 `llvmpipe`（CPU 上的软件 Vulkan），不加这个 flag 时 llama.cpp 可能把工作分摊到两者上 |

## 7. 为什么是 Fedora：16 KiB 的缺陷 {#7-por-que-fedora-o-defeito-dos-16-kib}

这是代价最大、也最容易坑到复现者的部分。

**现象**，使用标准 Mesa 时（在 Alpine edge 的 `mesa-vulkan-virtio` 上实测）：

```
/dev/dri/renderD128 existe          ← o dispositivo virtual subiu
krun_vmm::macos::vstate] Error removing memory map: MemoryUnmap
krun_vmm::macos::vstate] Error adding memory map: MemoryMap
vkCreateInstance failed with ERROR_OUT_OF_HOST_MEMORY
```

**原因**：Apple Silicon 的内存页是 **16 KiB**，`hv_vm_map` 只能映射按 16 KiB 对齐的区域。guest 内核使用 4 KiB 的页
（`getconf PAGESIZE` = 4096）。标准 Mesa 申请的 blob，包括 `vkCreateInstance` 中创建的命令 *ring*，都按 4 KiB
对齐。映射失败，Vulkan 根本打不开。

**无效的做法**：加大 VRAM。用 2、8、16 GiB 实测过，错误完全相同。

**修复**：COPR 中的补丁 `0001-virtio-vulkan-force-16k-alignment-for-allocations` 把以下内容向上取整到 16 KiB：

- 所有内存分配的大小（`vn_device_memory.c`）；
- blob（`vn_renderer_virtgpu.c`）；
- *ring*（`vn_ring.c`）。

现成的版本只有 Fedora 包，而且二进制基于 glibc，所以无法用于 Alpine（musl）镜像。

## 8. 诊断 {#8-diagnóstico}

| 现象 | 可能原因 | 处理方法 |
|---------|----------------|-------------|
| `E007: stack X has no GPU support` | 该 stack 没有 `gpu: true` | 使用 `llama` stack，或在镜像带补丁版 Mesa 的 stack 上声明 `gpu` |
| `krun_set_gpu_options2 failed: errno 45` (ENOTSUP) | libkrun 编译时没有 `GPU=1` | `task krun:install`（macOS）；Linux 上 nixpkgs 的版本不带 GPU |
| 加了 `--gpu` 但 `/dev/dri` 不存在 | `Configure` 没有挂上设备 | `rc doctor` 应显示 `ok gpu`；e2e 测试 `TestGPUVulkan` 正好能发现这个问题 |
| `vkCreateInstance ... ERROR_OUT_OF_HOST_MEMORY` + `Error adding memory map` | Mesa 没有 16 KiB 补丁 | 使用 COPR `slp/mesa-libkrun-vulkan` 中 Mesa 的镜像（第 7 节） |
| `vulkaninfo` 只列出 `llvmpipe` | Venus 没有加载，只剩软件 Vulkan | 检查上面那条消息；`VK_LOADER_DEBUG=all vulkaninfo` 会显示哪个 ICD 失败 |
| GPU 很慢或卡住 | 另一台带 GPU 的 VM 或其他应用在争用 GPU | 最多 2 台带 GPU 的 VM；查看“活动监视器 › GPU” |

无害的消息：`Error opening virtio-gpu device for Asahi native context`。Mesa 会先尝试 Asahi 原生驱动，找不到就回退到
Venus。

## 9. 实测数据 {#9-números-medidos}

macOS 27，Apple M1 Pro（16 GB），2026/10/19。在 VM 内运行 `llama-bench`，Qwen2.5 0.5B Q4_K_M，4 vCPU，4 轮：

| 测试 | GPU（Venus，`-ngl 99 -dev Vulkan0`） | CPU（`-ngl 0`） |
|-------|-------------------------------------|----------------|
| 处理 prompt（pp128） | **893–1.365 t/s** | 193–264 t/s |
| 生成文本（tg32/tg64） | 47–58 t/s | 46–123 t/s |

如何解读：

- **prompt**：GPU 快 3.5 到 7 倍，因为这是批处理（一次 128 个 token），正是 GPU 擅长并行的工作；
- **生成**：一次一个 token，每个 token 都要走一趟 guest → virtio → virglrenderer → MoltenVK → Metal 的往返。
  在 0.5B 的模型上，这段延迟比计算本身更重，CPU 打平甚至更快。更大的模型可能会反过来，但没有实测；
- CPU 受 host 负载影响波动很大（46 到 123 t/s）。GPU 更稳定。

e2e 测试 `TestGPULlama` 要求使用的设备是 Venus，并且 GPU 上的 prompt 处理至少比 CPU 快 2 倍。出于上述原因，它不
检查生成速度。

## 10. 限制与未测试项 {#10-limites-e-o-que-não-foi-testado}

| 项目 | 状态 |
|-------|----------|
| macOS Apple Silicon | 端到端**已测试**（`TestGPUVulkan`、`TestGPULlama`，连续两轮） |
| Linux（KVM） | **未测试**：nixpkgs 的 libkrun 不带 `GPU=1`，也没有带 KVM 和 GPU 的 Linux host 可用。Linux 上 virglrenderer 使用 host 的 Mesa 和 *render server* |
| Linux：进程隔离 | 带 GPU 的 VM 运行时**没有 Landlock/seccomp**：virglrenderer 要打开 `/dev/dri`，并用 `execve` 启动 *render server*，而策略会拒绝这些操作。放开这些需要在带 GPU 的 Linux host 上实测 |
| CUDA | **不支持**。在 VM 中使用 CUDA 需要把 CUDA API *remoting* 到 host 的 NVIDIA 驱动，需要 Linux host 和 NVIDIA 显卡 |
| Windows | 不在范围内（上游 libkrun 没有 WHP） |
| 隔离性 | CPU、内存和磁盘仍由 VM 隔离，但 **GPU 是共享的**，按 host 进程共享，而不是按 VM 边界隔离。不要把 GPU 用于互相敌对的第三方代码 |

## 11. 各部分在代码中的位置 {#11-onde-está-cada-coisa-no-código}

| 文件 | 内容 |
|---------|-----------|
| `nix/virglrenderer-darwin.nix`、`nix/libkrun-darwin.nix`、`flake.nix` | host 端构建 |
| `internal/adapters/krun/config.go` | `gpuVenusFlags`、`DefaultGPUVRAMMiB`、`Configure` 中的调用 |
| `internal/adapters/krun/binding.go` | 可选符号，`HasGPU()` |
| `internal/adapters/krun/sandbox.go` | 带 GPU 的 VM 不受 Landlock/seccomp 约束 |
| `internal/domain/domain.go` | `Language.GPU`、`Job.GPU`、`CheckGPU`、`ErrNoGPU`（E007） |
| `internal/app/pool.go` | `TakeGPU`、`DefaultMaxGPUVMs` |
| `internal/adapters/catalog/languages.yaml` | `llama` stack |
| `e2e/gpu_test.go` | `TestGPUVulkan`、`TestGPULlama` |
| `docs/stories/6.1.gpu-venus.md` | 对应的 story，记录了发现的每一个缺陷 |
