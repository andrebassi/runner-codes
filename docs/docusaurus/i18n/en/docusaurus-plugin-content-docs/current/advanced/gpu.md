---
title: 'GPU (Vulkan)'
description: 'How the microVM uses the host GPU through virtio-gpu/Venus: architecture, Nix build, the llama stack, measurements and diagnostics'
---

This page explains, end to end, how a program inside the microVM uses the Mac's GPU. It is meant for people who just
want to use it (sections 1 and 2), for those who need to understand it (3 to 6), and for those who will debug it (7 and 8).

## 1. In one sentence

With `--gpu`, the VM gets a virtual graphics card (**virtio-gpu**) that speaks **Vulkan**. Every Vulkan call from the
guest crosses the VM boundary and is executed by the host's real GPU. On macOS, this goes through **Metal**.

## 2. Usage

```bash
# once: the image with the guest Vulkan driver, llama.cpp and the model (~6 min, 1.4 GiB)
./bin/rc image build --stack llama

# does the VM see the GPU?
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'
# deviceName = Virtio-GPU Venus (Apple M1 Pro)

# a language model running on the GPU
./bin/rc run --stack llama --gpu --timeout 120s --code \
  'llama-cli -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -ngl 99 -dev Vulkan0 -no-cnv -n 64 -p "O que é uma microVM?"'

# shell with GPU, for exploring
./bin/rc shell --stack llama --gpu
```

Through the API, with the `gpu` field in the body of `/api/v1/execute` or `/api/v1/execute/stream`:

```bash
curl -N -X POST http://127.0.0.1:8080/api/v1/execute/stream -H 'content-type: application/json' -d '{
  "language": "llama", "gpu": true, "timeout": 120,
  "code": "llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -ngl 99 -dev Vulkan0"
}'
```

| Request | Result |
|---------|--------|
| `--gpu` on a stack with `gpu: true` in the catalog | VM with virtio-gpu; `/dev/dri/card0` and `/dev/dri/renderD128` in the guest |
| `--gpu` on a stack without `gpu` | rejected before any VM boots: `E007: stack <nome> has no GPU support` (HTTP 400) |
| no `--gpu` | the usual VM; `/dev/dri` does not even exist |

## 3. The path of a Vulkan call

```
 guest (Linux 6.12, aarch64)                    │ host (macOS, Apple Silicon)
                                                │
 program (llama.cpp, vulkaninfo)                │
   └─ libvulkan (loader)                        │
       └─ libvulkan_virtio (Mesa "Venus")       │
           │  serializes the Vulkan commands    │
           ▼                                    │
   /dev/dri/renderD128  (virtio-gpu driver) ────┼──▶ libkrun: virtio-gpu device (`rc vmm` process)
                                                │      └─ virglrenderer (Venus): deserializes and calls Vulkan
                                                │           └─ MoltenVK: translates Vulkan to Metal
                                                │                └─ Metal ─▶ Apple Silicon GPU
```

Two channels cross the boundary:

- **commands**: a queue (*ring*) in shared memory, where Mesa writes the Vulkan commands and virglrenderer reads
  them;
- **memory**: the buffers and textures the GPU uses are *blobs*. The host allocates them, and libkrun maps them into
  the guest's VRAM window with `hv_vm_map`, from Hypervisor.framework. The guest reads and writes them without copying.

**Why Vulkan only:** MoltenVK translates Vulkan, not OpenGL. That is why the VM turns Venus on and *virgl* (the OpenGL
path) off, with the flags `VIRGLRENDERER_VENUS | VIRGLRENDERER_NO_VIRGL`.

## 4. The host pieces (Nix)

Everything comes from the repository's `flake.nix`, through `task krun:install` (`nix profile add .#krun`). Nothing
comes from Homebrew.

| Piece | Where it comes from | Why |
|-------|---------------------|-----|
| **virglrenderer** `0.10.4e-krunkit` | `nix/virglrenderer-darwin.nix`, from the libkrun author's fork (`gitlab.freedesktop.org/slp/virglrenderer`), with a verified sha256 | the nixpkgs one is built with `-Dvenus=false` on macOS, and Venus is exactly what gives the guest Vulkan. The fork carries the adjustments for Venus to run on top of MoltenVK |
| **MoltenVK** 1.4.2 | nixpkgs | Vulkan → Metal. virglrenderer links directly against `libMoltenVK.dylib`, with no loader or ICD on the host |
| **libepoxy** | nixpkgs | virglrenderer dependency |
| **libkrun** 1.19.6 with `GPU=1` | `nix/libkrun-darwin.nix` | without `GPU=1` there is no virtio-gpu device and no `krun_set_gpu_options2` |

Two pitfalls solved in the build, for anyone who touches it:

- Nix's meson hook enables every `auto` *feature* (`-Dauto_features=enabled`). virglrenderer's `drm` is `auto` and
  would require libdrm, which only exists on Linux, so the derivation passes `-Ddrm=disabled`;
- virglrenderer runs **inside the `rc vmm` process** (`-Drender-server=false`). The separate *render server* is a
  Linux thing.

Checking what is installed:

```bash
./bin/rc doctor
# ok    gpu: libkrun built with virtio-gpu (Venus); use --gpu on a stack with gpu
```

## 5. What `rc` does when you ask for `--gpu`

1. **Validation** (`internal/domain`): `Job.CheckGPU` checks whether the stack has `gpu: true` in the catalog. If it
   does not, it returns `E007`, which becomes HTTP 400. The rejection is deliberate: whoever asks for a GPU is
   measuring time, and silently running on the CPU would produce a wrong number.
2. **Cold VM, outside the pool** (`internal/app/pool.go`, `TakeGPU`): a GPU VM does not sit idle in the pool, because
   it holds a VRAM window and a slot on the host GPU. At most **2** GPU VMs are alive at the same time
   (`DefaultMaxGPUVMs`); a third request waits for a slot until the job's deadline. The slot is returned on `Destroy`.
3. **VM spec** (`ports.VMSpec`): `GPU: true` and `GPUVRAMMiB` coming from the catalog (`gpu_vram_mib`, 4096 on the
   llama stack).
4. **libkrun** (`internal/adapters/krun/config.go`): before `krun_set_exec`, `Configure` calls
   `krun_set_gpu_options2(ctx, VENUS|NO_VIRGL, vram_em_bytes)`.
5. **libkrun without GPU** (the nixpkgs one on Linux): the `krun_set_gpu_options2` symbol does not even exist. The
   binding checks it with `Dlsym` before registering, so the library loads normally. Only a VM that asks for a GPU
   fails, with `ENOTSUP`.

**VRAM** is a host address window, not reserved RAM: the host only allocates what the guest actually uses. Each VM
gets its own window.

## 6. What the guest needs: the `llama` stack

The image is **Fedora 43**, not Alpine. Section 7 explains why. The catalog warmup
(`internal/adapters/catalog/languages.yaml`) runs once, in the build VM, with network access, and does the following:

1. **Mesa with the 16 KiB patch**: `dnf copr enable slp/mesa-libkrun-vulkan` and `dnf install mesa-vulkan-drivers`.
   The COPR belongs to the libkrun author.
2. **llama.cpp with the Vulkan backend**, compiled right there: tag `v0.7.0`, tarball verified by sha256, with
   `-DGGML_VULKAN=ON -DBUILD_SHARED_LIBS=OFF`. Fedora's `llama-cpp` package has no Vulkan backend (CPU and HIP only).
   Only `llama-cli` and `llama-bench` go to `/usr/local/bin`.
3. **Model** Qwen2.5 0.5B Instruct Q4_K_M (491 MB) in `/opt/models`, with its sha256 verified. It is baked into the
   image because the job VM has no network.

The job is an `sh` script. The llama.cpp flags that matter:

| Flag | Effect |
|------|--------|
| `-ngl 99` | all model layers on the GPU (`-ngl 0` = CPU) |
| `-dev Vulkan0` | uses Venus. Mesa also exposes `llvmpipe` (software Vulkan, on the CPU), and without this flag llama.cpp may split the work between the two |

## 7. Why Fedora: the 16 KiB defect

This is the part that cost the most and that bites hardest anyone trying to reproduce it.

**Symptom**, with stock Mesa (measured on Alpine edge, `mesa-vulkan-virtio`):

```
/dev/dri/renderD128 exists          ← the virtual device came up
krun_vmm::macos::vstate] Error removing memory map: MemoryUnmap
krun_vmm::macos::vstate] Error adding memory map: MemoryMap
vkCreateInstance failed with ERROR_OUT_OF_HOST_MEMORY
```

**Cause**: the Apple Silicon memory page is **16 KiB**, and `hv_vm_map` only maps regions aligned to 16 KiB. The
guest kernel uses 4 KiB pages (`getconf PAGESIZE` = 4096). Stock Mesa requests blobs, including the command *ring*
created in `vkCreateInstance`, aligned to 4 KiB. The mapping fails, and Vulkan never opens.

**What does not fix it**: increasing VRAM. Measured with 2, 8 and 16 GiB, and the error is identical.

**Fix**: the COPR patch `0001-virtio-vulkan-force-16k-alignment-for-allocations` rounds up to 16 KiB:

- the size of every memory allocation (`vn_device_memory.c`);
- the blobs (`vn_renderer_virtgpu.c`);
- the *ring* (`vn_ring.c`).

It only exists ready-made as a Fedora package, and the binary is glibc, so it does not work in an Alpine (musl) image.

## 8. Diagnostics

| Symptom | Likely cause | What to do |
|---------|--------------|------------|
| `E007: stack X has no GPU support` | the stack does not have `gpu: true` | use the `llama` stack, or declare `gpu` on a stack whose image has Mesa with the patch |
| `krun_set_gpu_options2 failed: errno 45` (ENOTSUP) | libkrun built without `GPU=1` | `task krun:install` (macOS); on Linux the nixpkgs one comes without GPU |
| `/dev/dri` does not exist with `--gpu` | `Configure` did not attach the device | `rc doctor` should say `ok gpu`; the e2e `TestGPUVulkan` catches exactly this |
| `vkCreateInstance ... ERROR_OUT_OF_HOST_MEMORY` + `Error adding memory map` | Mesa without the 16 KiB patch | image with Mesa from the `slp/mesa-libkrun-vulkan` COPR (section 7) |
| `vulkaninfo` lists only `llvmpipe` | Venus did not load and only software Vulkan is left | check the message above; `VK_LOADER_DEBUG=all vulkaninfo` shows which ICD failed |
| slow or hanging GPU | another GPU VM or another app competing for the GPU | at most 2 GPU VMs; see Activity Monitor › GPU |

Harmless message: `Error opening virtio-gpu device for Asahi native context`. Mesa first tries the native Asahi
driver, does not find it, and falls back to Venus.

## 9. Measured numbers

macOS 27, Apple M1 Pro (16 GB), 10/19/2026. `llama-bench` inside the VM, Qwen2.5 0.5B Q4_K_M, 4 vCPU, 4 runs:

| Test | GPU (Venus, `-ngl 99 -dev Vulkan0`) | CPU (`-ngl 0`) |
|------|-------------------------------------|----------------|
| prompt processing (pp128) | **893–1,365 t/s** | 193–264 t/s |
| text generation (tg32/tg64) | 47–58 t/s | 46–123 t/s |

How to read them:

- **prompt**: the GPU is 3.5 to 7 times faster, because it is batch work (128 tokens at once), exactly what the GPU
  parallelizes;
- **generation**: one token at a time, and each token pays the round trip guest → virtio → virglrenderer → MoltenVK →
  Metal. On a 0.5B model this latency weighs more than the math, and the CPU ties or wins. A larger model tends to
  reverse this, but that was not measured;
- the CPU varies a lot (46 to 123 t/s) with host load. The GPU is more stable.

The e2e `TestGPULlama` requires that the device used is Venus and that the prompt on the GPU comes out at least 2 times
faster than on the CPU. It does not check generation, for the reason above.

## 10. Limits and what was not tested

| Item | Status |
|------|--------|
| macOS Apple Silicon | **tested** end to end (`TestGPUVulkan`, `TestGPULlama`, two consecutive runs) |
| Linux (KVM) | **not tested**: the nixpkgs libkrun ships without `GPU=1`, and no Linux host with KVM and a GPU is available. On Linux, virglrenderer uses the host's Mesa and the *render server* |
| Linux: confinement | a GPU VM runs **without Landlock/seccomp**: virglrenderer opens `/dev/dri` and starts the *render server* with `execve`, which the policy denies. Allowing that requires measuring on a Linux host with a GPU |
| CUDA | **not supported**. CUDA in the VM would require *remoting* the CUDA API to the host's NVIDIA driver, with a Linux host and an NVIDIA card |
| Windows | out of scope (upstream libkrun has no WHP) |
| Isolation | CPU, memory and disk are still isolated by the VM, but the **GPU is shared** per host process, not by a VM boundary. Do not use the GPU for mutually hostile third-party code |

## 11. Where everything lives in the code

| File | What it has |
|------|-------------|
| `nix/virglrenderer-darwin.nix`, `nix/libkrun-darwin.nix`, `flake.nix` | the host build |
| `internal/adapters/krun/config.go` | `gpuVenusFlags`, `DefaultGPUVRAMMiB`, the call in `Configure` |
| `internal/adapters/krun/binding.go` | optional symbol, `HasGPU()` |
| `internal/adapters/krun/sandbox.go` | GPU VM outside Landlock/seccomp |
| `internal/domain/domain.go` | `Language.GPU`, `Job.GPU`, `CheckGPU`, `ErrNoGPU` (E007) |
| `internal/app/pool.go` | `TakeGPU`, `DefaultMaxGPUVMs` |
| `internal/adapters/catalog/languages.yaml` | the `llama` stack |
| `e2e/gpu_test.go` | `TestGPUVulkan`, `TestGPULlama` |
| `docs/stories/6.1.gpu-venus.md` | the story, with the log of every defect found |
