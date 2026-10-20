---
title: '浏览器中的屏幕'
description: '在浏览器中提供机器的显示：virtio-gpu framebuffer，通过 virtio-input 提供键盘和鼠标，无需 VNC 客户端'
---

带 `--display` 的机器会获得一块屏幕。`rc` 在 host 的 `127.0.0.1` 上通过一个页面提供它，页面上的键盘和鼠标
输入会回传给 VM。host 上不需要 VNC 客户端，也不需要图形服务器。

## 一屏看懂 {#em-uma-tela}

```bash
rc machine run -d --name desktop -s examples/desktop/runner.code   # Weston，1280x800
open http://localhost:6080
rc machine create --name ui --image alpine:latest --gpu --display 1920x1080 --display-port 7000
```

在 `runner.code` 中：

```toml
gpu = true
display = "1280x800"
display_port = 6080      # 默认值
```

## 工作原理 {#como-funciona}

```
guest                                    机器的 `rc vmm` 进程（host）
  KMS / Wayland / X  ─▶ virtio-gpu 2D ─▶ display 后端（C 回调，purego）
                                           └─ 共享屏幕（RGBA，版本号）
                                                └─ GET /frame?after=N  (JPEG, long-poll) ─▶ 页面
  evdev (libinput) ◀─ virtio-input ◀─── 键盘和 tablet 队列 ◀─ POST /input（evdev 事件） ◀─ 页面
```

- **输出：**
  - libkrun 调用我们的后端（`krun_set_display_backend`）：`configure_scanout`、`alloc_frame` 和
    `present_frame`；
  - 缓冲区是 `mmap` 内存，不受 GC 管理；
  - 每个呈现的帧都会被转换为 RGBA（包括 BGRX、XRGB 等格式转换），并唤醒等待者；
  - 查看器只在有新帧时才用 JPEG 响应 `GET /frame`。
- **输入：**
  - 有两个 `virtio-input` 设备（`krun_add_input_device`）：`runner.code keyboard` 和 `runner.code
    tablet`，后者是绝对坐标鼠标，带三个按键和滚轮；
  - 页面把 `KeyboardEvent.code` 转换为 evdev 的 keycode，并根据事件状态同步 Ctrl、Shift、Alt 和 Meta；
  - host 每次交付一个数据包（直到 `SYN_REPORT`），包与包之间间隔 3 ms。
- **用普通 HTTP 而不是 VNC/WebSocket：** 在 loopback 上，long-poll 能超过每秒 30 帧，而且页面
  不依赖任何库。

## 取舍：2D 屏幕，没有 Vulkan {#a-troca-tela-2d-sem-vulkan}

在 macOS 上，带 Venus（Vulkan）的 virtio-gpu 不处理 2D 资源。

- **不打补丁时会怎样：** guest 为屏幕创建的资源会失败（实测：
  `TRANSFER_TO_HOST_2D → ERR_UNSPEC`、`SET_SCANOUT → INVALID_RESOURCE_ID`），没有任何帧输出。
- **补丁：** `rc` 的 libkrun（`nix/libkrun-darwin.nix`）接受 `KRUN_GPU_BACKEND=2d`，它把 rutabaga 的组件
  替换为 `Rutabaga2D`，一个由 CPU 绘制的 framebuffer。带屏幕的 VM 进程会设置这个变量。
- **代价：** 带屏幕的 VM 没有 Vulkan。
  - 用 CPU 绘制的合成器可以工作（使用 `--renderer=pixman` 的 Weston、`modetest`、使用 framebuffer 的 X）。
  - 需要 GLES 的合成器（Hyprland、GNOME Shell）会退回到 `llvmpipe`，很慢。
- **不带屏幕的 VM：** 继续使用 Venus，供 `llama.cpp` 等使用。

## guest 中的要求 {#requisitos-no-guest}

| 用途 | 需要 |
|------|---------|
| 屏幕 | `gpu = true`；libkrunfw 的内核已自带 `virtio_gpu`（DRM/KMS，没有 fbdev：不存在 `/dev/fb0`） |
| 合成器中的键盘和鼠标 | 运行 `udev`（libinput 通过它枚举设备）；在 Alpine 上为 `eudev` + `udevadm trigger` |
| 没有 logind 的合成器 | `seatd` |

示例 [`desktop`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop/runner.code) 在
`cmd` 中完成了这些设置。

## 安全 {#segurança}

- 查看器**只监听 host 的 `127.0.0.1`**。能访问该端口的人就能看到屏幕并控制 VM 的键盘和鼠标，
  所以不要暴露这个端口。
- 没有密码：保护手段就是只监听 loopback。

## 实测数据（macOS 27，M1 Pro） {#números-medidos-macos-27-m1-pro}

| 指标 | 值 |
|--------|-------|
| desktop 示例的 `machine run -d`（init 已完成） | 5 s 到 Weston 启动 |
| 第一次（init 安装 Weston、eudev、seatd） | 12 s |
| 通过浏览器输入 50 个字符的文本，含 Shift 和符号 | 所有字符都能到达 |

## 限制 {#limites}

- 每台机器一块屏幕；尺寸在创建时固定。
- 没有音频。
- 光标由 guest 在帧内绘制：浏览器中没有独立的光标平面。
- 只在 macOS 上测试过。在 Linux 上 Venus 通过 virgl 处理 2D，但尚未测量。
