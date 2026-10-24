---
title: '浏览器中的屏幕'
description: '在浏览器中提供机器的显示：virtio-gpu framebuffer，通过 virtio-input 提供键盘和鼠标，无需 VNC 客户端'
---

带 `--display` 的机器会获得一块屏幕。`rc` 在 host 的 `127.0.0.1` 上通过一个页面提供它，页面上的键盘和鼠标
输入会回传给 VM。host 上不需要 VNC 客户端，也不需要图形服务器。

## 一屏看懂 {#em-uma-tela}

```bash
rc machine create --name tela --image alpine:latest --gpu --net --display 1024x768
rc machine start  --name tela
open http://localhost:6080
rc machine create --name ui --image alpine:latest --gpu --display 1920x1080 --display-port 7000
```

在 `runner.code` 中：

```toml
gpu = true
display = "1280x800"
display_port = 6080      # 默认值
display_title = "my app"   # 页面标题栏和标签页上的名称（默认 runner.codes；命令行用 --display-title）
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

## 屏幕随窗口调整 {#tela-que-acompanha-a-janela}

查看器页面在打开时以及窗口停止变化 400 ms 后，向 host 请求空闲区域的尺寸（`POST /resize`）。VM 进程会合并这些请求
（只有最后一个生效，绝不会同时执行两个），并在机器内运行：

```bash
/etc/rc/display-resize LARGURA ALTURA
```

`rc` 不知道 guest 用的是 Xorg、Wayland 还是什么都没有：切换分辨率的是镜像，通过这个 hook。没有该文件时，什么也不会
发生，屏幕保持启动时的尺寸。在 Xorg 桌面上，hook 是 `gtf` + `xrandr`（即[示例](/cli/screen#exemplo-desktop-com-claude-code)
中的那个）：

```bash
#!/bin/sh
w=$(( $1 / 8 * 8 )); h=$2; name="${w}x${h}"
echo "$1 $2" > /run/rc-display-size          # 注销或崩溃后，会话会重新应用该尺寸
export DISPLAY=:0
until xrandr >/dev/null 2>&1; do sleep 0.5; done   # 页面可能在 Xorg 启动之前就发出请求
out=$(xrandr | awk '/ connected/ {print $1; exit}')
xrandr | grep -q "^   $name " || { xrandr --newmode "$name" $(gtf "$w" "$h" 60 | sed -n 's/^ *Modeline "[^"]*" *//p'); xrandr --addmode "$out" "$name"; }
xrandr --output "$out" --mode "$name"
```

切换期间，一些在中途抓取的帧会被丢弃（在 `vmm.log` 中显示为 `read_2d_resource`）；下一帧就已经是新尺寸。`rc` 的
libkrun 为此带了两个修复：没有它们时，第一个被丢弃的帧会让 GPU 线程崩溃，或者让屏幕永远停住而 guest 依然正常
（连续切换 30 次实测）。

## guest 中的要求 {#requisitos-no-guest}

| 用途 | 需要 |
|------|---------|
| 屏幕 | `gpu = true`；libkrunfw 的内核已自带 `virtio_gpu`（DRM/KMS，没有 fbdev：不存在 `/dev/fb0`） |
| 合成器中的键盘和鼠标 | 运行 `udev`（libinput 通过它枚举设备）；在 Alpine 上为 `eudev` + `udevadm trigger` |
| 没有 logind 的合成器 | `seatd` |
| Xorg | `modesetting` 驱动，带 `Option "AccelMethod" "none"`（2D 屏幕上没有 GL：glamor 会让 Xorg 崩溃）和 `Option "SWcursor" "true"`（硬件光标会在第一次移动鼠标时让 Xorg 卡死） |
| Debian/systemd 镜像中的 udev | 在 `udevadm trigger` 之前设置 `SYSTEMD_IGNORE_CHROOT=1`：镜像在 VM 内的 chroot 中运行，没有这个变量时 `udevadm` 会忽略请求（键盘和鼠标不会出现在 Xorg 中） |

## 安全 {#segurança}

- 查看器**只监听 host 的 `127.0.0.1`**。能访问该端口的人就能看到屏幕并控制 VM 的键盘和鼠标，
  所以不要暴露这个端口。
- 没有密码：保护手段就是只监听 loopback。

## 实测数据（macOS 27，M1 Pro） {#números-medidos-macos-27-m1-pro}

| 指标 | 值 |
|--------|-------|
| `examples/desktop-claude` 的 XFCE 桌面，第一次（init 安装 Xorg、XFCE、Firefox 和 Claude Code，有缓存） | start 耗时 28–35 s |
| 同一个桌面，从 `rc pack` 启动 | start 耗时 5.6 s；约 20 s 后终端出现在屏幕上 |
| 通过 `POST /resize` 连续切换尺寸 30 次 | 屏幕正常，0 次 panic，图像为最终尺寸 |
| 通过浏览器输入 50 个字符的文本，含 Shift 和符号 | 所有字符都能到达 |

## 限制 {#limites}

- 每台机器一块屏幕。启动时的尺寸是创建时的尺寸；之后由 guest 的 hook 切换分辨率。
- 没有音频。
- 光标由 guest 在帧内绘制：浏览器中没有独立的光标平面。
- 只在 macOS 上测试过。在 Linux 上 Venus 通过 virgl 处理 2D，但尚未测量。

要通过程序操作屏幕（截图、点击、键盘），参见[屏幕控制](/cli/screen)。
