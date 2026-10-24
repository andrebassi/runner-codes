---
title: 'Display in the browser'
description: 'A machine display served in the browser: virtio-gpu framebuffer, keyboard and mouse over virtio-input, no VNC client'
---

A machine with `--display` gets a screen. `rc` serves it on a page on the host's `127.0.0.1`, and the page's keyboard and mouse
go back to the VM. No VNC client and no graphical server are needed on the host.

## At a glance

```bash
rc machine create --name tela --image alpine:latest --gpu --net --display 1024x768
rc machine start  --name tela
open http://localhost:6080
rc machine create --name ui --image alpine:latest --gpu --display 1920x1080 --display-port 7000
```

In `runner.code`:

```toml
gpu = true
display = "1280x800"
display_port = 6080      # default
display_title = "my app"   # name on the page bar and tab (default runner.codes; on the CLI, --display-title)
```

## How it works

```
guest                                    process `rc vmm` of the machine (host)
  KMS / Wayland / X  ─▶ virtio-gpu 2D ─▶ display backend (C callbacks, purego)
                                           └─ shared screen (RGBA, version)
                                                └─ GET /frame?after=N  (JPEG, long-poll) ─▶ page
  evdev (libinput) ◀─ virtio-input ◀─── keyboard and tablet queues ◀─ POST /input (evdev events) ◀─ page
```

- **Output:**
  - libkrun calls our backend (`krun_set_display_backend`): `configure_scanout`, `alloc_frame` and
    `present_frame`;
  - the buffers are `mmap` memory, outside the GC;
  - each presented frame becomes RGBA, with format conversion (BGRX, XRGB...), and wakes up whoever is waiting;
  - the viewer answers `GET /frame` with a JPEG only when there is a new frame.
- **Input:**
  - there are two `virtio-input` devices (`krun_add_input_device`): `runner.code keyboard` and `runner.code
    tablet`, the latter with an absolute mouse, three buttons and a wheel;
  - the page converts `KeyboardEvent.code` into an evdev keycode and syncs Ctrl, Shift, Alt and Meta from the event
    state;
  - the host delivers one packet at a time (up to the `SYN_REPORT`), with 3 ms between packets.
- **Plain HTTP instead of VNC/WebSocket:** on loopback, the long-poll exceeds 30 frames per second, and the page
  depends on no library.

## The trade-off: 2D display, no Vulkan

On macOS, virtio-gpu with Venus (Vulkan) does not serve 2D resources.

- **What happens without the patch:** the resource the guest creates for the screen fails (measured:
  `TRANSFER_TO_HOST_2D → ERR_UNSPEC`, `SET_SCANOUT → INVALID_RESOURCE_ID`), and no frame comes out.
- **The patch:** the `rc` libkrun (`nix/libkrun-darwin.nix`) accepts `KRUN_GPU_BACKEND=2d`, which swaps the
  rutabaga component for `Rutabaga2D`, a CPU framebuffer. The process of a VM with a display sets that variable.
- **The cost:** a VM with a display has no Vulkan.
  - Compositors that render on the CPU work (Weston with `--renderer=pixman`, `modetest`, X with a framebuffer).
  - Compositors that require GLES (Hyprland, GNOME Shell) fall back to `llvmpipe`, which is slow.
- **VM without a display:** keeps Venus, for `llama.cpp` and the rest.

## Screen that follows the window

The viewer page asks the host for the size of the free area, when it opens and 400 ms after the window stops changing
(`POST /resize`). The VM process coalesces the requests (only the last one counts, never two at once) and runs inside the
machine:

```bash
/etc/rc/display-resize LARGURA ALTURA
```

`rc` does not know whether the guest uses Xorg, Wayland or nothing: the image changes the resolution, through the hook.
Without the file, nothing happens and the screen stays at the boot size. On an Xorg desktop, the hook is a `gtf` +
`xrandr` (the one from the [example](/cli/screen#example-desktop-with-claude-code)):

```bash
#!/bin/sh
w=$(( $1 / 8 * 8 )); h=$2; name="${w}x${h}"
echo "$1 $2" > /run/rc-display-size          # the session reapplies it after a logout or crash
export DISPLAY=:0
until xrandr >/dev/null 2>&1; do sleep 0.5; done   # the page may ask before Xorg is up
out=$(xrandr | awk '/ connected/ {print $1; exit}')
xrandr | grep -q "^   $name " || { xrandr --newmode "$name" $(gtf "$w" "$h" 60 | sed -n 's/^ *Modeline "[^"]*" *//p'); xrandr --addmode "$out" "$name"; }
xrandr --output "$out" --mode "$name"
```

During a change, a few frames caught midway are discarded (they show up in `vmm.log` as `read_2d_resource`); the next
frame already comes out at the new size. The `rc` libkrun ships two fixes for this: without them, the first discarded
frame brought down the GPU thread, or left the screen frozen forever with a healthy guest (measured with 30 changes in a
row).

## Guest requirements

| For | Needs |
|------|---------|
| the display | `gpu = true`; the libkrunfw kernel already ships `virtio_gpu` (DRM/KMS, no fbdev: there is no `/dev/fb0`) |
| keyboard and mouse in a compositor | `udev` running (libinput enumerates through it); on Alpine, `eudev` + `udevadm trigger` |
| a compositor without logind | `seatd` |
| Xorg | `modesetting` driver with `Option "AccelMethod" "none"` (no GL on the 2D screen: glamor brings Xorg down) and `Option "SWcursor" "true"` (the hardware cursor froze Xorg on the first mouse movement) |
| udev in a Debian/systemd image | `SYSTEMD_IGNORE_CHROOT=1` before `udevadm trigger`: the image runs in a chroot inside the VM, and `udevadm` ignores the request without that variable (keyboard and mouse do not show up in Xorg) |

## Security

- The viewer listens **only on the host's `127.0.0.1`**. Whoever reaches the port sees the screen and controls the VM's keyboard and mouse,
  so do not expose the port.
- There is no password: the protection is listening only on loopback.

## Measured numbers (macOS 27, M1 Pro)

| Measurement | Value |
|--------|-------|
| XFCE desktop from `examples/desktop-claude`, first time (init installs Xorg, XFCE, Firefox and Claude Code, with cache) | 28–35 s of start |
| the same desktop from an `rc pack` | 5.6 s of start; terminal on screen ~20 s later |
| 30 size changes in a row through `POST /resize` | screen up, 0 panics, image at the final size |
| 50-character text with Shift and symbols through the browser | every character arrives |

## Limits

- One display per machine. The boot size is the one from creation; after that, the guest hook changes the resolution.
- No audio.
- The cursor is drawn by the guest, inside the frame: there is no separate cursor plane in the browser.
- Tested on macOS only. On Linux, Venus serves 2D through virgl, but it has not been measured.

To operate the screen programmatically (capture, click, keyboard), see [Screen control](/cli/screen).
