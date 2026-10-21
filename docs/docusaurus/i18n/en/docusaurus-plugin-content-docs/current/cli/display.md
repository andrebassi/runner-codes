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

## Guest requirements

| For | Needs |
|------|---------|
| the display | `gpu = true`; the libkrunfw kernel already ships `virtio_gpu` (DRM/KMS, no fbdev: there is no `/dev/fb0`) |
| keyboard and mouse in a compositor | `udev` running (libinput enumerates through it); on Alpine, `eudev` + `udevadm trigger` |
| a compositor without logind | `seatd` |

## Security

- The viewer listens **only on the host's `127.0.0.1`**. Whoever reaches the port sees the screen and controls the VM's keyboard and mouse,
  so do not expose the port.
- There is no password: the protection is listening only on loopback.

## Measured numbers (macOS 27, M1 Pro)

| Measurement | Value |
|--------|-------|
| `machine run -d` of the desktop example (init already done) | 5 s until Weston |
| first time (init installs Weston, eudev, seatd) | 12 s |
| 50-character text with Shift and symbols through the browser | every character arrives |

## Limits

- One display per machine; the size is fixed at creation.
- No audio.
- The cursor is drawn by the guest, inside the frame: there is no separate cursor plane in the browser.
- Tested on macOS only. On Linux, Venus serves 2D through virgl, but it has not been measured.
