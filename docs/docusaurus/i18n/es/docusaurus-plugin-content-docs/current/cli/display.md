---
title: 'Pantalla en el navegador'
description: 'Display de una máquina servido en el navegador: framebuffer virtio-gpu, teclado y mouse vía virtio-input, sin cliente VNC'
---

Una máquina con `--display` obtiene una pantalla. `rc` la sirve en una página en el `127.0.0.1` del host, y el
teclado y el mouse de la página vuelven a la VM. No hace falta un cliente VNC ni un servidor gráfico en el host.

## En una pantalla

```bash
rc machine create --name tela --image alpine:latest --gpu --net --display 1024x768
rc machine start  --name tela
open http://localhost:6080
rc machine create --name ui --image alpine:latest --gpu --display 1920x1080 --display-port 7000
```

En el `runner.code`:

```toml
gpu = true
display = "1280x800"
display_port = 6080      # por defecto
```

## Cómo funciona

```
guest                                    proceso `rc vmm` de la máquina (host)
  KMS / Wayland / X  ─▶ virtio-gpu 2D ─▶ backend de display (callbacks C, purego)
                                           └─ pantalla compartida (RGBA, versión)
                                                └─ GET /frame?after=N  (JPEG, long-poll) ─▶ página
  evdev (libinput) ◀─ virtio-input ◀─── colas de teclado y tablet ◀─ POST /input (eventos evdev) ◀─ página
```

- **Salida:**
  - libkrun llama a nuestro backend (`krun_set_display_backend`): `configure_scanout`, `alloc_frame` y
    `present_frame`;
  - los buffers son memoria `mmap`, fuera del GC;
  - cada cuadro presentado se convierte a RGBA, con la conversión de formato (BGRX, XRGB...), y despierta a quien
    espera;
  - el visor responde al `GET /frame` con JPEG solo cuando hay un cuadro nuevo.
- **Entrada:**
  - son dos dispositivos `virtio-input` (`krun_add_input_device`): `runner.code keyboard` y `runner.code
    tablet`, este con mouse absoluto, tres botones y rueda;
  - la página convierte `KeyboardEvent.code` en keycode de evdev y sincroniza Ctrl, Shift, Alt y Meta según el estado
    del evento;
  - el host entrega un paquete a la vez (hasta el `SYN_REPORT`), con 3 ms entre paquetes.
- **HTTP simple en lugar de VNC/WebSocket:** en el loopback, el long-poll supera los 30 cuadros por segundo, y la
  página no depende de ninguna biblioteca.

## El compromiso: pantalla 2D, sin Vulkan

En macOS, virtio-gpu con Venus (Vulkan) no atiende recursos 2D.

- **Qué sucede sin el parche:** el recurso que el guest crea para la pantalla falla (medido:
  `TRANSFER_TO_HOST_2D → ERR_UNSPEC`, `SET_SCANOUT → INVALID_RESOURCE_ID`), y no sale ningún cuadro.
- **El parche:** la libkrun de `rc` (`nix/libkrun-darwin.nix`) acepta `KRUN_GPU_BACKEND=2d`, que cambia el componente
  de rutabaga por `Rutabaga2D`, un framebuffer por CPU. El proceso de una VM con pantalla activa esa variable.
- **El precio:** la VM con pantalla no tiene Vulkan.
  - Un compositor que dibuja por CPU funciona (Weston con `--renderer=pixman`, `modetest`, X con framebuffer).
  - Un compositor que exige GLES (Hyprland, GNOME Shell) cae en `llvmpipe`, que es lento.
- **VM sin pantalla:** sigue con Venus, para `llama.cpp` y lo demás.

## Requisitos en el guest

| Para | Necesita |
|------|---------|
| la pantalla | `gpu = true`; el kernel de libkrunfw ya trae `virtio_gpu` (DRM/KMS, sin fbdev: no hay `/dev/fb0`) |
| teclado y mouse en un compositor | `udev` en ejecución (libinput enumera a través de él); en Alpine, `eudev` + `udevadm trigger` |
| compositor sin logind | `seatd` |

## Seguridad

- El visor escucha **solo en el `127.0.0.1`** del host. Quien alcance el puerto ve la pantalla y controla el teclado y
  el mouse de la VM, así que no exponga el puerto.
- No hay contraseña: la protección es escuchar solo en el loopback.

## Números medidos (macOS 27, M1 Pro)

| Medida | Valor |
|--------|-------|
| `machine run -d` del ejemplo desktop (init ya hecho) | 5 s hasta Weston |
| primera vez (init instala Weston, eudev, seatd) | 12 s |
| texto de 50 caracteres con Shift y símbolos desde el navegador | llegan todos los caracteres |

## Límites

- Una pantalla por máquina; el tamaño se fija en la creación.
- Sin audio.
- El cursor lo dibuja el guest, dentro del cuadro: sin plano de cursor separado en el navegador.
- Probado solo en macOS. En Linux, Venus atiende el 2D mediante virgl, pero no se ha medido.
