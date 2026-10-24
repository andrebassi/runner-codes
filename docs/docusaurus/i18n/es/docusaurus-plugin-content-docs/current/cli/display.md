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
display_title = "mi app"   # nombre en la barra y la pestaña de la página (por defecto runner.codes; en la CLI, --display-title)
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

## Pantalla que acompaña la ventana

La página del visor le pide al host el tamaño del área libre, al abrirse y 400 ms después de que la ventana deja de
cambiar (`POST /resize`). El proceso de la VM agrupa los pedidos (solo vale el último, nunca dos al mismo tiempo) y
ejecuta dentro de la máquina:

```bash
/etc/rc/display-resize LARGURA ALTURA
```

`rc` no sabe si el guest usa Xorg, Wayland o nada: quien cambia la resolución es la imagen, mediante el hook. Sin el
archivo, no pasa nada y la pantalla se queda en el tamaño del boot. En un escritorio Xorg, el hook es un `gtf` +
`xrandr` (el del [ejemplo](/cli/screen#ejemplo-escritorio-con-claude-code)):

```bash
#!/bin/sh
w=$(( $1 / 8 * 8 )); h=$2; name="${w}x${h}"
echo "$1 $2" > /run/rc-display-size          # la sesión lo vuelve a aplicar después de un logout o un crash
export DISPLAY=:0
until xrandr >/dev/null 2>&1; do sleep 0.5; done   # la página puede pedirlo antes de que Xorg arranque
out=$(xrandr | awk '/ connected/ {print $1; exit}')
xrandr | grep -q "^   $name " || { xrandr --newmode "$name" $(gtf "$w" "$h" 60 | sed -n 's/^ *Modeline "[^"]*" *//p'); xrandr --addmode "$out" "$name"; }
xrandr --output "$out" --mode "$name"
```

Durante un cambio, algunos cuadros tomados a mitad de camino se descartan (aparecen en el `vmm.log` como
`read_2d_resource`); el siguiente cuadro ya sale en el tamaño nuevo. La libkrun de `rc` trae dos correcciones para esto:
sin ellas, el primer cuadro descartado tumbaba el thread de la GPU, o dejaba la pantalla congelada para siempre con el
guest sano (medido con 30 cambios seguidos).

## Requisitos en el guest

| Para | Necesita |
|------|---------|
| la pantalla | `gpu = true`; el kernel de libkrunfw ya trae `virtio_gpu` (DRM/KMS, sin fbdev: no hay `/dev/fb0`) |
| teclado y mouse en un compositor | `udev` en ejecución (libinput enumera a través de él); en Alpine, `eudev` + `udevadm trigger` |
| compositor sin logind | `seatd` |
| Xorg | driver `modesetting` con `Option "AccelMethod" "none"` (sin GL en la pantalla 2D: glamor tumba Xorg) y `Option "SWcursor" "true"` (el cursor por hardware congelaba Xorg en el primer movimiento del mouse) |
| udev en una imagen Debian/systemd | `SYSTEMD_IGNORE_CHROOT=1` antes del `udevadm trigger`: la imagen corre en un chroot dentro de la VM, y `udevadm` ignora el pedido sin esa variable (teclado y mouse no aparecen en Xorg) |

## Seguridad

- El visor escucha **solo en el `127.0.0.1`** del host. Quien alcance el puerto ve la pantalla y controla el teclado y
  el mouse de la VM, así que no exponga el puerto.
- No hay contraseña: la protección es escuchar solo en el loopback.

## Números medidos (macOS 27, M1 Pro)

| Medida | Valor |
|--------|-------|
| escritorio XFCE de `examples/desktop-claude`, primera vez (init instala Xorg, XFCE, Firefox y Claude Code, con caché) | 28–35 s de start |
| el mismo escritorio a partir de un `rc pack` | 5,6 s de start; terminal en pantalla ~20 s después |
| 30 cambios de tamaño seguidos por `POST /resize` | pantalla en pie, 0 panics, imagen en el tamaño final |
| texto de 50 caracteres con Shift y símbolos desde el navegador | llegan todos los caracteres |

## Límites

- Una pantalla por máquina. El tamaño del boot es el de la creación; después, quien cambia la resolución es el hook del
  guest.
- Sin audio.
- El cursor lo dibuja el guest, dentro del cuadro: sin plano de cursor separado en el navegador.
- Probado solo en macOS. En Linux, Venus atiende el 2D mediante virgl, pero no se ha medido.

Para operar la pantalla por programa (captura, clic, teclado), vea [Control de pantalla](/cli/screen).
