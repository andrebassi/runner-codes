---
title: 'Control de pantalla'
description: 'Operar la pantalla de una máquina como una persona: captura, clic, teclado y desplazamiento por la CLI, la API, los SDKs y un servidor MCP para agentes'
---

# Control de pantalla

Una máquina con [pantalla](/cli/display) se puede operar como lo haría una persona: tomar una captura, hacer clic,
escribir, pulsar atajos y desplazarse. Funciona por la CLI, por la API HTTP, por los SDKs de Go y Python y por un
servidor MCP. Con el MCP, un agente como Claude Code opera la VM del mismo modo que opera un navegador.

Todo pasa por el visor de la pantalla, el mismo de la página en el navegador: la imagen sale de él, y el mouse y el
teclado llegan a los dispositivos virtio-input de la VM. **No se instala nada en el guest.**

## La flecha del agente

Quien mira en el navegador ve dónde actúa la automatización: una flecha clara con borde y brillo naranja se desliza hasta el
punto de cada acción (1,1 s) y solo entonces hace clic, como lo haría una persona; un
círculo pulsa en el clic y una etiqueta "digitando…" aparece con las teclas. Se queda en pantalla 30 s después de la última
acción. Solo las acciones del control de pantalla (CLI, API, SDKs y MCP) dibujan la flecha; el ratón de quien está en la
página sigue siendo solo su cursor.

El nombre en la barra y la pestaña de la página es `runner.codes`; `display_title` en el `runner.code` (o `--display-title`) lo cambia.

## Por la CLI

```bash
rc machine screen --name desk screenshot -o tela.jpg     # screen 1512x794 -> tela.jpg
rc machine screen --name desk click 640 400              # --button right, --double
rc machine screen --name desk type "ls -la"
rc machine screen --name desk key Return                 # ctrl+c, ctrl+alt+t, alt+F4, Up...
rc machine screen --name desk scroll 640 400 --up --amount 5
rc machine screen --name desk drag 100 100 400 300
rc machine screen --name desk move 640 400
echo '[{"action":"click","x":640,"y":400},{"action":"type","text":"ls\n"}]' | rc machine screen --name desk do
```

Las coordenadas son píxeles de la pantalla actual, con el origen en la esquina superior izquierda. La pantalla puede
[acompañar la ventana del navegador](/cli/display#pantalla-que-acompaña-la-ventana) y cambiar de tamaño; por eso, cada
comando lee el tamaño en el momento.

## Acciones

| Acción | Campos | Hace |
|------|--------|-----|
| `click` | `x`, `y`, `button` (`left`, `right`, `middle`), `double` | se mueve hasta el punto y hace clic |
| `move` | `x`, `y` | solo mueve el puntero (hover) |
| `drag` | `x`, `y`, `to_x`, `to_y` | presiona, arrastra por puntos intermedios y suelta |
| `type` | `text` | escribe, con Shift cuando hace falta; `\n` es Enter |
| `key` | `keys` | una tecla o atajo: `Return`, `Escape`, `Tab`, `ctrl+c`, `ctrl+shift+t`, `alt+F4`, `F5`, `Page_Down` |
| `scroll` | `x`, `y`, `direction` (`up`, `down`), `amount` (por defecto 3) | gira la rueda en el punto |
| `wait` | `ms` (hasta 60000) | espera entre acciones |

Una lista con cualquier acción inválida se rechaza entera, antes de que salga ningún evento: un `type` a medias o un
atajo sin la última tecla dejaría la máquina en un estado extraño.

**`type` escribe como un teclado US** y cubre solo ASCII. Los acentos y los emoji se rechazan con un mensaje. Para otro
texto, use `exec` con `xdotool type` dentro de la máquina (el [escritorio de ejemplo](#ejemplo-escritorio-con-claude-code)
ya lo trae).

## Por la API

```bash
curl -s -o tela.jpg -D - localhost:8080/api/v1/machines/desk/screen | grep -i x-screen
# X-Screen-Width: 1512
# X-Screen-Height: 794
curl -s -X POST localhost:8080/api/v1/machines/desk/screen/actions \
  -d '{"actions":[{"action":"click","x":640,"y":400},{"action":"type","text":"ls"},{"action":"key","keys":"Return"}]}'
# 204
```

| Ruta | Respuesta |
|------|----------|
| `GET /api/v1/machines/:name/screen` | `200` con la pantalla en JPEG y el tamaño en `X-Screen-Width` y `X-Screen-Height` |
| `POST /api/v1/machines/:name/screen/actions` | `204` cuando todas las acciones salieron; hasta 500 acciones por llamada |

Errores: `400 CONFIG_ERROR` para una acción inválida o una máquina sin pantalla; `409 INVALID_STATE` para una máquina
apagada o pausada; `502 SCREEN_UNAVAILABLE` cuando el visor de la máquina no responde.

## Por los SDKs

```go title="Go"
shot, _ := c.Screenshot(ctx, "desk")                  // shot.JPEG, shot.Width, shot.Height
c.Click(ctx, "desk", 640, 400)
c.Type(ctx, "desk", "ls -la")
c.Key(ctx, "desk", "Return")
c.ScreenActions(ctx, "desk",
	client.ScreenAction{Action: "scroll", X: 640, Y: 400, Direction: "up"},
	client.ScreenAction{Action: "wait", Ms: 300},
	client.ScreenAction{Action: "key", Keys: "ctrl+c"})
```

```python title="Python"
shot = c.screenshot("desk")                           # shot.jpeg, shot.width, shot.height
c.click("desk", 640, 400)
c.type("desk", "ls -la")
c.key("desk", "Return")
c.screen_actions("desk", {"action": "scroll", "x": 640, "y": 400, "direction": "up"},
                 {"action": "wait", "ms": 300}, {"action": "key", "keys": "ctrl+c"})
```

## Para un agente: `rc mcp`

`rc mcp` es un servidor [MCP](https://modelcontextprotocol.io) por stdio, vinculado a una máquina. Registrado en Claude
Code, le da al agente las herramientas para operar la VM:

```bash
claude mcp add rc -- rc mcp --name desk
```

| Herramienta | Hace |
|------------|-----|
| `screenshot` | devuelve la pantalla como imagen y el tamaño en píxeles |
| `click`, `move`, `drag`, `type`, `key`, `scroll` | las acciones de la tabla de arriba, una por llamada |
| `actions` | varias acciones en orden, en una llamada |
| `exec` | ejecuta un comando con `sh -c` y devuelve el código de salida, el stdout y el stderr |

Un fallo de una herramienta (máquina apagada, acción rechazada, comando con salida distinta de 0) vuelve como resultado
con `isError`, para que el agente lea el mensaje y pruebe otro camino.

Medido con Claude Code: le pedimos "toma una captura, haz clic en la pestaña `bash · claude` de la barra de tareas y
confirma que la ventana cambió". Tomó la captura, encontró la pestaña, hizo clic, vio la pantalla todavía sin cambiar,
esperó y confirmó el cambio en la segunda captura.

## Ejemplo: escritorio con Claude Code

[`examples/desktop-claude`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude) es un
escritorio XFCE en un `runner.code`: fondo negro, los accesos directos Arquivos, Claude, Navegador y Terminal, Firefox
como navegador predeterminado y Claude Code instalado. El acceso directo Claude abre
`claude --dangerously-skip-permissions`; la VM es el sandbox. El login de Claude entra por
[credencial](/cli/credentials): el guest solo ve un placeholder.

[`examples/desktop-claude-agent`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude-agent)
envía tareas a ese Claude de tres formas, todas por el SDK de Go:

| Modo | Cómo llega la tarea | ¿Se ve en pantalla? |
|------|---------------------|-------------|
| `headless` | `claude -p` por exec; la respuesta vuelve en JSON | no |
| `gui` | escrita en la ventana de Claude con `xdotool` (cualquier texto) | sí |
| `keys` | escrita por la API de pantalla (`ScreenActions`), sin nada instalado en la VM | sí |

Cada modo le pide a Claude un archivo con un contenido conocido y verifica el archivo por la API: "funcionó" es el
archivo correcto en el disco de la VM, no solo una respuesta.
