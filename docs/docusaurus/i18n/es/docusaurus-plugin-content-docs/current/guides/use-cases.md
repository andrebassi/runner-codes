---
title: 'Casos de uso'
description: 'Claude Code en el navegador dentro de una MicroVM, playground de código por la API, LLM local en la GPU y escritorio en el navegador'
---

Cada caso tiene una guía en [`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples) que arranca en una VM real y comprueba lo que promete.

## Claude Code en el navegador

Un agente sin frenos, dentro de una caja. Claude Code corre dentro de una MicroVM con `--dangerously-skip-permissions`:
edita, instala y ejecuta sin pedir confirmación, porque lo que salga mal se queda en la VM.

```bash
rc machine create --name agent -f examples/guides/coding-agent/runner.code
rc machine start  --name agent
rc machine web-terminal --name agent -- claude --dangerously-skip-permissions
```

![Claude Code corriendo en una MicroVM de rc, en el navegador](/img/claude-web-terminal.jpg)

*Sesión real (20/10/2026): `/init` escribió `CLAUDE.md` en la carpeta del Mac montada en `/workspace`.*

| Qué | Cómo |
|-----|------|
| proyecto | la carpeta del host montada en `/workspace`: lo que el agente escribe aparece ahí |
| terminal | [`rc machine web-terminal`](/cli/machine#terminal-web): solo en `127.0.0.1`, con token en la URL |
| root sin confirmación | `IS_SANDBOX=1` en el `runner.code`; sin él Claude rechaza `--dangerously-skip-permissions` como root |
| puertos | cualquier servidor que el agente levante (uvicorn, Vite, una base de datos) se vuelve una pestaña al final de la barra, con la app adentro, y responde en el `127.0.0.1` de tu computadora. La pestaña `shell` da un terminal aparte y `eventos` muestra lo que se abrió y cerró |
| SSH | `[auth] ssh_agent = true`: `git push` con las claves del host, que no entran en la VM |
| login | una vez, con el `/login` de la suscripción o la clave de la API; queda en `/root/.claude` en el disco de la máquina y sobrevive al stop |
| continuar | cerrar la pestaña no detiene nada; para retomar después de salir: `rc machine web-terminal --name agent -- claude --dangerously-skip-permissions --continue` |
| clave de la API sin entrar en la VM | `--credential anthropic=ANTHROPIC_API_KEY@api.anthropic.com:set-header=x-api-key` |

Todas las pestañas abiertas en la misma URL comparten un PTY: redimensionar una ventana cambia el tamaño para todas.

## Pruébalo: claude-in-chrome.runner.codes

El mismo terminal, publicado: en [claude-in-chrome.runner.codes](https://claude-in-chrome.runner.codes/) cada
visitante recibe una máquina propia con Claude Code abierto y sin confirmaciones de permiso. Haces `/login` con tu
cuenta, y el login queda solo en tu máquina.

![El playground en el navegador: la pestaña shell y el puerto 3000 en su propia pestaña](/img/claude-in-chrome-playground.jpg)

| Qué | Cómo |
|-----|------|
| máquina | una Fly Machine por visitante (Fly no expone `/dev/kvm`, así que la microVM es la de Fly), 2 vCPUs y 2 GiB |
| quién eres | el visitorId del navegador (FingerprintJS v3, MIT) solo elige qué máquina es tuya; el token del terminal es un HMAC del servidor |
| puertos | cualquier servidor que levantes responde en `https://<puerto>-<id>.runner.codes/` y se vuelve una pestaña |
| red | internet abierta; la red privada de Fly y el proxy `/.fly/api` quedan cerrados para el visitante |
| límite | 3 máquinas; con todas en uso la página avisa "playground is full" |
| tiempo | 30 min sin uso la apaga; 24 h sin uso borra la máquina y su disco |

El código está en `cmd/rc-playground` (el front), `cmd/rc-visitor` (dentro de cada máquina) y
`deploy/playground/flake.nix` (las imágenes, con Nix). La operación paso a paso está en el runbook, sección 8w.

## Playground de código

Ejercicios, corrección automática, sandbox de documentación: cada envío corre en una MicroVM nueva, sin red, y
desaparece al final. 15 lenguajes por la misma ruta, 26–43 ms por solicitud con el pool calentado. Ver la [API HTTP](/cli/http-api).

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
curl -s -X POST http://127.0.0.1:8080/api/v1/run -H 'content-type: application/json' \
  -d '{"language":"python","stdin":"oi","files":[{"name":"main.py","content":"print(input().upper())"}]}'
```

## LLM local en la GPU

llama.cpp con Vulkan dentro de la VM, hablando con la GPU del host por virtio-gpu (Venus → MoltenVK → Metal): 56,7
tokens/s en la GPU frente a 10,3 en la CPU (Qwen2.5 0.5B). No se monta ningún directorio del host salvo que declares un
volumen. Guía: [`local-llm`](https://github.com/andrebassi/runner-codes/tree/main/examples/local-llm).

## Escritorio en el navegador

Weston en la pantalla del navegador, con teclado y ratón, en `http://127.0.0.1:6080`: para probar una app gráfica o
dejar que un agente use una pantalla. Guía: [`desktop`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop); detalles en [Pantalla en el navegador](/cli/display).
