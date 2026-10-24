---
title: 'Casos de uso'
description: 'Claude Code en el navegador dentro de una MicroVM, playground de código por la API, LLM local en la GPU y escritorio en el navegador'
---

Cada caso de abajo se ejecutó en una VM real.

## Claude Code en el navegador

Un agente sin frenos, dentro de una caja. Claude Code corre dentro de una MicroVM con `--dangerously-skip-permissions`:
edita, instala y ejecuta sin pedir confirmación, porque lo que salga mal se queda en la VM.

```bash
rc machine create --name agent --image node:22 --net -e IS_SANDBOX=1 \
  --init "npm install -g @anthropic-ai/claude-code"
rc machine start  --name agent
rc machine web-terminal --name agent -- claude --dangerously-skip-permissions
```

<figure class="rc-vterm">
<div class="rc-tbar"><i style="background:#ff5f57"></i><i style="background:#febc2e"></i><i style="background:#28c840"></i><span>terminal web · runner.codes</span></div>
<div class="rc-tabs" aria-hidden="true"><span class="on">claude</span><span>shell</span><span>eventos</span><span class="port">8000</span></div>
<pre role="img" aria-label="Sesión real de Claude Code en el terminal web: escribió un servidor Python en el puerto 8000, lo probó y explicó lo que hizo."><span style="color:#d77757"> ▐▛███▛█</span>   <span style="font-weight:600">Claude Code</span> <span style="color:#999999">v2.1.304</span>
<span style="color:#d77757">▝▜██████▀</span>  <span style="color:#999999">Opus 5.5 · Claude Max</span>
<span style="color:#d77757"> ▝▝   ▝▝ </span>  <span style="color:#999999">/tmp/demo</span>


<span style="color:#999999">❯ </span><span style="color:#ffffff">Write a tiny Python web server that answers &quot;hello from the playground&quot; on port 8000, start it in</span>
  <span style="color:#ffffff">the background and check it with curl.</span>

  <span style="color:#999999">Ran </span><span style="color:#999999;font-weight:600">2</span><span style="color:#999999"> shell commands</span>

<span style="color:#ffffff">●</span> curl isn&#x27;t installed here, so I&#x27;ll check the server with Python&#x27;s HTTP client instead.

  <span style="color:#999999">Ran </span><span style="color:#999999;font-weight:600">1</span><span style="color:#999999"> shell command</span>

<span style="color:#ffffff">●</span> The server is running in the background on port 8000 and answers <span style="color:#b1b9f9">hello from the playground</span>. I
  couldn&#x27;t check it with curl because curl isn&#x27;t installed in this environment. Instead I fetched
  the page with Python&#x27;s built-in HTTP client and got <span style="color:#b1b9f9">200 text/plain</span> with that text.

  - <span style="font-weight:600">Code:</span> <span style="color:#b1b9f9">/tmp/demo/server.py</span> uses only Python&#x27;s standard library and answers any GET request.
  - <span style="font-weight:600">Log:</span> <span style="color:#b1b9f9">/tmp/demo/server.log</span> records each request.

  To check it with curl yourself, install it first (for example <span style="color:#b1b9f9">! apt-get install -y curl</span>), then run
  <span style="color:#b1b9f9">curl localhost:8000</span>. To stop the server, run <span style="color:#b1b9f9">pkill -f server.py</span>, or <span style="color:#b1b9f9">kill</span> its process ID if <span style="color:#b1b9f9">pkill</span>
  isn&#x27;t available either (<span style="color:#b1b9f9">pgrep</span> wasn&#x27;t).

<span style="color:#999999">✻</span> <span style="color:#999999">Churned for 15s · done 4:18 PM</span>

<span style="color:#888888">────────────────────────────────────────────────────────────────────────────────────────────────────</span>
❯ install curl and check it
<span style="color:#888888">────────────────────────────────────────────────────────────────────────────────────────────────────</span>
  <span style="color:#ff6b80">⏵⏵ bypass permissions on</span><span style="color:#999999"> (shift+tab to cycle)</span>
</pre>
<div class="rc-cap">Sesión real (20/10/2026): Claude Code escribió y levantó un servidor en el puerto 8000, y el puerto se volvió una pestaña.</div>
</figure>

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

<figure class="rc-vterm">
<div class="rc-tbar"><i style="background:#ff5f57"></i><i style="background:#febc2e"></i><i style="background:#28c840"></i><span>claude-in-chrome.runner.codes</span></div>
<div class="rc-tabs" aria-hidden="true"><span>claude</span><span>shell</span><span class="on">eventos</span><span class="port">8000</span></div>
<div class="rc-ev"><div><time>13:09:54</time>claude: sesión iniciada</div><div><time>13:12:07</time>shell: sesión iniciada</div><div><time>13:12:12</time>puerto 8000 abierto en la máquina</div></div>
<div class="rc-cap">Eventos reales de una máquina del playground: el servidor del puerto 8000 abre en <code>https://8000-&lt;id&gt;.runner.codes/</code>.</div>
</figure>

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
volumen. Detalles en [GPU](/advanced/gpu).

## Escritorio en el navegador

La pantalla del navegador (`--display`), con teclado y ratón, en `http://127.0.0.1:6080`: para probar una app gráfica o
dejar que un agente use una pantalla. Detalles en [Pantalla en el navegador](/cli/display).

## Escritorio con Claude Code, operado por un agente

El [`examples/desktop-claude/runner.code`](https://github.com/andrebassi/runner-codes/blob/main/examples/desktop-claude/runner.code)
monta un escritorio XFCE en la microVM: fondo negro, los accesos directos Arquivos, Claude, Navegador y Terminal, Firefox
como navegador predeterminado y Claude Code instalado. La pantalla acompaña la ventana del navegador, toda aplicación
abre maximizada, y un logout vuelve a levantar la sesión sola.

```bash
cd examples/desktop-claude
rc machine create --name desk
CLAUDE_CODE_OAUTH_TOKEN=... rc machine start --name desk    # el guest solo ve un placeholder del token
open http://127.0.0.1:6080
```

A partir de ahí, tres formas de enviar trabajo al Claude de esa VM, todas por el SDK de Go
([`examples/desktop-claude-agent`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude-agent)):
`claude -p` por exec (respuesta en JSON), la tarea escrita en su ventana, o escrita por la API de pantalla. Y, con el
[`rc mcp`](/cli/screen#para-um-agente-rc-mcp), el Claude Code de tu máquina opera la pantalla de la VM como opera un
navegador: captura, clic, teclado.
