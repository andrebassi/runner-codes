---
title: 'Use cases'
description: 'Claude Code in the browser inside a MicroVM, a code playground over the API, a local LLM on the GPU and a desktop in the browser'
---

Each case below was run in a real VM.

## Claude Code in the browser

An agent with no brakes, in a box. Claude Code runs inside a MicroVM with `--dangerously-skip-permissions`: it edits,
installs and executes without asking, because whatever goes wrong stays in the VM.

```bash
rc machine create --name agent --image node:22 --net -e IS_SANDBOX=1 \
  --init "npm install -g @anthropic-ai/claude-code"
rc machine start  --name agent
rc machine web-terminal --name agent -- claude --dangerously-skip-permissions
```

<figure class="rc-vterm">
<div class="rc-tbar"><i style="background:#ff5f57"></i><i style="background:#febc2e"></i><i style="background:#28c840"></i><span>web terminal · runner.codes</span></div>
<div class="rc-tabs" aria-hidden="true"><span class="on">claude</span><span>shell</span><span>eventos</span><span class="port">8000</span></div>
<pre role="img" aria-label="Real Claude Code session in the web terminal: it wrote a Python server on port 8000, tested it and explained what it did."><span style="color:#d77757"> ▐▛███▛█</span>   <span style="font-weight:600">Claude Code</span> <span style="color:#999999">v2.1.304</span>
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
<div class="rc-cap">Real session (2026-10-20): Claude Code wrote and started a server on port 8000, and the port became a tab.</div>
</figure>

| What | How |
|------|-----|
| project | the host folder mounted at `/workspace`: what the agent writes shows up there |
| terminal | [`rc machine web-terminal`](/cli/machine#web-terminal): `127.0.0.1` only, with a token in the URL |
| root without confirmation | `IS_SANDBOX=1` in `runner.code`; without it Claude refuses `--dangerously-skip-permissions` as root |
| ports | any server the agent starts (uvicorn, Vite, a database) becomes a tab at the end of the bar, with the app inside, and answers on your computer's `127.0.0.1`. The `shell` tab gives a separate terminal and `eventos` shows what opened and closed |
| SSH | `[auth] ssh_agent = true`: `git push` with the host's keys, which never enter the VM |
| login | once, via the subscription `/login` or an API key; it lives in `/root/.claude` on the machine's disk and survives a stop |
| resume | closing the tab stops nothing; to pick up after exiting: `rc machine web-terminal --name agent -- claude --dangerously-skip-permissions --continue` |
| API key that never enters the VM | `--credential anthropic=ANTHROPIC_API_KEY@api.anthropic.com:set-header=x-api-key` |

Every tab open on the same URL shares one PTY: resizing one window changes the size for all of them.

## Try it: claude-in-chrome.runner.codes

The same terminal, published: at [claude-in-chrome.runner.codes](https://claude-in-chrome.runner.codes/) each
visitor gets a machine of their own with Claude Code open and no permission prompts. You run `/login` with your own
account, and the login stays on your machine only.

<figure class="rc-vterm">
<div class="rc-tbar"><i style="background:#ff5f57"></i><i style="background:#febc2e"></i><i style="background:#28c840"></i><span>claude-in-chrome.runner.codes</span></div>
<div class="rc-tabs" aria-hidden="true"><span>claude</span><span>shell</span><span class="on">eventos</span><span class="port">8000</span></div>
<div class="rc-ev"><div><time>13:09:54</time>claude: session started</div><div><time>13:12:07</time>shell: session started</div><div><time>13:12:12</time>port 8000 opened on the machine</div></div>
<div class="rc-cap">Real events from a playground machine: the port 8000 server opens at <code>https://8000-&lt;id&gt;.runner.codes/</code>.</div>
</figure>

| What | How |
|------|-----|
| machine | one Fly Machine per visitor (Fly exposes no `/dev/kvm`, so the microVM is Fly's), 2 vCPUs and 2 GiB |
| who you are | the browser visitorId (FingerprintJS v3, MIT) only picks which machine is yours; the terminal token is a server HMAC |
| ports | any server you start answers at `https://<port>-<id>.runner.codes/` and becomes a tab |
| network | open internet; the Fly private network and the `/.fly/api` proxy are closed to the visitor |
| limit | 3 machines; with all of them in use the page says "playground is full" |
| time | 30 min idle stops it; 24 h without use deletes the machine and its disk |

The code lives in `cmd/rc-playground` (the front), `cmd/rc-visitor` (inside each machine) and
`deploy/playground/flake.nix` (the images, built with Nix). The step-by-step operation is in the runbook, section 8w.

## Code playground

Exercises, auto-grading, documentation sandboxes: each submission runs in a fresh MicroVM, with no network, and is gone
at the end. 15 languages through the same route, 26–43 ms per request with the pool warmed. See the [HTTP API](/cli/http-api).

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
curl -s -X POST http://127.0.0.1:8080/api/v1/run -H 'content-type: application/json' \
  -d '{"language":"python","stdin":"oi","files":[{"name":"main.py","content":"print(input().upper())"}]}'
```

## Local LLM on the GPU

llama.cpp with Vulkan inside the VM, talking to the host GPU through virtio-gpu (Venus → MoltenVK → Metal): 56.7
tokens/s on the GPU versus 10.3 on the CPU (Qwen2.5 0.5B). No host directory is mounted unless you declare a volume.
Details in [GPU](/advanced/gpu).

## Desktop in the browser

The browser screen (`--display`), with keyboard and mouse, at `http://127.0.0.1:6080`: for testing a GUI app or letting an
agent use a screen. Details in [Display in the browser](/cli/display).

## Desktop with Claude Code, operated by an agent

[`examples/desktop-claude/runner.code`](https://github.com/andrebassi/runner-codes/blob/main/examples/desktop-claude/runner.code)
sets up an XFCE desktop in the microVM: black background, the Arquivos, Claude, Navegador and Terminal shortcuts, Firefox
as the default browser and Claude Code installed. The screen follows the browser window, every application opens
maximized, and a logout brings the session back on its own.

```bash
cd examples/desktop-claude
rc machine create --name desk
CLAUDE_CODE_OAUTH_TOKEN=... rc machine start --name desk    # the guest only sees a placeholder of the token
open http://127.0.0.1:6080
```

From there, three ways to send work to that VM's Claude, all through the Go SDK
([`examples/desktop-claude-agent`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude-agent)):
`claude -p` via exec (answer in JSON), the task typed into its window, or typed through the screen API. And, with
[`rc mcp`](/cli/screen#for-an-agent-rc-mcp), the Claude Code on your machine operates the VM's screen the way it operates
a browser: screenshot, click, keyboard.
