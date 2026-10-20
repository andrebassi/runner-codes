---
title: 'Use cases'
description: 'Claude Code in the browser inside a MicroVM, a code playground over the API, a local LLM on the GPU and a desktop in the browser'
---

Each case has a guide in [`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples) that boots in a real VM and checks what it promises.

## Claude Code in the browser

An agent with no brakes, in a box. Claude Code runs inside a MicroVM with `--dangerously-skip-permissions`: it edits,
installs and executes without asking, because whatever goes wrong stays in the VM.

```bash
rc machine create --name agent -f examples/guides/coding-agent/runner.code
rc machine start  --name agent
rc machine web-terminal --name agent -- claude --dangerously-skip-permissions
```

![Claude Code running in an rc MicroVM, in the browser](/img/claude-web-terminal.jpg)

*Real session (10/20/2026): `/init` wrote `CLAUDE.md` into the Mac folder mounted at `/workspace`.*

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
Guide: [`local-llm`](https://github.com/andrebassi/runner-codes/tree/main/examples/local-llm).

## Desktop in the browser

Weston on the browser screen, with keyboard and mouse, at `http://127.0.0.1:6080`: for testing a GUI app or letting an
agent use a screen. Guide: [`desktop`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop); details in [Display in the browser](/cli/display).
