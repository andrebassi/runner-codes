---
title: '使用场景'
description: '在浏览器中运行于 MicroVM 内的 Claude Code、通过 API 的代码 playground、GPU 上的本地 LLM，以及浏览器中的桌面'
---

每个场景在 [`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples) 里都有一份指南，会在真实的 VM 中启动并验证它所承诺的内容。

## 浏览器中的 Claude Code {#claude-code-no-navegador}

一个不设刹车的 agent，关在盒子里。Claude Code 在 MicroVM 中以 `--dangerously-skip-permissions` 运行：它会直接编辑、
安装和执行，无需确认，因为出了问题也只留在 VM 里。

```bash
rc machine create --name agent -f examples/guides/coding-agent/runner.code
rc machine start  --name agent
rc machine web-terminal --name agent -- claude --dangerously-skip-permissions
```

![在浏览器中运行于 rc MicroVM 内的 Claude Code](/img/claude-web-terminal.jpg)

*真实会话（2026-10-20）：`/init` 把 `CLAUDE.md` 写进了挂载在 `/workspace` 的 Mac 目录。*

| 项目 | 方式 |
|------|------|
| 项目目录 | host 的目录挂载到 `/workspace`：agent 写的内容会出现在那里 |
| 终端 | [`rc machine web-terminal`](/cli/machine#terminal-web)：只监听 `127.0.0.1`，URL 带 token |
| 以 root 免确认运行 | `runner.code` 中设置 `IS_SANDBOX=1`；没有它，Claude 会拒绝以 root 运行 `--dangerously-skip-permissions` |
| 端口 | agent 启动的任何服务器（uvicorn、Vite、数据库）都会变成顶栏末尾的一个标签页，内嵌应用，并在你电脑的 `127.0.0.1` 上响应。`shell` 标签页提供另一个独立终端，`eventos` 显示打开和关闭的内容 |
| SSH | `[auth] ssh_agent = true`：用 host 的密钥 `git push`，密钥不进入 VM |
| 登录 | 只需一次，用订阅的 `/login` 或 API key；保存在机器磁盘的 `/root/.claude`，stop 之后依然有效 |
| 继续 | 关闭标签页不会停止任何东西；退出后要接着做：`rc machine web-terminal --name agent -- claude --dangerously-skip-permissions --continue` |
| API key 不进入 VM | `--credential anthropic=ANTHROPIC_API_KEY@api.anthropic.com:set-header=x-api-key` |

同一个 URL 打开的所有标签页共用一个 PTY：调整其中一个窗口的大小会改变所有标签页的尺寸。

## 试一试:claude-in-chrome.runner.codes {#experimente-claude-in-chromerunnercodes}

同一个终端,公开发布:在 [claude-in-chrome.runner.codes](https://claude-in-chrome.runner.codes/) 每位访客都有一台自己的
机器,Claude Code 已打开且不需要确认权限。你用自己的账号执行 `/login`,登录信息只留在你的机器上。

![浏览器中的 playground:shell 标签页和单独标签页中的 3000 端口](/img/claude-in-chrome-playground.jpg)

| 内容 | 方式 |
|------|------|
| 机器 | 每位访客一台 Fly Machine(Fly 不提供 `/dev/kvm`,所以 microVM 是 Fly 的),2 个 vCPU 和 2 GiB |
| 你是谁 | 浏览器的 visitorId(FingerprintJS v3,MIT)只用来选出哪台机器是你的;终端令牌是服务器的 HMAC |
| 端口 | 你启动的任何服务器都会在 `https://<端口>-<id>.runner.codes/` 响应,并成为一个标签页 |
| 网络 | 互联网开放;Fly 私有网络和 `/.fly/api` 代理对访客关闭 |
| 上限 | 3 台机器;全部在用时页面提示 "playground is full" |
| 时间 | 空闲 30 分钟关机;24 小时未使用则删除机器和磁盘 |

代码在 `cmd/rc-playground`(front)、`cmd/rc-visitor`(每台机器内部)和 `deploy/playground/flake.nix`(用 Nix
构建的镜像)。逐步操作见 runbook 第 8w 节。

## 代码 playground {#playground-de-código}

练习题、自动评分、文档 sandbox：每次提交都在一台全新的、无网络的 MicroVM 中运行，结束即消失。同一个路由支持 15 种语言，
pool 预热后每个请求 26–43 ms。参见 [HTTP API](/cli/http-api)。

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
curl -s -X POST http://127.0.0.1:8080/api/v1/run -H 'content-type: application/json' \
  -d '{"language":"python","stdin":"oi","files":[{"name":"main.py","content":"print(input().upper())"}]}'
```

## GPU 上的本地 LLM {#llm-local-na-gpu}

VM 内的 llama.cpp 使用 Vulkan，通过 virtio-gpu（Venus → MoltenVK → Metal）调用 host 的 GPU：GPU 上 56.7 tokens/s，
CPU 上 10.3（Qwen2.5 0.5B）。除非你声明了卷，否则不会挂载任何 host 目录。指南：[`local-llm`](https://github.com/andrebassi/runner-codes/tree/main/examples/local-llm)。

## 浏览器中的桌面 {#desktop-no-navegador}

浏览器里的 Weston 桌面，支持键盘和鼠标，地址 `http://127.0.0.1:6080`：适合测试图形应用，或让 agent 使用一块屏幕。
指南：[`desktop`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop)；详见[浏览器中的显示](/cli/display)。
