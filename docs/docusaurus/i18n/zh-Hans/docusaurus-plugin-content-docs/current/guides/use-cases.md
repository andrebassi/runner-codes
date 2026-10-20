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

<figure class="rc-vterm">
<div class="rc-tbar"><i style="background:#ff5f57"></i><i style="background:#febc2e"></i><i style="background:#28c840"></i><span>Web 终端 · runner.codes</span></div>
<div class="rc-tabs" aria-hidden="true"><span class="on">claude</span><span>shell</span><span>eventos</span><span class="port">8000</span></div>
<pre role="img" aria-label="Web 终端中的真实 Claude Code 会话:它编写了 8000 端口的 Python 服务器,测试并说明了所做的事。"><span style="color:#d77757"> ▐▛███▛█</span>   <span style="font-weight:600">Claude Code</span> <span style="color:#999999">v2.1.304</span>
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
<div class="rc-cap">真实会话(2026-10-20):Claude Code 编写并启动了 8000 端口的服务器,该端口成为一个标签页。</div>
</figure>

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

<figure class="rc-vterm">
<div class="rc-tbar"><i style="background:#ff5f57"></i><i style="background:#febc2e"></i><i style="background:#28c840"></i><span>claude-in-chrome.runner.codes</span></div>
<div class="rc-tabs" aria-hidden="true"><span>claude</span><span>shell</span><span class="on">eventos</span><span class="port">8000</span></div>
<div class="rc-ev"><div><time>13:09:54</time>claude: 会话已开始</div><div><time>13:12:07</time>shell: 会话已开始</div><div><time>13:12:12</time>机器上的 8000 端口已打开</div></div>
<div class="rc-cap">playground 机器的真实事件:8000 端口的服务器在 <code>https://8000-&lt;id&gt;.runner.codes/</code> 打开。</div>
</figure>

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
