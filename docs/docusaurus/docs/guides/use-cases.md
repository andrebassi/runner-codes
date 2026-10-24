---
title: 'Casos de uso'
description: 'Claude Code no navegador numa MicroVM, playground de código pela API, LLM local na GPU e desktop no navegador'
---

Cada caso abaixo foi rodado numa VM real.

## Claude Code no navegador

Um agente sem freio, numa caixa. O Claude Code roda dentro de uma MicroVM com `--dangerously-skip-permissions`: ele
edita, instala e executa sem pedir confirmação, porque o que der errado fica na VM.

```bash
rc machine create --name agent --image node:22 --net -e IS_SANDBOX=1 \
  --init "npm install -g @anthropic-ai/claude-code"
rc machine start  --name agent
rc machine web-terminal --name agent -- claude --dangerously-skip-permissions
```

<figure class="rc-vterm">
<div class="rc-tbar"><i style="background:#ff5f57"></i><i style="background:#febc2e"></i><i style="background:#28c840"></i><span>terminal web · runner.codes</span></div>
<div class="rc-tabs" aria-hidden="true"><span class="on">claude</span><span>shell</span><span>eventos</span><span class="port">8000</span></div>
<pre role="img" aria-label="Sessão real do Claude Code no terminal web: escreveu um servidor Python na porta 8000, testou e explicou o que fez."><span style="color:#d77757"> ▐▛███▛█</span>   <span style="font-weight:600">Claude Code</span> <span style="color:#999999">v2.1.304</span>
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
<div class="rc-cap">Sessão real (20/10/2026): o Claude Code escreveu e subiu um servidor na porta 8000, e a porta virou uma aba.</div>
</figure>

| O quê | Como |
|-------|------|
| projeto | a pasta do host montada em `/workspace`: o que o agente escreve aparece nela |
| terminal | [`rc machine web-terminal`](/cli/machine#terminal-web): só em `127.0.0.1`, com token na URL |
| root sem confirmação | `IS_SANDBOX=1` no `runner.code`; sem ele o Claude recusa `--dangerously-skip-permissions` como root |
| portas | qualquer servidor que o agente subir (uvicorn, Vite, um banco) vira uma aba no fim da barra, com a app dentro, e responde no `127.0.0.1` do seu computador. A aba `shell` dá um terminal à parte e `eventos` mostra o que abriu e fechou |
| SSH | `[auth] ssh_agent = true`: `git push` com as chaves do host, que não entram na VM |
| login | uma vez, pelo `/login` da assinatura ou pela chave da API; fica em `/root/.claude` no disco da máquina e sobrevive ao stop |
| continuar | fechar a aba não para nada; para retomar depois de sair: `rc machine web-terminal --name agent -- claude --dangerously-skip-permissions --continue` |
| chave da API sem entrar na VM | `--credential anthropic=ANTHROPIC_API_KEY@api.anthropic.com:set-header=x-api-key` |

Todas as abas abertas na mesma URL dividem um PTY: redimensionar uma janela muda o tamanho para todas.

## Experimente: claude-in-chrome.runner.codes

O mesmo terminal, publicado: em [claude-in-chrome.runner.codes](https://claude-in-chrome.runner.codes/) cada
visitante ganha uma máquina própria com o Claude Code aberto sem confirmação de permissão. Você faz o `/login` com a
sua conta, e o login fica só na sua máquina.

<figure class="rc-vterm">
<div class="rc-tbar"><i style="background:#ff5f57"></i><i style="background:#febc2e"></i><i style="background:#28c840"></i><span>claude-in-chrome.runner.codes</span></div>
<div class="rc-tabs" aria-hidden="true"><span>claude</span><span>shell</span><span class="on">eventos</span><span class="port">8000</span></div>
<div class="rc-ev"><div><time>13:09:54</time>claude: sessão iniciada</div><div><time>13:12:07</time>shell: sessão iniciada</div><div><time>13:12:12</time>porta 8000 aberta na máquina</div></div>
<div class="rc-cap">Eventos reais de uma máquina do playground: o servidor da porta 8000 abre em <code>https://8000-&lt;id&gt;.runner.codes/</code>.</div>
</figure>

| O quê | Como |
|-------|------|
| máquina | uma Fly Machine por visitante (o Fly não expõe `/dev/kvm`, então a microVM é a do Fly), 2 vCPUs e 2 GiB |
| quem é você | o visitorId do navegador (FingerprintJS v3, MIT) só escolhe qual máquina é sua; o token do terminal é HMAC do servidor |
| portas | qualquer servidor que você subir responde em `https://<porta>-<id>.runner.codes/` e vira uma aba |
| rede | internet aberta; a rede privada do Fly e o proxy `/.fly/api` ficam fechados para o visitante |
| teto | 3 máquinas; com todas em uso a página avisa "playground is full" |
| tempo | 30 min sem uso desliga; 24 h sem uso apaga a máquina e o disco |

O código está em `cmd/rc-playground` (o front), `cmd/rc-visitor` (dentro de cada máquina) e
`deploy/playground/flake.nix` (as imagens, pelo Nix). A operação passo a passo está no runbook, seção 8w.

## Playground de código

Exercícios, correção automática, sandbox de documentação: cada envio roda numa MicroVM nova, sem rede, e some no fim.
15 linguagens pela mesma rota, 26–43 ms por requisição com o pool aquecido. Veja a [API HTTP](/cli/http-api).

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
curl -s -X POST http://127.0.0.1:8080/api/v1/run -H 'content-type: application/json' \
  -d '{"language":"python","stdin":"oi","files":[{"name":"main.py","content":"print(input().upper())"}]}'
```

## LLM local na GPU

llama.cpp com Vulkan dentro da VM, falando com a GPU do host por virtio-gpu (Venus → MoltenVK → Metal): 56,7 tokens/s
na GPU contra 10,3 na CPU (Qwen2.5 0.5B). Nenhum diretório do host é montado, a menos que você declare um volume.
Detalhes em [GPU](/advanced/gpu).

## Desktop no navegador

A tela do navegador (`--display`), com teclado e mouse, em `http://127.0.0.1:6080`: para testar app gráfico ou deixar um
agente usar uma tela. Detalhes em [Tela no navegador](/cli/display).

## Desktop com Claude Code, operado por um agente

O [`examples/desktop-claude/runner.code`](https://github.com/andrebassi/runner-codes/blob/main/examples/desktop-claude/runner.code)
monta um desktop XFCE na microVM: fundo preto, os atalhos Arquivos, Claude, Navegador e Terminal, o Firefox como
navegador padrão e o Claude Code instalado. A tela acompanha a janela do navegador, todo aplicativo abre maximizado, e
um logout religa a sessão sozinho.

```bash
cd examples/desktop-claude
rc machine create --name desk
CLAUDE_CODE_OAUTH_TOKEN=... rc machine start --name desk    # o guest só vê um placeholder do token
open http://127.0.0.1:6080
```

Daí, três jeitos de mandar trabalho ao Claude dessa VM, todos pelo SDK Go
([`examples/desktop-claude-agent`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude-agent)):
`claude -p` por exec (resposta em JSON), a tarefa digitada na janela dele, ou digitada pela API de tela. E, com o
[`rc mcp`](/cli/screen#para-um-agente-rc-mcp), o Claude Code da sua máquina opera a tela da VM como opera um navegador:
print, clique, teclado.
