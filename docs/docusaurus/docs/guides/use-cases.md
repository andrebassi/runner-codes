---
title: 'Casos de uso'
description: 'Claude Code no navegador numa MicroVM, playground de código pela API, LLM local na GPU e desktop no navegador'
---

Cada caso tem um guia em [`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples) que sobe numa VM real e confere o que promete.

## Claude Code no navegador

Um agente sem freio, numa caixa. O Claude Code roda dentro de uma MicroVM com `--dangerously-skip-permissions`: ele
edita, instala e executa sem pedir confirmação, porque o que der errado fica na VM.

```bash
rc machine create --name agent -f examples/guides/coding-agent/runner.code
rc machine start  --name agent
rc machine web-terminal --name agent -- claude --dangerously-skip-permissions
```

![Claude Code rodando numa MicroVM do rc, no navegador](/img/claude-web-terminal.jpg)

*Sessão real (20/10/2026): o `/init` escreveu o `CLAUDE.md` na pasta do Mac montada em `/workspace`.*

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

![O playground no navegador: o Claude Code aberto e a porta 3000 numa aba própria](/img/claude-in-chrome-playground.jpg)

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
Guia: [`local-llm`](https://github.com/andrebassi/runner-codes/tree/main/examples/local-llm).

## Desktop no navegador

Weston na tela do navegador, com teclado e mouse, em `http://127.0.0.1:6080`: para testar app gráfico ou deixar um
agente usar uma tela. Guia: [`desktop`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop); detalhes em [Tela no navegador](/cli/display).
