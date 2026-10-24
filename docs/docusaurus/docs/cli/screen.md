---
title: 'Controle de tela'
description: 'Operar a tela de uma máquina como uma pessoa: captura, clique, teclado e rolagem pela CLI, pela API, pelos SDKs e por um servidor MCP para agentes'
---

# Controle de tela

Uma máquina com [tela](/cli/display) pode ser operada como uma pessoa faria: tirar um print, clicar, digitar, apertar
atalhos e rolar. Funciona pela CLI, pela API HTTP, pelos SDKs Go e Python e por um servidor MCP. Com o MCP, um agente
como o Claude Code opera a VM do mesmo jeito que opera um navegador.

Tudo passa pelo visualizador da tela, o mesmo da página no navegador: a imagem sai dele, e mouse e teclado chegam aos
dispositivos virtio-input da VM. **Nada é instalado no guest.**

## A seta do agente

Quem assiste pelo navegador vê onde a automação age: uma seta clara com contorno e brilho laranja desliza até o ponto de
cada ação (1,1 s) e só então clica, como uma pessoa faria; um círculo pulsa
no clique e um selo "digitando…" aparece nas teclas. Fica na tela por 30 s depois da última ação. Só as ações do controle de
tela (CLI, API, SDKs e MCP) desenham a seta; o mouse de quem está na página continua sendo só o cursor dela.

O nome na barra e na aba da página é `runner.codes`; `display_title` no `runner.code` (ou `--display-title`) troca.

## Pela CLI

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

As coordenadas são pixels da tela atual, com a origem no canto superior esquerdo. A tela pode
[acompanhar a janela do navegador](/cli/display#tela-que-acompanha-a-janela) e mudar de tamanho; por isso, cada comando
lê o tamanho na hora.

## Ações

| Ação | Campos | Faz |
|------|--------|-----|
| `click` | `x`, `y`, `button` (`left`, `right`, `middle`), `double` | move até o ponto e clica |
| `move` | `x`, `y` | só move o ponteiro (hover) |
| `drag` | `x`, `y`, `to_x`, `to_y` | aperta, arrasta por pontos intermediários e solta |
| `type` | `text` | digita, com Shift quando precisa; `\n` é Enter |
| `key` | `keys` | uma tecla ou atalho: `Return`, `Escape`, `Tab`, `ctrl+c`, `ctrl+shift+t`, `alt+F4`, `F5`, `Page_Down` |
| `scroll` | `x`, `y`, `direction` (`up`, `down`), `amount` (padrão 3) | gira a roda no ponto |
| `wait` | `ms` (até 60000) | espera entre ações |

Uma lista com qualquer ação inválida é recusada inteira, antes de qualquer evento sair: um `type` pela metade ou um
atalho sem a última tecla deixaria a máquina num estado estranho.

**`type` digita como um teclado US** e cobre só ASCII. Acento e emoji são recusados com uma mensagem. Para outro texto,
use `exec` com `xdotool type` dentro da máquina (o [desktop de exemplo](#exemplo-desktop-com-claude-code) já traz).

## Pela API

```bash
curl -s -o tela.jpg -D - localhost:8080/api/v1/machines/desk/screen | grep -i x-screen
# X-Screen-Width: 1512
# X-Screen-Height: 794
curl -s -X POST localhost:8080/api/v1/machines/desk/screen/actions \
  -d '{"actions":[{"action":"click","x":640,"y":400},{"action":"type","text":"ls"},{"action":"key","keys":"Return"}]}'
# 204
```

| Rota | Resposta |
|------|----------|
| `GET /api/v1/machines/:name/screen` | `200` com a tela em JPEG e o tamanho em `X-Screen-Width` e `X-Screen-Height` |
| `POST /api/v1/machines/:name/screen/actions` | `204` quando todas as ações saíram; até 500 ações por chamada |

Erros: `400 CONFIG_ERROR` para ação inválida ou máquina sem tela; `409 INVALID_STATE` para máquina desligada ou pausada;
`502 SCREEN_UNAVAILABLE` quando o visualizador da máquina não responde.

## Pelos SDKs

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

## Para um agente: `rc mcp`

`rc mcp` é um servidor [MCP](https://modelcontextprotocol.io) por stdio, ligado a uma máquina. Registrado no Claude
Code, ele dá ao agente as ferramentas de operar a VM:

```bash
claude mcp add rc -- rc mcp --name desk
```

| Ferramenta | Faz |
|------------|-----|
| `screenshot` | devolve a tela como imagem e o tamanho em pixels |
| `click`, `move`, `drag`, `type`, `key`, `scroll` | as ações da tabela acima, uma por chamada |
| `actions` | várias ações em ordem, numa chamada |
| `exec` | roda um comando com `sh -c` e devolve o código de saída, o stdout e o stderr |

Falha de uma ferramenta (máquina desligada, ação recusada, comando com saída diferente de 0) volta como resultado com
`isError`, para o agente ler a mensagem e tentar outro caminho.

Medido com o Claude Code: pedimos "tire um screenshot, clique na aba `bash · claude` da barra de tarefas e confirme que
a janela mudou". Ele tirou o print, achou a aba, clicou, viu a tela ainda sem mudar, esperou e confirmou a troca no
segundo print.

## Exemplo: desktop com Claude Code

[`examples/desktop-claude`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude) é um desktop
XFCE num `runner.code`: fundo preto, os atalhos Arquivos, Claude, Navegador e Terminal, Firefox como navegador padrão e
o Claude Code instalado. O atalho Claude abre `claude --dangerously-skip-permissions`; a VM é a caixa de areia. O login
do Claude entra por [credencial](/cli/credentials): o guest só vê um placeholder.

[`examples/desktop-claude-agent`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude-agent)
manda tarefas a esse Claude de três jeitos, todos pelo SDK Go:

| Modo | Como a tarefa chega | Vê na tela? |
|------|---------------------|-------------|
| `headless` | `claude -p` por exec; a resposta volta em JSON | não |
| `gui` | digitada na janela do Claude com `xdotool` (qualquer texto) | sim |
| `keys` | digitada pela API de tela (`ScreenActions`), sem nada instalado na VM | sim |

Cada modo pede ao Claude um arquivo com um conteúdo conhecido e confere o arquivo pela API: "funcionou" é o arquivo
certo no disco da VM, não só uma resposta.
