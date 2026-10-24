# desktop-claude

Um desktop Linux numa microVM do rc, declarado inteiro no [`runner.code`](runner.code):

- XFCE no Xorg, fundo preto, só a barra de cima (janelas abertas e relógio);
- atalhos **Arquivos**, **Claude**, **Navegador** e **Terminal**, abertos com um clique e sempre maximizados;
- Firefox como navegador padrão (inclusive para o login do `claude`);
- Claude Code instalado; o atalho Claude abre `claude --dangerously-skip-permissions` (a VM é a caixa de areia);
- a tela acompanha a janela do navegador e o logout religa a sessão sozinho.
- a página da tela se chama `superbot.computer` (`display_title`), na barra e na aba do navegador.

## Pela CLI

```bash
cd examples/desktop-claude
rc machine create --name desk                      # lê o runner.code desta pasta
CLAUDE_CODE_OAUTH_TOKEN="$(pass show bassi/anthropic/claude-code-token)" rc machine start --name desk
open http://127.0.0.1:6080
```

O token do Claude entra por credencial: o guest só vê `RC_PLACEHOLDER_CLAUDE_...`, e o gateway do rc põe o valor real
nas chamadas a `api.anthropic.com`. Sem o token, o desktop sobe igual; o `claude` pede login.

## Pelo SDK Go

```bash
rc serve --listen 127.0.0.1:8080 &
go run ./examples/desktop-claude -server http://127.0.0.1:8080          # -delete apaga no fim; -display-port 6081
```

O [`main.go`](main.go) manda este mesmo `runner.code` no campo `runner_code` da API, liga a máquina e confere por
dentro: o Claude Code, o Firefox padrão, os quatro atalhos, a tela depois de 200 movimentos de mouse e de um pedido de
1200x700, o terminal maximizado e a sessão de volta depois de um logout.

## Fundo de tela do superbot.computer

[`superbot-wallpaper.svg`](superbot-wallpaper.svg) é o desenho (vetor, editável); o
[`superbot-wallpaper.png`](superbot-wallpaper.png) sai dele em 3024x1476, o dobro da tela, e cobre qualquer tamanho de
janela no modo "zoomed" do XFCE. O `runner.code` sobe com fundo preto; para aplicar numa máquina ligada:

```bash
rsvg-convert -o superbot-wallpaper.png superbot-wallpaper.svg          # só depois de editar o SVG
rc machine cp superbot-wallpaper.png desk:/usr/share/backgrounds/superbot.png
rc machine exec --name desk -- sh -c 'p=$(pgrep -x xfdesktop); export $(tr "\0" "\n" < /proc/$p/environ | grep -E "^(DBUS_SESSION_BUS_ADDRESS|DISPLAY)=");
  b=/backdrop/screen0/monitorVirtual-1/workspace0;
  xfconf-query -c xfce4-desktop -p $b/image-style -n -t int -s 5;
  xfconf-query -c xfce4-desktop -p $b/last-image -n -t string -s /usr/share/backgrounds/superbot.png'
```

O disco da máquina é dela: o fundo, os arquivos criados e o que for instalado sobrevivem a `stop`/`start`. Só o `/tmp`
(em memória) zera a cada boot, e `rc machine delete` apaga tudo.

## Mais rápido: pack

O primeiro start instala tudo (~30 s com o cache do apt e do npm). Para máquinas seguintes, empacote a pronta:

```bash
rc machine stop --name desk
rc pack create --from-vm desk --output ./desk-pack      # ~20 s, ~1,1 GB
rc machine create --name desk2 --from ./desk-pack       # start em ~5,6 s, sem o init
```

## O que este desktop precisou (e por quê)

| Ajuste | Motivo |
|--------|--------|
| `AccelMethod "none"` no Xorg | a tela do rc é 2D, sem GL: o glamor derruba o Xorg |
| `SWcursor "true"` | o cursor por hardware travava o Xorg no primeiro movimento de mouse |
| `SYSTEMD_IGNORE_CHROOT=1` antes do `udevadm trigger` | a imagem roda num chroot e o udevadm ignorava o pedido: sem teclado e mouse no Xorg |
| gancho `/etc/rc/display-resize` | o rc chama com o tamanho da janela; o gancho faz `gtf` + `xrandr` |
| `maximize-windows` (xprop + wmctrl) | o XFCE não tem "abrir tudo maximizado" |
| `skipDangerousModePermissionPrompt` (no exemplo do agente) | sem ele, o Claude abre no aviso do modo sem permissões |

Para mandar tarefas a esse Claude por programa, veja [`../desktop-claude-agent`](../desktop-claude-agent).
