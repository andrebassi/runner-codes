# desktop-claude-agent

Manda tarefas ao Claude Code de uma máquina do [`desktop-claude`](../desktop-claude), só pelo SDK Go, de três jeitos:

| Modo | Como a tarefa chega | Vê na tela? | Limite |
|------|---------------------|-------------|--------|
| `headless` | `claude -p` por exec; a resposta volta em JSON | não | — |
| `gui` | a janela do Claude abre na tela e a tarefa é digitada nela com `xdotool` | sim | precisa do `xdotool` na imagem (o desktop traz) |
| `keys` | a tarefa é digitada pela API de tela do rc (`ScreenActions`) | sim | só ASCII (teclado US) |

Cada modo pede ao Claude um arquivo em `/root/agent` com um conteúdo conhecido e confere o arquivo pela API: "funcionou"
é o arquivo certo no disco da VM, não só uma resposta. Os modos com tela salvam uma captura (`agent-gui.jpg`,
`agent-keys.jpg`).

```bash
CLAUDE_CODE_OAUTH_TOKEN="$(pass show bassi/anthropic/claude-code-token)" rc serve --listen 127.0.0.1:8080 &
go run ./examples/desktop-claude -server http://127.0.0.1:8080
go run ./examples/desktop-claude-agent -server http://127.0.0.1:8080                   # os três modos
go run ./examples/desktop-claude-agent -server http://127.0.0.1:8080 -mode gui -task "Crie um app Flask em /root/app"
```

Antes da primeira tarefa, o programa marca no Claude da máquina o onboarding feito, o tema, a pasta como confiável e o
aceite do modo sem permissões (`skipDangerousModePermissionPrompt` nas configurações do usuário). Sem isso, a janela
abre numa dessas telas, e o Enter da tarefa digitada responde a ela.

Cada tarefa usa a assinatura do Claude dona do token.

## Operar a tela você mesmo

O mesmo controle que o modo `keys` usa está na CLI e no MCP:

```bash
rc machine screen --name desktop-claude screenshot -o tela.jpg
rc machine screen --name desktop-claude click 400 735
rc machine screen --name desktop-claude type "Liste os arquivos de /root/agent"
rc machine screen --name desktop-claude key Return
claude mcp add rc -- rc mcp --name desktop-claude     # o Claude Code da sua máquina opera a VM
```
