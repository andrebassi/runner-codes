# Exemplos

| Pasta | O que mostra |
|-------|--------------|
| [`sdk-go`](sdk-go/main.go) | o SDK Go (`pkg/client`) contra um `rc serve`: um job efêmero e uma máquina do começo ao fim |
| [`sdk-python`](sdk-python/main.py) | o mesmo com o SDK Python (`sdk/python`) |
| [`desktop-claude`](desktop-claude/runner.code) | desktop XFCE com Claude Code e Firefox num `runner.code`; o `main.go` cria e confere pelo SDK Go |
| [`desktop-claude-agent`](desktop-claude-agent/main.go) | tarefas ao Claude dessa VM de três jeitos: `claude -p`, digitada na janela (`xdotool`), pela API de tela |
| [`guides/`](guides/) | guias de uso do `rc`, cada um com um `run.sh` que prova o que diz numa VM de verdade |

```bash
./bin/rc serve --listen 127.0.0.1:8080 &
go run ./examples/sdk-go http://127.0.0.1:8080
PYTHONPATH=sdk/python python3 examples/sdk-python/main.py http://127.0.0.1:8080
```

Saída esperada nos dois: `job: 42`, a máquina `sdk-...` em `running`, o `exec` do arquivo gravado nela e a máquina
apagada no fim (runbook, seção 10b).

Para declarar uma máquina num arquivo versionado, veja o formato do [`runner.code`](../docs/docusaurus/docs/cli/runner-code.md).
