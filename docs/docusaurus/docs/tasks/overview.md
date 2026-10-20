---
title: 'Visão geral das tasks'
description: 'As tasks do Taskfile.yaml do Runner Codes'
---

## Lista

O projeto usa [Task](https://taskfile.dev). `task` sem argumento lista tudo.

| Task | O que faz |
|------|-----------|
| `task krun:install` | instala libkrun + libkrunfw pelo Nix (`.#krun`) |
| `task build` | compila host (`bin/rc`) e guest em `bin/` e assina o host no macOS |
| `task image:build -- --stack <l>` | gera o ext4 da stack (`-- --all` para todas); compila antes |
| `task run -- <flags>` | executa um código numa VM nova |
| `task shell -- --stack <l>` | shell interativo numa VM nova |
| `task serve -- <flags>` | sobe a API em `127.0.0.1:8080` com pool; compila antes |
| `task doctor` | confere se o host roda microVMs |
| `task test` | testes de unidade com race detector |
| `task test:cov` | gate de cobertura: reprova abaixo de 90% total, 100% no domínio ou da catraca do guest; inclui o SDK Python |
| `task sdk:py:cov` | só o SDK Python: linha e branch, cada um ≥ 90% |
| `task test:targets` | gate multi-alvo: darwin/arm64, linux/amd64 e linux/arm64 compilam; windows é recusado |
| `task test:e2e` | ponta a ponta contra a libkrun real; precisa das imagens construídas |
| `task runbook` | executa o `docs/runbook.md` e confere cada resultado esperado |
| `task guides` | roda os guias de `examples/guides`, cada um numa VM real (`DIRS="pack teardown"` para alguns) |
| `task site:demos` | as execuções reais por trás das demos do site (`SCENES="network pack"` para algumas) |
| `task gates` | `test:targets`, `test:cov`, `test:e2e` e `runbook`, nessa ordem |
| `task ship -- "<título>" <arquivo>` | publica a branch atual, abre o PR e faz o merge |
| `task clean` | apaga `bin/`; o cache de imagens fica onde está |

## Fluxo típico

```bash
task krun:install
task build
task doctor
task image:build -- --stack python
task run -- --stack python --code 'print(1)'
./bin/rc machine run --stack python -- python3 -V
task serve
task runbook
```

Detalhes em [Build](/tasks/build) e [Testes](/tasks/testing).
