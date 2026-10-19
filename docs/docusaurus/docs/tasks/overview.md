---
title: 'Visão geral das tasks'
description: 'As tasks do Taskfile.yaml do Runner Codes'
---

## Lista

O projeto usa [Task](https://taskfile.dev). `task` sem argumento lista tudo.

| Task | O que faz |
|------|-----------|
| `task krun:install` | instala libkrun + libkrunfw pelo Nix (`.#krun`) |
| `task build` | compila host e guest em `bin/` e assina o host no macOS |
| `task image:build -- --stack <l>` | gera o ext4 da linguagem (`-- --all` para todas); compila antes |
| `task run -- <flags>` | executa um código numa VM nova |
| `task serve -- <flags>` | sobe a API em `127.0.0.1:8080` com pool; compila antes |
| `task doctor` | confere se o host roda microVMs |
| `task test` | testes de unidade com race detector |
| `task test:cov` | gate de cobertura: reprova abaixo de 90% total ou 100% no domínio |
| `task test:targets` | gate multi-alvo: darwin/arm64, linux/amd64 e linux/arm64 compilam; windows é recusado |
| `task test:e2e` | ponta a ponta contra a libkrun real; precisa das imagens construídas |
| `task gates` | `test:targets`, `test:cov` e `test:e2e`, nessa ordem |
| `task clean` | apaga `bin/`; o cache de imagens fica onde está |

## Fluxo típico

```bash
task krun:install
task build
task doctor
task image:build -- --stack python
task run -- --stack python --code 'print(1)'
task serve
```

Detalhes em [Build](/tasks/build) e [Testes](/tasks/testing).
