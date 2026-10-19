# scripts

Scripts chamados pelas tasks do `Taskfile.yaml`. Rode pela task, não direto.

| Script | Task | O que faz |
|---|---|---|
| `build.sh` | `task build` | compila o host (`cmd/rc`) e o guest estático (`cmd/runner-guest`, Linux), reprova se o guest sair dinâmico e assina o host no macOS |
| `krun-install.sh` | `task krun:install` | instala libkrun + libkrunfw pelo Nix (`.#krun` do `flake.nix`) |
| `coverage-gate.sh` | `task test:cov` | gate de cobertura que reprova: 90% total, 100% domínio, catraca do guest |
| `targets-gate.sh` | `task test:targets` | confere que cada alvo declarado compila e que o Windows é recusado com o motivo |

`benchmark-on-demand.sh` é local e fica no `.gitignore`, porque carrega credencial.
