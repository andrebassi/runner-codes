# install

Instalar o `rc` e provar que ele liga uma VM de verdade. Código de saída 0 sozinho não prova nada: a prova é um
marcador impresso pelo guest e um kernel que não é o do host.

| Tema | No rc |
|---|---|
| instalação | por Nix (`task krun:install`) e `task build`; a prova de boot é o `run.sh` |
| onde as coisas ficam | o rc vive em `bin/` do repositório e o estado em `~/Library/Caches/runner-codes` (Linux: `~/.cache/runner-codes`) |
| armadilhas | o entitlement de hypervisor sai assinado no `task build`; `rc doctor` confere libkrun, libkrunfw e assinatura |
| Windows | fora do escopo (a libkrun upstream não tem WHP) |
| preflight | `rc doctor` |
| prova de boot | `run.sh`: marcador impresso pelo guest e kernel do guest diferente do host |
| limpeza | `run.sh` confere que a VM efêmera não sobrou |
