# teardown

Parar, apagar e provar que ficou limpo: nenhuma máquina, nenhum processo de VM, nenhum diretório.

| Tema | No rc |
|---|---|
| fluxo | `run.sh`: stop, `delete --force --cascade`, `machine prune`, `pack prune` e a prova de limpeza |
| armadilhas | `run.sh` mede as duas: Ctrl-C no run leva a VM; `kill -9` no CLI também (no rc isso foi corrigido: a VM ficava viva) |
| onde o estado mora | `~/Library/Caches/runner-codes` (Linux `~/.cache/runner-codes`); não há `--uninstall` |
| Kubernetes | fora do escopo: não há shim de containerd |
| Windows | fora do escopo |
| prova de limpeza | `run.sh`: nenhuma máquina, nenhum `rc vmm`, nenhum diretório |
