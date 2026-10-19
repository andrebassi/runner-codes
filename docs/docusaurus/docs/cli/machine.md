---
title: 'machine'
description: 'Máquinas persistentes: uma VM com nome e disco próprio, que guarda o que você grava entre comandos e entre stop e start'
---

`rc run` cria uma VM nova por job e a destrói no fim. Para trabalho em várias etapas (instalar, compilar, testar,
corrigir), use uma **máquina**: uma VM com nome, com disco próprio, que continua existindo entre um comando e outro e
entre `stop` e `start`. É o equivalente do `smolvm machine`.

## Em uma tela

```bash
rc machine create --name dev --stack python      # registra e cria o disco (não liga)
rc machine start  --name dev                     # liga; a VM continua depois que o comando sai
rc machine exec   --name dev -- python3 -V       # roda um comando (saída em streaming)
rc machine cp ./app.py dev:/workspace/app.py     # sobe arquivo
rc machine exec   --name dev -- python3 /workspace/app.py
rc machine cp dev:/workspace/result.json .       # baixa arquivo
rc machine logs   --name dev --tail 20           # console da máquina (funciona parada)
rc machine stop   --name dev                     # grava o disco e desliga
rc machine ls                                    # todas, com o estado
rc machine delete --name dev                     # apaga máquina e disco (-f se ligada)
```

`--name` é opcional em todos: o padrão é `$RC_MACHINE_NAME` ou, sem ela, `default`. Com o direnv dá para fixar uma
máquina por projeto.

## O que persiste

| Caminho | Persiste? | Onde mora |
|---------|-----------|-----------|
| `/` (pacotes, `/etc`, `/opt`, `/root`...) | **sim** | camada gravável (overlay) no disco da máquina, por cima da imagem da stack |
| `/workspace` | **sim** | diretório no disco da máquina; o lugar indicado para código, dados e resultados |
| `/tmp` | **não** | tmpfs de 256 MiB, vazio a cada start |

O `HOME` é `/root`, que também persiste.

## Como funciona

```
host (macOS)                                      │ guest
                                                  │
~/Library/Caches/runner-codes/machines/dev/       │
  machine.json   registro (stack, cpus, ...)      │
  disk.ext4      disco da máquina ── /dev/vdb ────┼─▶ /persist ─┬─ upper/      (o que foi gravado)
  spec.json      spec da VM                       │             ├─ work/       (do overlayfs)
  console.log    console (rc machine logs)        │             └─ workspace/  ─▶ /workspace
  v.sock         canal com o guest (vsock 5000)   │
  vmm.pid        processo da VM                   │  imagem da stack ── /dev/vda (read-only)
                                                  │  overlay: imagem por baixo + upper por cima ─▶ /
```

- **Disco**: na primeira vez, a VM de build formata um ext4 vazio esparso (`disks/data-10g.ext4`). Cada máquina
  recebe um **clone** dele: clonefile no APFS, FICLONE no btrfs/XFS, cópia esparsa no resto. Medido no macOS: um disco
  de 10 GiB ocupa 6,4 MB no host logo depois de criado.
- **Boot**: o primeiro processo da máquina é o `/opt/runner/machine.sh`, no lugar do `start.sh` dos jobs. Ele monta o
  disco em `/persist` e um overlayfs com a imagem por baixo e o `upper/` por cima, liga `/workspace`, e roda o
  guest-runner num `chroot` ali.
- **VM destacada**: o processo `rc vmm` da máquina roda em sessão própria (`setsid`), sem o pipe de vigia e sem
  `Pdeathsig`. Por isso ele sobrevive ao `rc` que o ligou. O pid fica em `vmm.pid`, e "ligada" quer dizer pid vivo. Os
  jobs efêmeros não mudam: a VM deles continua morrendo junto com o pai.
- **Stop**: o host pede `shutdown` ao guest, que faz `sync` (grava no ext4 o que está em cache), responde e sai, e a
  VM desliga. Se não terminar em `--timeout` (padrão 10 s), recebe `SIGKILL`. `--timeout 0` mata na hora.
- **Imagem atualizada**: o start usa a imagem atual da stack. Se a imagem for reconstruída, a máquina passa a ver a
  nova por baixo do que ela mesma gravou, como uma atualização de sistema.

## Comandos

| Comando | Flags | Observação |
|---------|-------|------------|
| `create` | `--name`, `--stack` (obrigatório), `--cpus` (2), `--memory` MiB (2048), `--disk-gib` (10), `--gpu` | não liga a máquina |
| `start` | `--name` | espera o guest responder (até 30 s); se não responder, mata e mostra o fim do console |
| `stop` | `--name`, `--timeout` (10s) | parar uma máquina já parada não é erro |
| `status` | `--name`, `--json` | `running` ou `stopped`, com o pid |
| `ls` | `--json` | todas as máquinas |
| `delete` | `--name`, `-f` | ligada só com `-f` |
| `exec` | `--name`, `-e K=V` (repete), `-w DIR`, `-i` (repassa o stdin), `--timeout` (0 = sem limite) | argv **sem shell implícito**: para shell, `-- sh -c '...'`. O rc do `rc` é o do comando (124 no timeout) |
| `cp` | `SRC DST`, com `nome:/caminho` em um dos lados | pedaços de 1 MiB; atômico nos dois lados; até 4 GiB |
| `logs` | `--name`, `--tail N` | console da VM: o que o guest e o `machine.sh` escrevem |

## Erros

Toda recusa traz um código estável, o mesmo nome usado pelo smolvm. Decida pelo código, não pela mensagem:

| Código | Quando |
|--------|--------|
| `NOT_FOUND` | máquina (ou arquivo, no `cp`) não existe |
| `CONFLICT` | `create` com nome já usado |
| `INVALID_STATE` | `start` numa ligada; `exec`/`cp` numa parada; `delete` numa ligada sem `-f` |
| `CONFIG_ERROR` | nome inválido (minúsculas, dígitos e `-`, até 63), stack sem imagem, `--gpu` em stack sem GPU, caminho relativo no `cp` |

## Números medidos

macOS 27, Apple M1 Pro, stack python:

| Medida | Valor |
|--------|-------|
| `start` até o guest responder | 204–244 ms |
| disco de 10 GiB recém-criado, espaço no host | 6,4 MB |
| `cp` de 50 MB para a máquina | 620 ms (~84 MB/s) |
| `cp` de 50 MB da máquina | 594 ms (~88 MB/s) |

## API

As mesmas operações em `rc serve`. Veja [Visão geral da API](/api-reference/overview#máquinas).

## Fora desta versão

Rede (`--net`, `--allow-host`), `--secret-env`, `--ssh-agent`, shell interativo com TTY na máquina
(`rc machine shell`), `machine update` e `machine stats`. O plano está na
[Referência para agentes](/agents#plano-de-evolução).
