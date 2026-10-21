---
title: 'machine'
description: 'Máquinas persistentes: uma VM com nome e disco próprio, que guarda o que você grava entre comandos e entre stop e start'
---

`rc run` cria uma VM nova por job e a destrói no fim. Para trabalho em várias etapas (instalar, compilar, testar,
corrigir), use uma **máquina**: uma VM com nome, com disco próprio, que continua existindo entre um comando e outro e
entre `stop` e `start`.

## Em uma tela

```bash
rc machine create --name dev --stack python      # registra e cria o disco (não liga)
rc machine start  --name dev                     # liga; a VM continua depois que o comando sai
rc machine exec   --name dev -- python3 -V       # roda um comando (saída em streaming)
rc machine shell  --name dev                     # terminal interativo (Ctrl+C, vi, resize)
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

Com um [`runner.code`](/cli/runner-code) no diretório, o `create` lê dele a base (stack ou imagem), os recursos, o
ambiente, o init e o workload; as flags passadas vencem o arquivo.

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
- **Terminal (`shell`)**: o guest abre um PTY (devpts montado pelo `machine.sh`) e roda o shell nele. A conexão vira
  um canal de frames nos dois sentidos: `input` (teclas, em base64 para não perder byte de controle) e `resize` do
  host; `output` e `exit` do guest. O terminal do host fica em modo raw, e o `SIGWINCH` vira `resize`. Fechar o
  terminal mata o programa no guest; a máquina continua ligada.
- **Imagem atualizada**: o start usa a imagem atual da stack. Se a imagem for reconstruída, a máquina passa a ver a
  nova por baixo do que ela mesma gravou, como uma atualização de sistema.

## Máquina efêmera: `machine run`

Cria, liga e usa de uma vez:

```bash
rc machine run --net --image alpine -- uname -a          # roda e apaga
rc machine run --net -it --image alpine -- /bin/sh       # terminal; apaga ao sair
echo dados | rc machine run -i --stack python -- sh -c 'read x; echo $x'
rc machine run -d -s runner.code                         # fica ligada com o workload
docker save meuapp | rc machine run --image - -- ./app   # imagem local, sem registry
rc machine run --image ./meuapp.tar -- ./app             # tar do docker save ou diretório OCI layout
```

### Abas

```
runner.codes · agent │ claude │ shell │ eventos │ ● 3000
```

| Aba | O que é |
|-----|---------|
| a do comando (`claude`) | o programa passado depois de `--`; sem comando, o shell da máquina. Fechar a aba não para nada |
| `shell` | um terminal à parte na mesma máquina, aberto no primeiro clique (até 8 sessões vivas por página) |
| `eventos` | a linha do tempo: sessão iniciada, sessão que saiu (com o código), porta aberta e porta fechada |
| `● 3000` | uma aba por porta aberta na máquina, sempre por último: a app num iframe, com **recarregar** e **nova aba ↗** (há sites que não deixam ser exibidos em iframe). Some quando o servidor cai |

| Ponto | Como é |
|-------|--------|
| o que roda | o comando depois do `--`; senão o `cmd`/`entrypoint` do `runner.code`; senão, com `-t`, um shell |
| sem `-d` | nome `run-<hex>`; a máquina é apagada no fim, também no Ctrl+C (rc 130) e no SIGTERM |
| `-d` | nome `default` (ou `--name`); o comando vira o workload e a máquina fica ligada |
| `-t` / `-it` | terminal de verdade (PTY no guest); `-i` sem `-t` só repassa o stdin; sem nenhum, o stdin não é lido |
| imagem local | caminho começando com `/`, `./` ou `../`: tar do `docker save` (ou tar OCI) ou diretório OCI layout; arquitetura diferente da do host é recusada |
| `--image -` | lê o tar do stdin e guarda em `<cache>/imports/sha256-<hash>.tar`: a mesma imagem reaproveita o cache |
| init | roda (o disco é sempre novo) |

As flags de declaração são as do `create`, inclusive `-s`/`-f`, `-v`, `-p`, `--net`, a política de rede e
`--display LxA` (veja [Tela no navegador](/cli/display)).

## Branch e pausa

```bash
rc machine pause  --name dev                    # congela: processos, memória e conexões ficam como estão
rc machine resume --name dev
rc machine branch --from dev --name try-1       # cópia copy-on-write do disco (ligada ou parada)
```

| Ponto | Como é |
|-------|--------|
| pause | SIGSTOP no processo da VM: vCPUs, gateway de rede e visualizador param juntos; `exec`, `cp`, `update` e `start` recusam com `INVALID_STATE`; `stop` retoma antes de desligar |
| branch | o disco da filha é um clone copy-on-write do da mãe (clonefile no APFS, FICLONE no Linux): instantâneo e sem ocupar espaço até divergir. Instalação, `/root` e `/workspace` vêm junto, e o init não roda de novo |
| mãe ligada ou pausada | o guest faz `sync` e a VM congela só durante o clone; o disco sai consistente como depois de uma queda de energia |
| o que não vem | a **execução**: processos e memória. Checkpoint de RAM e CPU exige snapshot, que a libkrun upstream não tem |
| portas e tela | a filha herda as mesmas portas e porta de tela; as duas não sobem juntas sem trocar uma delas |

## Comandos

| Comando | Flags | Observação |
|---------|-------|------------|
| `create` | `--name`, `-f FILE`, `--stack` ou `--image`, `--cpus` (4), `--memory` MiB (8192, elástica), `--disk-gib` (10), `--gpu`, `--gpu-vram`, `-e K=V`, `-w DIR`, `-u USER`, `--init CMD`, `--stop-signal`, `--stop-timeout`, `--stop-on-exit`, `-v`, `-p`, `--docker-socket`, `--expose-socket`, `--mount-socket`, `--secret-env`, `--secret-file`, `--ssh-agent`, `--nested`, `-- WORKLOAD` | não liga a máquina. Sem `-f`, lê `./runner.code` se existir. Veja [runner.code](/cli/runner-code) |
| `start` | `--name` | espera o guest responder (até 30 s); se não responder, mata e mostra o fim do console. Se o processo da VM morre antes (configuração recusada, como `--nested` num host que não aninha), o erro vem na hora com a última linha do log dele. Depois roda o init (1ª vez do disco) e sobe o workload |
| `stop` | `--name`, `--timeout` (padrão: o `stop_timeout` da máquina, ou 10s) | manda o `stop_signal` ao workload antes do sync; parar uma máquina já parada não é erro |
| `status` | `--name`, `--json` | `running` ou `stopped`, com o pid |
| `ls` | `--json` | todas as máquinas |
| `delete` | `--name`, `-f` | ligada só com `-f` |
| `exec` | `--name`, `-e K=V` (repete), `-w DIR`, `-i` (repassa o stdin), `--timeout` (0 = sem limite) | argv **sem shell implícito**: para shell, `-- sh -c '...'`. O rc do `rc` é o do comando (124 no timeout) |
| `shell` | `--name`, `-e K=V`, `-w DIR`, `-- CMD` opcional | terminal de verdade (PTY no guest): Ctrl+C interrompe só o comando, tamanho da janela acompanha o host. Sem comando, abre `bash` se a imagem tiver, senão `sh`. O rc do `rc` é o do `exit` |
| `web-terminal` | `--name`, `-e K=V`, `-w DIR`, `-u USER`, `--port` (0 = livre), `--no-open`, `-- CMD` opcional | o mesmo terminal no navegador: serve em `127.0.0.1` uma página com xterm.js, com token na URL, e abre o navegador. O rc do `rc` é o do programa |
| `cp` | `SRC DST`, com `nome:/caminho` em um dos lados | pedaços de 1 MiB; atômico nos dois lados; até 4 GiB |
| `logs` | `--name`, `--tail N`, `--source all\|workload` | console da VM: o que o guest, o `machine.sh` e o workload escrevem; `workload` filtra só ele |
| `sync` | `--name` | devolve ao host as cópias dos volumes staged (ver [Rede e volumes](/cli/network#volume-staged--v-hostgueststaged)) |
| `data-dir` | `--name` | o diretório da máquina no host (registro, disco, sockets publicados, logs) |
| `prune` | `--all` | o mesmo que `rc image prune`: libera builds antigos de imagem; com `--all`, também as imagens de referência que nenhuma máquina usa |

## Terminal web

```bash
rc machine web-terminal --name dev                         # shell da máquina no navegador
rc machine web-terminal --name agent -- claude --dangerously-skip-permissions
```

O `rc` imprime `web terminal: http://127.0.0.1:PORTA/?t=<token>` e abre o navegador. A página é um terminal de verdade
(xterm.js, embutido no binário, funciona sem internet): Ctrl+C, setas, editores de tela cheia, colar e o tamanho da janela
chegam ao PTY do guest. Links na tela são clicáveis.

| Ponto | Como é |
|-------|--------|
| sessão | nasce com o comando, não com a página: o programa já roda enquanto o navegador abre |
| F5, segunda aba | recebem de novo os últimos 256 KiB da tela e seguem ao vivo; todas as abas digitam na mesma sessão |
| fim | quando o programa sai, a página avisa o código e o `rc` sai com ele; Ctrl+C no terminal de quem rodou encerra a sessão. A máquina segue ligada |
| rede | só `127.0.0.1`; `--port` escolhe a porta (0 pega uma livre) |
| token | 128 bits aleatórios na URL, exigido em toda rota que lê ou escreve na sessão; sem ele, 403 |
| outras defesas | `Host` que não seja de loopback (DNS rebinding) e POST com `Origin` de outro site recebem 403; o corpo do teclado tem teto de 64 KiB |
| máquina parada | recusada antes de abrir servidor ou navegador |
| portas | qualquer porta TCP que um programa abrir na máquina (em `0.0.0.0`, `::`, `127.0.0.1` ou `::1`) vira, em até 2 s, uma aba e responde no `127.0.0.1` do host, no mesmo número; se ele estiver ocupado no host, numa porta livre (a aba mostra `5432 → 51234`). É TCP cru pelo vsock: WebSocket, hot reload e caminho absoluto (o `/docs` do FastAPI) funcionam, e a máquina não precisa de `--net`. Servidor preso só ao IP da `eth0` fica de fora. Enquanto a aba existir, o número fica ocupado no host: um `-p 3000` de outra máquina recusa com `address already in use` |

O [caso de uso do Claude Code](/guides/use-cases) abre o agente assim. Com `IS_SANDBOX=1` no ambiente da máquina, o
`--dangerously-skip-permissions` roda como root; sem ele, o Claude recusa (`cannot be used with root/sudo privileges`).

## Erros

Toda recusa traz um código estável. Decida pelo código, não pela mensagem:

| Código | Quando |
|--------|--------|
| `NOT_FOUND` | máquina (ou arquivo, no `cp`) não existe |
| `CONFLICT` | `create` com nome já usado |
| `INVALID_STATE` | `start` numa ligada; `start` numa pausada (`is paused; resume it ... or stop it first`); `exec`/`cp` numa parada; `delete` numa ligada sem `-f`; `sync` numa parada com volume staged |
| `CONFIG_ERROR` | nome inválido (minúsculas, dígitos e `-`, até 63), stack sem imagem, `--gpu` em stack sem GPU, caminho relativo no `cp` |

## Números medidos

macOS 27, Apple M1 Pro, stack python:

| Medida | Valor |
|--------|-------|
| `start` até o guest responder | 204–244 ms |
| disco de 10 GiB recém-criado, espaço no host | 6,4 MB |
| `cp` de 50 MB para a máquina | 620 ms (~84 MB/s) |
| `cp` de 50 MB da máquina | 594 ms (~88 MB/s) |

## Usuário, segredos, agente SSH e sockets

| Flag | O que faz |
|------|-----------|
| `-u`, `--user NOME\|uid[:gid]` | workload, exec e shell rodam como ele; o init roda como root. `exec --user` e `shell --user` trocam só aquele comando |
| `--secret-env VAR=HOST_VAR` | o valor da variável do host, lido no start, no ambiente de todo comando; fica só na memória do guest, nunca no registro |
| `--secret-file VAR=/abs/arquivo` | o mesmo, com o conteúdo de um arquivo do host. Segredo com o nome de uma credencial alimenta a binding e não entra na VM |
| `--ssh-agent` | o agente de quem dá o start (`SSH_AUTH_SOCK`) aparece no guest; a chave privada fica no host |
| `--docker-socket` | o `/var/run/docker.sock` do guest vira `docker.sock` no `data-dir` da máquina |
| `--expose-socket GUEST[:HOST]` | um socket Unix que o workload escuta, alcançável no host (padrão: o nome dele no `data-dir`) |
| `--mount-socket HOST:GUEST` | um socket do host dentro da máquina; o diretório do guest é criado se faltar. Até 64 pontes por máquina |
| `--nested` | expõe as extensões de virtualização do host (`/dev/kvm` no guest). Precisa de KVM aninhado no Linux ou de um Mac M3+ com macOS 15+; `rc doctor` diz se este host aninha. O `runner.code` não liga |

As pontes de socket só existem na CLI: pela API seriam caminhos arbitrários no disco do servidor.

## API

As mesmas operações em `rc serve`. Veja [Visão geral da API](/api-reference/overview#máquinas).

## Fora desta versão

O WebSocket interativo da API (o terminal hoje é só pela CLI), `machine stats` e o checkpoint de RAM (a libkrun
upstream não tem snapshot).
Rede, portas e volumes estão em [Rede e volumes](/cli/network). O plano está na
[Referência para agentes](/agents#plano-de-evolução).
