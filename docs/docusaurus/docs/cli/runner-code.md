---
title: 'runner.code'
description: 'O arquivo runner.code: a máquina declarada em TOML (stack ou imagem, recursos, env, init e workload), o equivalente do runner.code'
---

`rc machine create` aceita flags, mas uma máquina com ambiente, comandos de preparo e um processo que fica rodando
vira uma linha longa demais para lembrar e impossível de revisar. O **`runner.code`** é essa declaração num arquivo
TOML que vai para o repositório junto com o código. É o equivalente do
[runner.code]().

## Em uma tela

```toml
# runner.code
stack = "python"                 # ou: image = "redis:7-alpine"
cpus = 2
memory = 2048                    # MiB
storage = 10                     # GiB do disco gravável

env = ["APP_ENV=dev"]
workdir = "/app"

init = [                         # roda UMA vez, no primeiro start do disco
  "pip install --user flask",
]

entrypoint = ["python3", "-m"]   # workload: entrypoint + cmd,
cmd = ["http.server", "8000"]    # sobe a cada start
stop_signal = "SIGINT"
stop_timeout = 30

[dev]                            # perfil de desenvolvimento, aplicado por cima
env = ["DEBUG=1"]
```

```bash
rc machine create --name web               # lê ./runner.code
rc machine create --name web -f infra/runner.code
rc machine start  --name web               # init (1ª vez) + workload
rc machine logs   --name web --source workload
rc machine stop   --name web               # SIGINT ao workload, até 30 s, sync, desliga
```

## Chaves

| Chave | Tipo | Padrão | O que faz |
|-------|------|--------|-----------|
| `stack` | texto | — | stack do catálogo como base (`python`, `go`, ...). Usa a imagem com warmup |
| `image` | texto | — | **qualquer** imagem OCI como base (`redis:7-alpine`). Construída no primeiro uso, sem warmup |
| `cpus` | inteiro | 2 | vCPUs (teto: o catálogo/limite global) |
| `memory` | inteiro, MiB | 2048 | teto de memória; a libkrun só aloca o que o guest toca |
| `storage` | inteiro, GiB | 10 | disco gravável da máquina (esparso: ocupa no host o que foi gravado) |
| `gpu` | booleano | `false` | GPU do host via Vulkan (Venus). Com `stack`, a stack precisa declarar GPU; com `image`, a imagem precisa ter o Mesa com Venus |
| `gpu_vram` | inteiro, MiB | o da stack | janela de memória compartilhada da GPU. Exige `gpu = true` |
| `env` | lista `K=V` | — | ambiente do init, do workload, do `exec` e do `shell` |
| `workdir` | caminho absoluto | — | diretório do init, do workload e do `exec`/`shell` sem `-w`. **Criado** no start se não existir |
| `init` | lista de comandos | — | cada item roda com `sh -c`, em ordem, **uma vez por disco** |
| `entrypoint` | lista | — | início do argv do workload |
| `cmd` | lista | — | resto do argv do workload |
| `stop_signal` | texto | `SIGTERM` | sinal mandado ao workload no stop (`SIGINT`, `SIGTERM`, `SIGQUIT`, `SIGHUP`, `SIGUSR1`, `SIGUSR2`, `SIGKILL`, com ou sem `SIG`, ou o número) |
| `stop_timeout` | inteiro, s | 10 | quanto o workload tem para sair; depois, `SIGKILL`. Máximo 300 |
| `stop_on_exit` | booleano | `false` | quando o workload sai, a máquina grava o disco e desliga sozinha. Exige workload |
| `[dev]` | tabela | — | `env` **soma** ao de cima; `workdir` e `init` **substituem** |

## Ordem de precedência

**flag > arquivo > padrão.** Uma flag só conta se foi digitada: `--cpus 0` não é o mesmo que não dizer nada.

| Flag | Chave | Como combina |
|------|-------|--------------|
| `-f`, `--file` | — | sem ela, usa `./runner.code` se existir |
| `--stack`, `--image` | `stack`, `image` | uma limpa a outra |
| `--cpus`, `--memory`, `--disk-gib`, `--gpu`, `--gpu-vram` | `cpus`, `memory`, `storage`, `gpu`, `gpu_vram` | substitui |
| `-e`, `--env` (repete) | `env` | **soma** ao do arquivo; dentro da VM, a última definição de uma chave vence |
| `-w`, `--workdir` | `workdir` | substitui |
| `--init` (repete) | `init` | substitui a lista inteira |
| `--stop-signal`, `--stop-timeout`, `--stop-on-exit` | as mesmas | substitui |
| `-- CMD ...` | `entrypoint` + `cmd` | substitui o workload inteiro |

`--name` não existe no arquivo de propósito: o mesmo `runner.code` serve para várias máquinas.

## Ciclo de vida

```
rc machine start
  │
  ├─ VM liga, guest responde                       (igual a uma máquina sem runner.code)
  ├─ mkdir -p <workdir>                            (só se houver init ou workload)
  ├─ init já rodou neste disco?  ── sim ──┐        (marcador /var/lib/runner/init-done)
  │        └─ não: cada item com sh -c,   │
  │           env + workdir; marca no fim │
  ├─ workload sobe em segundo plano  ◀────┘        (grupo de processos próprio)
  │     saída → console, cada linha com "[workload] "
  └─ start devolve

rc machine stop
  ├─ stop_signal ao grupo do workload
  ├─ espera até stop_timeout  ── não saiu ──▶ SIGKILL
  ├─ sync do disco
  └─ guest sai, VM desliga
```

Pontos que mordem:

- **Init que falha deixa a máquina ligada** e **não** marca o disco. O `start` devolve erro com o comando e o stderr;
  `rc machine shell` deixa ver o estado, e o próximo `start` (depois de um `stop`) roda o init de novo, do começo.
  Escreva comandos de init que aguentem rodar duas vezes (`mkdir -p`, `pip install`).
- **O init roda uma vez por disco, não por arquivo.** Mudar o `init` do `runner.code` não roda nada numa máquina já
  criada; o arquivo é lido no `create`. Para refazer: `rc machine delete` e `create` de novo.
- **O workload sobe a cada `start`**, inclusive depois de um `stop_on_exit`.
- O stop dá ao guest `stop_timeout` + 5 s antes de matar a VM pelo host: o workload tem o prazo inteiro, e o sync do
  disco ainda cabe depois.
- `--timeout` no `rc machine stop` vale só para aquele stop e passa por cima do `stop_timeout`.

## Logs

O console da máquina tem o que o guest escreve e a saída do workload, cada linha desta com a marca `[workload] `.

```bash
rc machine logs --name web                        # tudo
rc machine logs --name web --source workload      # só o workload, sem a marca
rc machine logs --name web --source workload --tail 20
```

Quando o workload sai, a última linha é `exited with code N`.

## Imagem qualquer

```toml
image = "redis:7-alpine"
cmd = ["redis-server", "--save", "60", "1"]
```

A imagem é baixada no host, achatada e convertida num ext4 pela mesma VM de build das stacks, com o guest e o
`start.sh` injetados e o `Env` da imagem preservado. O nome interno é `img-` + 12 hex do sha256 da referência: a
mesma referência reaproveita o ext4 do cache.

Diferença para `stack`: não há warmup nem entrada no catálogo, e a imagem precisa ser Linux da arquitetura do host
(arm64 no Mac, amd64 ou arm64 no Linux).

## O que é recusado

Chave do runner.code que o `runner.code` reconhece mas ainda não implementa é **erro com o motivo**, nunca ignorada em
silêncio: `[network] allow_hosts` ignorado faria alguém achar que a máquina tem filtro de rede.

| Chave | Situação |
|-------|----------|
| `net`, `[network]`, `ports`, `net_backend` | rede: Fase C. A máquina segue **sem rede** |
| `[auth]`, `[secrets]` | SSH agent e segredos: Fase B |
| `volumes`, `allow_system_mounts` | montagem de diretório do host: fora, por desenho (copie com `rc machine cp`) |
| `user`, `overlay`, `health`, `restart`, `[artifact]`, `[pack]`, `rosetta`, `cuda`, ... | não implementados |
| qualquer outra chave | `unknown key(s): ...` (erro de digitação não passa) |

## API

`POST /api/v1/machines` aceita os mesmos campos no JSON (`image`, `env`, `workdir`, `init`, `workload`,
`stop_signal`, `stop_timeout`, `stop_on_exit`, `gpu_vram_mib`) ou o arquivo inteiro em `runner_code`. Com os dois,
o arquivo é a base e todo campo não vazio do JSON vence, como as flags na CLI.

```bash
curl -s localhost:8080/api/v1/machines -d "$(jq -n --rawfile rc runner.code '{name:"web", runner_code:$rc}')"
curl -s 'localhost:8080/api/v1/machines/web/logs?source=workload&tail=20'
curl -s -X POST localhost:8080/api/v1/machines/web/stop          # usa o stop_timeout da máquina
```
