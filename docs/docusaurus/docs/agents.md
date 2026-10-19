---
title: 'Referência para agentes'
description: 'O que o rc faz hoje, como usar cada comando, o que ele tem e não tem em relação ao smolvm, e o plano de evolução'
---

Referência curta e completa para quem opera o `rc`, gente ou agente de IA: o que existe, como chamar e o que cada
coisa garante. Segue o formato do "Agent Reference" do smolvm, com uma diferença: aqui cada linha diz o que **existe
hoje**. O que ainda não existe fica na seção [Comparação com o smolvm](#comparação-com-o-smolvm) e no
[plano](#plano-de-evolução).

O `rc` executa código não confiável (gerado por LLM) numa **microVM nova por job**, com kernel próprio, sem rede, com
root read-only, e destrói a VM no fim. O VMM é a libkrun e o kernel vem da libkrunfw. O boot leva cerca de 260 ms.

## Plataformas

| Host | Guest | Hipervisor | Situação |
|------|-------|-----------|----------|
| macOS Apple Silicon | Linux arm64 | Hypervisor.framework | testado de ponta a ponta |
| Linux x86_64 / aarch64 | Linux da mesma arquitetura | KVM (`/dev/kvm`) | compila; confinamento testado; boot completo não medido (sem host com KVM) |
| Windows | — | — | fora: a libkrun upstream não tem WHP. O build para Windows falha de propósito |

## Instalação

```bash
task krun:install                    # libkrun + libkrunfw (+ GPU no macOS) pelo Nix: nix profile add .#krun
task build                           # bin/rc (assinado no macOS) + bin/rc-guest-linux-<arch>
./bin/rc image build --stack python  # ou --all; uma imagem ext4 por stack
./bin/rc doctor                      # confere host, libkrun, GPU, guest, sandbox e imagens
```

Sem Homebrew nem Docker: tudo pelo Nix. `nix develop` dá Go, task, coreutils e Node para desenvolver.

## Referência rápida

```bash
# executar código numa VM nova (boot a frio, a VM some no fim)
./bin/rc run --stack python --code 'print(2**10)'
./bin/rc run --stack go --file main.go --stdin 'entrada' --timeout 30s

# a saída aparece enquanto o programa roda (streaming)
./bin/rc run --stack python --code $'import time\nprint("a")\ntime.sleep(2)\nprint("b")'

# shell interativo numa VM nova (TTY de verdade: Ctrl+C, tamanho da janela)
./bin/rc shell --stack bash

# GPU do host por Vulkan (só em stack com gpu)
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'

# máquina persistente: o que você grava fica entre comandos e entre stop/start
./bin/rc machine create --name dev --stack python
./bin/rc machine start  --name dev
./bin/rc machine exec   --name dev -- sh -c 'mkdir -p /workspace/out && echo pronto > /etc/motd'   # sem rede: --net é a Fase C
./bin/rc machine cp ./app.py dev:/workspace/app.py
./bin/rc machine exec   --name dev -- python3 /workspace/app.py
./bin/rc machine stop   --name dev

# API HTTP com pool de VMs prontas (e as rotas de máquina)
./bin/rc serve --listen 127.0.0.1:8080 --pool-size 2
```

## Quando usar o quê

| Objetivo | Comando |
|----------|---------|
| rodar um trecho de código isolado e pegar a saída | `rc run --stack S --code '...'` |
| ver a saída enquanto roda (build, teste longo) | `rc run` (já é streaming) ou `POST /api/v1/execute/stream` |
| explorar a imagem de uma stack à mão | `rc shell --stack S` |
| ambiente de várias etapas que guarda estado | `rc machine create` → `start` → `exec`/`cp` → `stop` ([machine](/cli/machine)) |
| servir muitos jobs com latência baixa | `rc serve`: o pool tira o boot do caminho (26–78 ms por job) |
| usar a GPU (IA, Vulkan) | `--gpu` na stack `llama` ([GPU](/advanced/gpu)) |
| saber se o host está pronto | `rc doctor` |
| listar stacks e quais imagens estão construídas | `rc languages` |

## Estrutura da CLI

```
rc run    --stack S (--code C | --file F) [--stdin T] [--timeout 10s] [--gpu]
rc shell  --stack S [--gpu]
rc machine create --name N --stack S [--cpus 2] [--memory 2048] [--disk-gib 10] [--gpu]
rc machine start|stop|status|delete [--name N]          # stop --timeout 10s; delete -f
rc machine ls [--json]
rc machine exec [--name N] [-e K=V] [-w DIR] [-i] [--timeout D] -- CMD ARGS...
rc machine cp SRC DST                                    # um lado é nome:/caminho
rc machine logs [--name N] [--tail N]
rc serve  [--listen 127.0.0.1:8080] [--pool-size 2]
rc image build (--stack S ... | --all)
rc languages
rc doctor
rc vmm --spec <json> [--watch-fd 3] [--lib <libkrun>]   # interno: um processo por VM
```

`rc run` sai com **0** só quando o programa saiu com 0. Saída diferente de 0, timeout, compilação falha e job recusado
dão rc 1.

## Flags

| Flag | Comando | O que faz |
|------|---------|-----------|
| `--stack` | run, shell, image build | stack do catálogo (`python`, `go`, `rust`, `java`, `nodejs`, `c`, `cpp`, `ruby`, `php`, `perl`, `bash`, `sh`, `awk`, `jq`, `sqlite`, `llama`) |
| `--code` / `--file` | run | código do arquivo principal, direto ou de um arquivo local |
| `--stdin` | run | entrada padrão do programa (até 1 MB) |
| `--timeout` | run | prazo do job, padrão 10 s, máximo 300 s; ao vencer, a VM é morta com `SIGKILL` |
| `--gpu` | run, shell | GPU do host por Vulkan (virtio-gpu/Venus); só em stack com `gpu: true`, senão E007 |
| `--listen` | serve | endereço; padrão loopback, porque a API não tem autenticação |
| `--pool-size` | serve | VMs prontas por stack (padrão 2) |
| `--all` | image build | constrói todas as stacks do catálogo |

## Variáveis de ambiente

| Variável | Padrão | Uso |
|----------|--------|-----|
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS) | imagens, VM de build, diretórios das VMs |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib` (macOS); no Linux o do perfil do Nix ou `libkrun.so.1` | qual libkrun carregar |
| `RUNNER_GUEST_BIN` | `rc-guest-linux-<arch>` ao lado do `rc` | o guest-runner injetado nas imagens |
| `RUNNER_SANDBOX` | ligado | `off` desliga Landlock + seccomp no Linux (só para diagnóstico; o `vmm` avisa) |
| `RC_MACHINE_NAME` | `default` | nome de máquina quando `--name` falta (fixe por projeto com direnv) |

## Catálogo de stacks

Fica em `internal/adapters/catalog/languages.yaml`, embutido no binário. Cada stack tem:

```yaml
- name: python               # nome que a CLI e a API recebem; precisa existir no executor do guest
  image: python:3.12-alpine  # imagem OCI oficial de origem
  cpus: 1                    # teto de vCPU por job (contra abuso)
  memory_mib: 512            # teto de RAM; a memória da libkrun é elástica
  warmup: python3 -m compileall -q -j0 /usr/local/lib/python3.12   # roda uma vez, no build, com rede
  gpu: false                 # true libera --gpu
  gpu_vram_mib: 0            # VRAM do guest com GPU (0 = 4096)
```

O build baixa a imagem OCI sem Docker (go-containerregistry), extrai numa VM de build Alpine, injeta o guest, roda o
warmup e gera um **ext4 read-only**. A chave de cache inclui o digest da imagem, o warmup, os scripts e o hash do
guest, então mudar qualquer um deles reconstrói. Campo desconhecido no YAML é erro de build.

## API HTTP

`rc serve` mantém um **pool** de VMs já bootadas por stack. Cada VM atende **um** job e morre, e a reposição é
assíncrona, com no máximo 4 boots simultâneos no total.

| Método | Caminho | O que faz |
|--------|---------|-----------|
| GET | `/health` | `{"status","version","pool":{"python":2,...}}` |
| GET | `/api/v1/languages` | stacks, imagem de origem e se está pronta |
| POST | `/api/v1/run` | `{language, files[], stdin}`; `stdin` em lista roda um job por item (lote) |
| POST | `/api/v1/execute` | `{language, code \| files[], stdin, timeout, gpu}` |
| POST | `/api/v1/execute/stream` | mesmo corpo, resposta em Server-Sent Events |
| — | `/api/v1/machines/...` | ciclo de vida das máquinas: create, start, stop, exec, exec/stream, files, logs ([detalhes](/api-reference/overview#máquinas)) |

Resposta de `/run` e `/execute`:

```json
{"status":"success","stdout":"1024\n","stderr":null,"exception":null,"executionTime":12}
```

- `status: "success"` diz que a API executou, **não** que o programa passou: olhe `stderr` e `exception`.
- `exception` é `null`, `"Timeout"`, `"Compilation failed"` ou a descrição de uma falha de infraestrutura.

Streaming (`/execute/stream`):

```
event: stdout
data: "a\n"

event: result
data: {"status":"success","exitCode":0,"exception":null,"executionTime":2034,"timedOut":false}
```

- O `data` de `stdout`/`stderr` é uma string JSON: a quebra de linha vem escapada.
- O `result` não repete a saída.
- Uma falha depois do início vira `event: error`.

### Códigos de erro

| Código | HTTP | Quando |
|--------|------|--------|
| `E001: operation timed out` | 200 | o guest não respondeu dentro do timeout mais a folga do host (2 s) |
| `E005: stdin too long` | 400 | stdin acima de 1.000.000 bytes |
| `E006: unsupported language X` | 400 | stack fora do catálogo ou sem imagem construída neste host |
| `E007: stack X has no GPU support` | 400 | `gpu` pedido numa stack sem `gpu: true` |
| outros limites (timeout, arquivos, tamanho, nome) | 400 | `error` com o motivo |
| falha de infraestrutura | 500 | `exception` com o motivo (VM não subiu, canal com o guest caiu) |

## Padrões importantes

- **Uma VM nova por job**: nada passa de um job para o outro; o pool só tira o boot do caminho.
- **Sem rede**: o vsock é criado sem TSI. Sem desligar o vsock implícito, a libkrun ligaria a rede sozinha.
- **Root read-only** (ext4 por virtio-blk, não virtiofs, que deixaria o guest alcançar o resto do host); `/tmp` é um
  tmpfs de 256 MiB.
- **Limites por job**: stdin ≤ 1 MB; até 32 arquivos; código ≤ 4 MiB no total; timeout de 10 s (máximo 300 s).
- **CPU e RAM por stack** são teto contra abuso; a memória é elástica, e uma VM parada ocupa cerca de 44 MB.
- **API sem autenticação**: escuta em `127.0.0.1` por padrão.

## Comportamentos importantes

- **Timeout = `SIGKILL` no grupo do processo da VM.** Não sobra processo; o e2e confere com `pgrep`.
- **Se o `rc` morre, as VMs morrem junto**: `Pdeathsig` no Linux, pipe de vigia no fd 3 no macOS.
- **Streaming entrega o que o programa escreve no pipe.** Python roda com `PYTHONUNBUFFERED=1`. C (`printf`), Ruby e
  Perl acumulam o stdout quando a saída não é terminal, e só entregam no `flush`.
- **`rc shell` com terminal** ganha console virtio TTY e põe o terminal do host em modo raw; com pipe, fica o console
  normal.
- **Imagem desatualizada**: se o guest da imagem for anterior ao streaming, o erro manda rodar `rc image build`.
- **Linux**: o processo `vmm` de cada VM de job roda sob Landlock + seccomp. Com Landlock ABI < 9 não há isolamento de
  socket Unix entre VMs, e o `rc doctor` avisa. VM com GPU fica fora do confinamento.
- **GPU**: VM com GPU sobe a frio, fora do pool, no máximo 2 ao mesmo tempo, e divide a GPU do host (isolamento por
  processo, não por VM).

## Comparação com o smolvm

O smolvm (smol-machines) usa a mesma base: libkrun + libkrunfw, imagens OCI, sem daemon. A diferença de propósito é
que o `rc` executa **jobs descartáveis** de código não confiável, e o smolvm gerencia **máquinas** de uso geral.

| Recurso do smolvm | No `rc` hoje | Observação |
|-------------------|--------------|------------|
| `machine run` efêmero | **sim**: `rc run` | por stack do catálogo, não por imagem arbitrária |
| `-it` shell interativo | **sim**: `rc shell` | TTY real, Ctrl+C, tamanho da janela |
| `exec --stream` / SSE | **sim**: `rc run` e `/api/v1/execute/stream` | evento final `result` (no smolvm, `exit`) |
| `--gpu`, `--gpu-vram` | **sim**: `--gpu`; VRAM pelo catálogo | Venus/Vulkan; testado no macOS |
| rede desligada por padrão | **sim** | e não há como ligar num job (por desenho) |
| HTTP API (`serve`) | **sim** | rotas de job, não de máquina; com pool pré-bootado, que o smolvm não tem |
| imagens OCI sem Docker | **sim** | pull no host, ext4 gerado numa VM de build |
| máquinas persistentes (`create`/`start`/`exec`/`stop`/`ls`/`delete`) | **sim**: `rc machine` | raiz gravável (overlay) e `/workspace` persistem; `/tmp` é tmpfs, como no smolvm |
| `machine cp`, `/workspace` persistente | **sim** | pedaços de 1 MiB, atômico, até 4 GiB; ~84 MB/s de subida e ~88 MB/s de descida |
| `machine logs`, `status` | **sim** | `logs` lê o console, legível com a máquina parada |
| `machine stats`, `update`, `monitor` | **não** | Fase D |
| `machine shell` (TTY na máquina) e WebSocket interativo | **não** | próximo passo da Fase A |
| erros com código estável | **sim** nas máquinas | `NOT_FOUND`, `CONFLICT`, `INVALID_STATE`, `CONFIG_ERROR` |
| `--secret-env` / `--secret-file` | **não** | Fase B |
| `--ssh-agent` | **não** | Fase B |
| `--net`, `--allow-host`, `--allow-cidr`, `--block-private` | **não** | Fase C: o TSI não filtra destino, precisa de virtio-net com filtro nosso |
| Smolfile (TOML) | **não** | Fase D; o equivalente hoje é a entrada do catálogo |
| `--image ./img.tar`, `--image -` | **não** | Fase D |
| `--error-format json` | parcial | a API já devolve códigos E00x estáveis |
| `pack` (`.smolmachine`) | **não** | fora do plano por ora |
| `checkpoint` Mac → Linux | **não** | a libkrun upstream não tem snapshot |
| `--cuda` | **não** | exige host Linux com NVIDIA |
| Windows (WHP) | **não** | a libkrun upstream não tem WHP; o smolvm usa um fork |
| — | **sim, só no `rc`** | Landlock + seccomp por VM no Linux; pool pré-bootado; catálogo com warmup gravado na imagem; gate de cobertura de 90% |

## Plano de evolução

Cada fase fecha como as anteriores: story AIOX, e2e duas vezes seguidas, defeito plantado de propósito para provar que
o teste pega, e gate de 90%. A **Fase A está feita** (story 7.1), menos o `machine shell` com TTY; as outras ainda não.

| Fase | O que entra | Decisão de desenho | Risco conhecido |
|------|-------------|--------------------|-----------------|
| **A — máquinas** ✅ | `rc machine create/start/stop/delete/ls/status/exec/logs/cp`; rotas `/api/v1/machines/...` (falta `shell` com TTY) | estado em `<cache>/machines/<nome>/`; disco ext4 gravável por máquina; overlayfs no guest (imagem por baixo, disco por cima) para o que for instalado persistir; `/workspace` no disco; o `vmm` roda destacado, com registro de PID e teste de vida | hoje o `vmm` morre com o pai de propósito, e os jobs efêmeros precisam continuar assim |
| **B — segredos e SSH** | `--secret-env`, `--secret-file`, `--ssh-agent` | segredo resolvido na hora do start, nunca gravado no registro; recusado pela API (seria leitura arbitrária do host); SSH por uma porta vsock ligada ao `SSH_AUTH_SOCK`, sem a chave entrar na VM | root no guest lê o env do processo; é defesa em camadas, não sigilo absoluto |
| **C — rede com filtro** | `--net`, `--allow-host`, `--allow-cidr`, `--block-private`, `-p` | virtio-net com pilha de rede em userspace no host (ex.: gvproxy) e o filtro feito por nós; o TSI segue só para a VM de build | é a parte mais cara e mais sensível; medir antes de prometer |
| **D — conveniências** | Smolfile, `--image ./img.tar` e `-`, `machine stats`, `--error-format json` | o go-containerregistry já lê `docker save`/OCI layout | — |

Ordem sugerida: A → B → D → C, com C por último porque abrir rede é o que mais mexe na garantia central do projeto.

## Onde aprofundar

- [Arquitetura](/architecture), [Segurança](/advanced/security), [GPU](/advanced/gpu)
- [Relatório de testes](/advanced/test-report): o que foi medido em cada plataforma
- `docs/prd.md`, `docs/architecture.md`, `docs/stories/`: requisitos, decisões e cada story com o log dos defeitos
