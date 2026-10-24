# AGENTS.md: rc (runner.codes)

Referência para agentes de IA que **usam** o `rc` ou **trabalham neste repositório**. O que está aqui existe hoje; o que
ainda não existe está em [O que ainda não existe](#o-que-ainda-não-existe). A referência completa, nos 4 idiomas, é a
página [Referência para agentes](https://docs.runner.codes/agents) dos docs.

O `rc` roda código não confiável (gerado por LLM, por exemplo) numa **microVM com kernel próprio**: a libkrun é o VMM e
a libkrunfw dá o kernel. Há dois modos:
- **job**: VM nova, sem rede, root só leitura, destruída no fim (boot até a 1ª resposta em ~260 ms no Apple Silicon);
- **máquina**: VM persistente, com disco gravável, rede opcional com filtro por host, volumes, segredos e portas.

## Plataformas

| Host | Guest | Hipervisor | Situação |
|------|-------|-----------|----------|
| macOS Apple Silicon | Linux arm64 | Hypervisor.framework | testado de ponta a ponta |
| Linux x86_64 / aarch64 | Linux da mesma arquitetura | KVM (`/dev/kvm`) | x86_64 medido num droplet com KVM; aarch64 só compila |
| Windows | — | — | fora: a libkrun upstream não tem WHP, e o build para Windows falha de propósito |

## Onde está cada coisa

| Para | Onde |
|------|------|
| uso, CLI, API, segurança, GPU, Kubernetes (4 idiomas) | [docs.runner.codes](https://docs.runner.codes) (fonte em `docs/docusaurus`) |
| teste à mão, passo a passo, com o esperado de cada passo | [`docs/runbook.md`](docs/runbook.md); `task runbook` executa e confere |
| requisitos, decisões e cada story com o log de defeitos | `docs/prd.md`, `docs/architecture.md`, `docs/stories/` |
| runtime de Kubernetes (k3s e microk8s), relato técnico | [`deploy/README.md`](deploy/README.md) |
| guias que provam cada fluxo numa VM real | [`examples/guides/`](examples/guides/) (`task guides`) |

## Referência rápida

```bash
# job: VM nova, sai com o código do programa, a VM some no fim
./bin/rc run --stack python --code 'print(2**10)'
./bin/rc run --stack go --file main.go --stdin 'entrada' --timeout 30s

# shell interativo numa VM nova da stack (TTY de verdade)
./bin/rc shell --stack bash

# máquina: o que se grava fica entre comandos e entre stop/start
./bin/rc machine create --name dev --stack python --net
./bin/rc machine start  --name dev
./bin/rc machine cp ./app.py dev:/workspace/app.py
./bin/rc machine exec   --name dev -- python3 /workspace/app.py
./bin/rc machine stop   --name dev

# API HTTP com pool de VMs prontas
./bin/rc serve --listen 127.0.0.1:8080 --pool-size 2
```

## Quando usar o quê

| Objetivo | Comando |
|----------|---------|
| rodar um trecho isolado e pegar a saída | `rc run --stack S --code '...'` |
| ambiente de várias etapas que guarda estado | `rc machine create` → `start` → `exec`/`cp` → `stop` |
| criar, ligar, usar e apagar numa tacada | `rc machine run [-d] ... -- CMD` (sem `-d`, a máquina some no fim) |
| muitos jobs com latência baixa | `rc serve`: o pool tira o boot do caminho (26–78 ms por job) |
| um terminal da máquina no navegador | `rc machine web-terminal --name N [-- CMD]` (127.0.0.1, token na URL) |
| levar uma imagem ou máquina para outro host | `rc pack create` (artefato executável) |
| GPU do host (Vulkan) | `--gpu`, só em stack com `gpu: true` (a `llama`) |
| conferir se o host está pronto | `rc doctor` |
| cada pod de um cluster numa microVM | `runtimeClassName: rc` depois de `deploy/k3s/install.sh` ou `deploy/microk8s/install.sh` |

### Job ou máquina

| | job (`rc run`, API `/execute`) | máquina (`rc machine`) |
|---|---|---|
| disco | ext4 da stack, só leitura; `/tmp` em tmpfs de 256 MiB | a imagem por baixo, o disco da máquina por cima (overlay): o que se instala persiste |
| rede | nenhuma | desligada por padrão; `--net` ou filtro por host |
| vida | um job e acabou | até `stop`/`delete` |

## Estrutura da CLI

```
rc run    --stack S (--code C | --file F) [--stdin T] [--timeout 10s] [--gpu]
rc shell  --stack S [--gpu]
rc machine create|run [--name N] (--stack S | --image REF | -f runner.code) [flags] [-- CMD ...]
rc machine start|stop|status|delete|pause|resume [--name N]
rc machine exec [--name N] [-e K=V] [-w DIR] [-u USER] [-i] [--timeout D] -- CMD ARGS...
rc machine shell|web-terminal [--name N] [-- CMD]
rc machine cp SRC DST                 # um dos lados é nome:/caminho
rc machine logs [--name N] [--tail N] [--source workload]
rc machine ls | branch | sync | update | data-dir | prune
rc pack create|inspect|prune
rc image build (--stack S ... | --all)
rc serve [--listen 127.0.0.1:8080] [--pool-size 2]
rc languages | doctor
```

Sem `--name`, a máquina é `default` (ou `RC_MACHINE_NAME`).

## Imagem da máquina

| Forma | Exemplo |
|-------|---------|
| stack do catálogo | `--stack python` |
| qualquer imagem OCI de registry público | `--image node:22` |
| imagem no disco: tar do `docker save` ou diretório OCI layout | `--image ./out/img` (começa com `/`, `./` ou `../`) |

A imagem é convertida em ext4 no primeiro uso e fica em cache; o rc confere a arquitetura antes. Registry com
autenticação não é suportado: para imagem privada, grave em OCI layout e use o caminho local.

## Flags de máquina mais usadas

| Flag | O que faz |
|------|-----------|
| `--cpus`, `--memory`, `--disk-gib` | teto de vCPU (padrão 4), de RAM em MiB (padrão 8192, elástica) e disco gravável (padrão 10 GiB, esparso) |
| `--net` | saída para a internet pelo gateway do host |
| `--allow-host H`, `--allow-cidr C`, `--block-private` | só os hosts/faixas permitidos; `--block-private` fecha a rede local e o loopback do host |
| `-p HOST:GUEST` | publica uma porta do guest no `127.0.0.1` do host (o guest disca o próprio loopback: servidor que escuta só em localhost responde) |
| `-v HOST:GUEST[:ro]` | diretório do host montado a cada start |
| `--init CMD` | roda uma vez, no primeiro start (instalar pacotes) |
| `-e K=V`, `--secret-env G=H`, `--secret-file G=/arq` | ambiente; os segredos são lidos no start e nunca gravados |
| `--credential NOME=VAR@HOST` | a máquina usa a credencial nesse host sem ler o valor |
| `--ssh-agent` | agente SSH do host dentro da máquina; as chaves não entram na VM |
| `-u USER`, `-w DIR` | quem roda e onde (o `init` roda como root) |
| `--gpu`, `--display WxH` | GPU do host por Vulkan; tela servida no navegador, com teclado e mouse |
| `--expose-socket`, `--mount-socket`, `--docker-socket` | pontes de socket Unix entre host e guest |

## runner.code

Uma máquina declarada num arquivo TOML versionado, em vez de uma fila de flags (`rc machine create -f runner.code`).
Flag na linha de comando vence o arquivo; chave desconhecida ou ainda não suportada é erro, com o motivo.

```toml
image = "node:22"
cpus = 2
memory = 2048
net = true
env = ["IS_SANDBOX=1"]
init = ["npm install -g @anthropic-ai/claude-code"]

[secrets]
ANTHROPIC_API_KEY = { from_env = "ANTHROPIC_API_KEY" }

[dev]
ports = ["13000:3000"]
volumes = [".:/workspace"]
```

Cuidado do TOML: chave escrita depois de uma tabela (`[secrets]`, `[dev]`) pertence a essa tabela. Formato completo:
[runner.code](https://docs.runner.codes/cli/runner-code).

## API HTTP

| Método | Caminho | O que faz |
|--------|---------|-----------|
| GET | `/health` | versão e VMs prontas por stack |
| POST | `/api/v1/execute` | `{language, code \| files[], stdin, timeout, gpu}` |
| POST | `/api/v1/execute/stream` | o mesmo, com a saída em Server-Sent Events |
| — | `/api/v1/machines/...` | ciclo de vida das máquinas (create, start, stop, exec, files, logs) |

`status: "success"` diz que a API executou, **não** que o programa passou: olhe `stderr` e `exception`. Timeout vem
como `exception: "Timeout"`. Erros com código estável (`E005` stdin longo, `E006` stack desconhecida, `E007` sem GPU).
A API **não tem autenticação**: escuta em `127.0.0.1` por padrão.

SDKs (só biblioteca padrão, mesmos métodos; receitas em https://docs.runner.codes/api-reference/sdks):

```bash
go get runner.codes/pkg/client@latest
pip install "git+https://github.com/andrebassi/runner-codes@v1.1.1#subdirectory=sdk/python"   # import runner_code
```

## Padrões e comportamentos que importam

- **Timeout é `SIGKILL` no grupo do processo da VM**; não sobra processo. Se o `rc` morre, as VMs morrem junto.
- **Limites por job**: stdin ≤ 1 MB, até 32 arquivos, código ≤ 4 MiB, timeout de 10 s (máximo 300 s).
- **Streaming entrega o que o programa escreve no pipe**: C, Ruby e Perl acumulam o stdout fora de terminal.
- **Linux**: o processo `vmm` de cada VM de job roda sob Landlock + seccomp. VM com GPU fica fora do confinamento.
- **A máquina dá boot com o `/bin/sh` da imagem, sem `PATH` no ambiente**: imagem cujo `sh` não tenha `/bin` no PATH
  padrão (o bash do nixpkgs usa `/no-such-path`) não sobe: o `mount` do boot não é achado. Com o `sh` do busybox, sobe.
- **O Entrypoint da config OCI não é lido**: declare `entrypoint`/`cmd` no `runner.code` ou o comando depois de `--`.

## O que ainda não existe

| Recurso | Situação |
|---------|----------|
| `machine stats`, `machine monitor` | não existe |
| exec interativo por WebSocket na API | não existe; o `rc machine web-terminal` serve um terminal no navegador |
| snapshot de RAM e CPU | não existe (a libkrun upstream não tem); o `branch` copia o disco |
| CUDA | não existe (exige host Linux com NVIDIA); `rc doctor`, CLI e API dizem não, com o motivo |
| registry com autenticação | não existe; use imagem local |
| Windows | fora |

## Trabalhar neste repositório

- **Módulo Go `runner.codes`**, hexagonal: `internal/domain` (regras puras) ← `internal/app` → `internal/ports` ←
  adapters (`krun`, `guestproto`, `oci`, `catalog`, `httpapi`...). `cmd/rc/main.go` só liga as peças.
- **Dois binários, nunca juntar**: `cmd/rc` (host, link dinâmico por causa do purego) e `cmd/runner-guest` (estático,
  roda dentro da VM). O `scripts/build.sh` reprova se o guest sair dinâmico.
- **Ferramentas pelo Nix**, sem Homebrew nem Docker: `task krun:install`, `nix develop`.

```bash
task build              # bin/rc (assinado no macOS) + bin/rc-guest-linux-<arch>
task test               # unidade, -race
task test:cov           # gate: 90% total, 100% em internal/domain (reprova abaixo)
task test:targets       # darwin/arm64 e linux/{amd64,arm64} compilam; windows recusa
task test:e2e           # libkrun real (precisa de task build e das imagens)
task runbook            # docs/runbook.md de ponta a ponta, conferindo cada passo
task guides             # examples/guides, cada pasta numa VM real
```

Regras de entrega:
- **cobertura**: código novo nasce com teste; o gate de 90% reprova abaixo.
- **runbook**: recurso novo ganha seção no `docs/runbook.md` (e nas versões en, es e zh) e o passo no
  `scripts/runbook.sh`, na mesma entrega.
- **landing e docs**: atualizados junto, nos 4 idiomas (pt-BR, en, es, zh).
- **comentários e textos** em português; identificadores em inglês.
- **commits** assinados.
- **nada derivado de outro projeto** entra no repositório: referência é só referência, e o texto se escreve do zero.
