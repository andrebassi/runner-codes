# Runner Codes

**Execução isolada de código gerado por LLM em microVMs libkrun**

[![Website](https://img.shields.io/badge/website-runner.codes-orange)](https://runner.codes)
[![Documentation](https://img.shields.io/badge/docs-docs.runner.codes-blue)](https://docs.runner.codes)
[![Release](https://img.shields.io/github/v/release/andrebassi/runner-codes)](https://github.com/andrebassi/runner-codes/releases)
[![License](https://img.shields.io/badge/license-MPL--2.0-blue)](LICENSE)

Cada job roda numa **microVM nova**, com kernel próprio, sem rede, com root read-only, e é destruída no fim.
O VMM é a [libkrun](https://github.com/containers/libkrun) e o kernel vem da
[libkrunfw](https://github.com/containers/libkrunfw). Roda nativo no **macOS Apple Silicon** (Hypervisor.framework)
e no **Linux x86_64/aarch64** (KVM). As linguagens saem de **imagens OCI oficiais**, sem Docker.

> Antes desta versão o runner usava Firecracker numa EC2 bare metal. A troca pela libkrun trouxe o macOS como alvo,
> tirou a EC2 e o S3, e a API passou a executar de verdade (antes `/execute` era stub).

## Números medidos

macOS 27.0.1, Apple Silicon, 19/10/2026. O Linux ainda não foi medido.

| Medida | Valor |
|---|---|
| boot até a 1ª resposta de um job Python (30 boots) | p50 **260 ms**, p95 **266 ms** |
| job Python pela CLI, boot frio | ~240 ms no total |
| job pela API com pool pré-bootado | **26–43 ms** de requisição HTTP |
| VM parada no pool | 44 MB de footprint |
| `image build --lang python` | 8–10 s |

## Como funciona

```
cliente ──HTTP──▶ runner.code serve ──▶ pool (N VMs prontas por linguagem, cada uma usada uma vez)
                                                │ spawn
                                                ▼
                                   runner.code vmm  ── libkrun ── libkrunfw (Linux 6.12)
                                                │ socket Unix ⇄ vsock 5000
                                                ▼
                                   runner-guest (root ext4 read-only, /tmp em tmpfs)
```

- **Um processo por VM:** `krun_start_enter` assume o processo. Timeout é `SIGKILL` no grupo. Se o serviço morre,
  as VMs morrem junto (`Pdeathsig` no Linux, pipe de vigia no macOS).
- **Sem rede:** o vsock é criado sem TSI. Sem desligar o vsock implícito, a libkrun ligaria a rede sozinha.
- **Root em disco, não virtiofs:** virtiofs não impede o guest de alcançar o resto do host.
- **Imagens:** `image build` baixa a imagem OCI oficial (go-containerregistry), e uma VM de build Alpine extrai,
  aquece (`.pyc`, cache de build do Go, pacotes apk) e gera o ext4 com `mkfs.ext4`. O cache é por digest + warmup + guest.

Detalhes em [docs/architecture.md](docs/architecture.md); requisitos em [docs/prd.md](docs/prd.md).

## Instalação

### macOS (Apple Silicon)

```bash
brew tap slp/krun && brew trust slp/krun
brew install slp/krun/libkrunfw slp/krun/libkrun
task build                        # compila host + guest e assina com o entitlement do hypervisor
task image:build -- --lang python
```

### Linux (KVM)

```bash
nix profile install nixpkgs#libkrun nixpkgs#libkrunfw
task build
task image:build -- --lang python
```

O usuário precisa de leitura e escrita em `/dev/kvm`. `./bin/runner.code doctor` confere o host.

## Uso

```bash
./bin/runner.code run --lang python --code 'print(2**10)'
./bin/runner.code serve --listen 127.0.0.1:8080 --pool-size 2

curl -s -X POST http://127.0.0.1:8080/api/v1/run -H 'content-type: application/json' \
  -d '{"language":"python","stdin":"oi","files":[{"name":"main.py","content":"print(input().upper())"}]}'
# {"status":"success","stdout":"OI\n","stderr":null,"exception":null,"executionTime":35}
```

`status: "success"` quer dizer que a API executou; o programa pode ter falhado (`stderr`, `exception`). Timeout
aparece como `exception: "Timeout"`.

A API **não tem autenticação**. Por isso o padrão é escutar só em `127.0.0.1`.

## Linguagens

O catálogo fica em [`internal/adapters/catalog/languages.yaml`](internal/adapters/catalog/languages.yaml):
python, nodejs, go, rust, java, c, cpp, ruby, php, perl, bash, sh, awk, jq, sqlite.

Para acrescentar uma linguagem:
1. ela precisa existir no executor do guest ([`internal/guest/executor.go`](internal/guest/executor.go));
2. ganha uma entrada no YAML, com imagem, teto de CPU/RAM e warmup.

Um teste confere que toda linguagem do catálogo existe no executor.

## Desenvolvimento

| Task | O que faz |
|---|---|
| `task build` | host + guest em `bin/`, assinado no macOS |
| `task test` | unidade com `-race` |
| `task test:cov` | gate: 90% total, 100% domínio (reprova abaixo) |
| `task test:targets` | darwin/arm64 e linux/{amd64,arm64} compilam; windows recusa com o motivo |
| `task test:e2e` | libkrun real: saída, timeout, rede bloqueada, root ro, isolamento, teto de RAM, pool |
| `task gates` | os três acima |

## Fora do escopo

- **Windows:** a libkrun upstream não tem backend WHP. O build para Windows falha de propósito.
- **Landlock e seccomp no processo `vmm` (Linux):** estão planejados e ainda não implementados. No macOS esses
  mecanismos não existem.

## Licença

[MPL-2.0](LICENSE)
