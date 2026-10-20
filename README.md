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

macOS 27.0.1, Apple Silicon, 19/10/2026. Testes por plataforma (macOS de ponta a ponta, Linux com Landlock + seccomp, Windows) e o que não foi testado: [relatório de testes](docs/docusaurus/docs/advanced/test-report.md).

| Medida | Valor |
|---|---|
| boot até a 1ª resposta de um job Python (30 boots) | p50 **260 ms**, p95 **266 ms** |
| job Python pela CLI, boot frio | ~240 ms no total |
| job pela API com pool pré-bootado | **26–43 ms** de requisição HTTP |
| VM parada no pool | 44 MB de footprint |
| `image build --stack python` | 8–10 s |
| Linux (droplet DigitalOcean, 4 vCPU, KVM aninhado): job Python pela CLI, boot frio | ~1,4 s no total |
| Kubernetes (mesmo droplet): pod `runtimeClassName: rc` até `Running` | ~2 s |

## Como funciona

```
cliente ──HTTP──▶ rc serve ──▶ pool (N VMs prontas por linguagem, cada uma usada uma vez)
                                                │ spawn
                                                ▼
                                   rc vmm  ── libkrun ── libkrunfw (Linux 6.12)
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

Para testar à mão, passo a passo: [docs/runbook.md](docs/runbook.md) (`task runbook` executa e confere a mesma sequência).

Referência para agentes (cada comando, flag, rota e erro, o que ainda não existe e o plano de evolução):
[docs/docusaurus/docs/agents.md](docs/docusaurus/docs/agents.md). GPU de ponta a ponta:
[docs/docusaurus/docs/advanced/gpu.md](docs/docusaurus/docs/advanced/gpu.md).

## Instalação

Tudo pelo [Nix](https://nixos.org), sem Homebrew nem Docker. O `flake.nix` do repo expõe `.#krun` (libkrun +
libkrunfw), e o `rc` procura a libkrun em `~/.nix-profile/lib` por padrão.

```bash
task krun:install                 # nix profile add .#krun
task build                        # compila host + guest; no macOS assina com o entitlement do hypervisor
task image:build -- --stack python
```

- **macOS (Apple Silicon):** libkrun 1.19.6 e libkrunfw 5.6.2 de derivações próprias em `nix/` (o nixpkgs só
  empacota a libkrun para Linux). A 1ª instalação compila a libkrun, ~10 min.
- **Linux (KVM):** libkrun e libkrunfw do nixpkgs, com virtio-blk ligado. O usuário precisa de leitura e escrita em
  `/dev/kvm`.

`./bin/rc doctor` confere o host.

## Uso

```bash
./bin/rc run --stack python --code 'print(2**10)'
./bin/rc serve --listen 127.0.0.1:8080 --pool-size 2

curl -s -X POST http://127.0.0.1:8080/api/v1/run -H 'content-type: application/json' \
  -d '{"language":"python","stdin":"oi","files":[{"name":"main.py","content":"print(input().upper())"}]}'
# {"status":"success","stdout":"OI\n","stderr":null,"exception":null,"executionTime":35}
```

`status: "success"` quer dizer que a API executou; o programa pode ter falhado (`stderr`, `exception`). Timeout
aparece como `exception: "Timeout"`.

A API **não tem autenticação**. Por isso o padrão é escutar só em `127.0.0.1`.

### Saída em streaming

O `rc run` mostra o stdout e o stderr enquanto o programa roda. Pela API, `POST /api/v1/execute/stream` (mesmo corpo
do `/execute`) responde em Server-Sent Events: eventos `stdout`/`stderr` com o pedaço como string JSON e um `result`
no fim.

```bash
curl -N -X POST http://127.0.0.1:8080/api/v1/execute/stream -H 'content-type: application/json' \
  -d '{"language":"python","code":"import time\nprint(\"early\")\ntime.sleep(1.5)\nprint(\"late\")"}'
```

C, Ruby e Perl acumulam o stdout quando a saída é pipe: o programa precisa dar flush para o pedaço sair antes do fim.

### GPU (Vulkan) para IA

`--gpu` dá à VM a GPU do host por virtio-gpu/Venus (Vulkan → MoltenVK → Metal no macOS).
Vale em stack com `gpu: true` no catálogo; a stack `llama` traz o llama.cpp com backend Vulkan e um modelo
Qwen2.5 0.5B.

```bash
./bin/rc image build --stack llama          # ~6 min na 1ª vez: Mesa com patch de 16 KiB + llama.cpp + modelo
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'
# deviceName = Virtio-GPU Venus (Apple M1 Pro)
```

Medido no M1 Pro: prompt 893–1.365 t/s na GPU contra 193–264 na CPU. CUDA não é suportado (exige host Linux com NVIDIA), e
GPU no Linux não foi testada. Detalhes em [GPU](docs/docusaurus/docs/advanced/gpu.md).

### Kubernetes (k3s e microk8s)

Com o runtime `rc` no containerd, cada pod com `runtimeClassName: rc` vira uma microVM com kernel próprio e o IP que o
CNI deu ao pod: `kubectl exec`, logs, Service e DNS do cluster funcionam como num pod comum.

```bash
sudo deploy/k3s/install.sh            # ou deploy/microk8s/install.sh (nó Linux com /dev/kvm)
kubectl apply -f deploy/k8s/example-pod.yaml && kubectl logs rc-hello   # kernel: Linux 6.12.91
```

Provado com `deploy/k8s/e2e.sh` (19 checagens) no k3s 1.36 e no microk8s 1.35. Limites (port-forward, hostNetwork,
volumes copiados na criação) em [Kubernetes](docs/docusaurus/docs/advanced/kubernetes.md).

### Shell dentro da VM

```bash
./bin/rc shell --stack python     # ou: task shell -- --stack python
```

Abre um shell interativo numa VM nova da stack, com a mesma imagem dos jobs: root read-only, sem rede e `/tmp`
gravável. Num terminal, o guest recebe um TTY de verdade: Ctrl+C interrompe só o comando e o tamanho da janela
chega ao guest. A VM some no `exit`.

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
- **Confinamento no macOS:** não existe Landlock nem seccomp; o processo `vmm` fica só com o entitlement e o grupo de
  processo. No Linux o processo `vmm` de cada VM de job roda com Landlock + seccomp (veja a doc de segurança).
- **Socket de outra VM em kernel com Landlock ABI < 9:** o controle de `connect` em socket Unix por caminho só existe
  a partir da ABI 9. O `rc doctor` avisa quando o kernel não tem.

## Licença

[MPL-2.0](LICENSE)
