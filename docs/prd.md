# runner.codes — PRD: execução em microVM libkrun

## Goals and Background Context

### Goals

- Executar código gerado por LLM numa microVM **nova por job**, com kernel próprio, sem rede e sem acesso ao host.
- Trocar Firecracker por **libkrun + libkrunfw**, rodando no **macOS arm64 (HVF)** e no **Linux x86_64/aarch64 (KVM)**.
- Usar **imagens OCI oficiais** como base de cada linguagem, sem daemon Docker e sem root para gerar a imagem.
- Tirar o boot do caminho do pedido com **pool pré-bootado** e **warmup gravado na imagem**.
- Fazer `/api/v1/run` e `/api/v1/execute` executarem de verdade (hoje são stub).
- Entregar com gate de cobertura de 90% que reprova, e arquitetura hexagonal.

### Background Context

Hoje o runner.codes depende de Firecracker: exige uma EC2 bare metal (`m5zn.metal`, 79 tasks `aws:*`), rootfs
ext4 montados com `debootstrap` e root, snapshots guardados no S3 e um handshake vsock próprio. Medido em
19/10/2026:
- `/api/v1/execute` devolve um texto fixo de "execução pendente";
- `pkg/guest` não compila no macOS;
- `pkg/host` tem 21,2% de cobertura com um teste falhando, e `internal/*` tem 0%.

A libkrun é o VMM usado pelo krunkit. Ela roda nativa no Mac (Hypervisor.framework) e no Linux
(KVM), carrega o kernel a partir da libkrunfw e aceita um disco ext4 como root. O dono escolheu usá-la
**direto**, sem camada intermediária, para manter o guest-runner e o protocolo vsock que já existem. Com isso, warmup,
conversão de OCI e sandbox do VMM passam a ser responsabilidade deste projeto. O Windows fica fora, porque a
libkrun upstream não tem backend WHP.

### Change Log

| Date | Version | Description | Author |
|---|---|---|---|
| 2026-10-19 | 0.1 | PRD inicial da migração Firecracker → libkrun | @pm |

## Requirements

### Functional

- **FR1:** `POST /api/v1/run` recebe `{language, files[], stdin}` e devolve `{status, stdout, stderr, exception, executionTime}` com o resultado real da execução. O contrato `RunRequest`/`RunResponse` atual é mantido.
- **FR2:** `POST /api/v1/execute` recebe `{trace_id, lang, code, timeout}` (formato interno) e devolve `{trace_id, stdout, stderr, exit_code}`.
- **FR3:** `GET /api/v1/languages` lista as linguagens com imagem construída neste host.
- **FR4:** Cada job roda numa microVM que nunca executou outro job. Ao fim do job, a VM é destruída.
- **FR5:** `rc image build --stack <l>` gera, a partir da imagem OCI oficial da linguagem, um ext4 read-only com o guest-runner dentro e o warmup aplicado. A geração não exige root nem Docker.
- **FR6:** Um pool por linguagem mantém N VMs já bootadas (N configurável, padrão 2) e repõe cada VM consumida em background.
- **FR7:** Com o pool vazio, o job faz boot frio na hora; ele não é recusado.
- **FR8:** Um job que passa do timeout é encerrado matando a VM, e a resposta indica timeout.
- **FR9:** `rc run --stack <l> --code <c>` executa um job pela CLI, sem o servidor HTTP.
- **FR10:** `rc doctor` diz se o host pode rodar VMs (HVF/KVM, libkrun, libkrunfw, imagens construídas), com um motivo por item reprovado.

### Non Functional

- **NFR1:** O guest não tem rede. O vsock é criado sem TSI.
- **NFR2:** O guest não enxerga diretório do host. O root é disco ext4 read-only, e a escrita vai a um disco próprio da VM.
- **NFR3:** No Linux, o processo do VMM roda com uid sem privilégio, Landlock e seccomp. No macOS, roda com o entitlement do hypervisor; o macOS não oferece Landlock nem seccomp, e isso fica declarado na doc de segurança.
- **NFR4:** Nenhuma VM sobrevive ao serviço: no Linux por `Pdeathsig`, no macOS por pipe de vigia.
- **NFR5:** CPU e RAM por linguagem são **teto contra abuso**, declarados no catálogo.
- **NFR6:** A latência do job com pool cheio é dominada pela execução. O boot frio p95 é medido no spike e registrado em `docs/architecture.md`.
- **NFR7:** Cobertura ≥ 90% de linha no total, domain 100% e adapters > 90%, com `task test:cov` reprovando abaixo do piso.
- **NFR8:** O código é Go (host e guest). Identificadores em inglês, comentários em pt-BR.
- **NFR9:** O build para `GOOS=windows` falha com mensagem explícita, não em silêncio.

## Technical Assumptions

### Repository Structure

Monorepo. Um módulo Go (`runner.code`), um binário (`runner.code`) com subcomandos de host e de guest, e
o site de docs em `docs/docusaurus`.

### Service Architecture

Monólito com arquitetura hexagonal. Um processo de serviço (HTTP + pool), e cada VM num processo filho
`rc vmm`, criado por spawn: `krun_start_enter` assume o processo, e fork de processo multi-thread
é morto pelo HVF.

### Testing Requirements

- Testes de unidade em `internal/domain` (100%) e nos adapters (> 90%), com fakes atrás das ports.
- Testes ponta a ponta (build tag `e2e`) contra libkrun real no Mac arm64, rodados duas vezes seguidas.
- No Linux, o mesmo e2e numa VM com KVM, quando houver uma disponível.
- Prova de falha do gate: `COVERAGE_FLOOR=101 task test:cov` → rc≠0.

### Additional Technical Assumptions

- libkrun 1.19.6 e libkrunfw 5.6.2 no macOS, por derivação Nix própria (`nix/`; até 19/10/2026 vinham do brew `slp/krun`). No Linux, nixpkgs (libkrun 1.19.5 com virtio-blk, libkrunfw 5.5.0). Sem Homebrew.
- Binding por `purego` (sem cgo).
- OCI por `go-containerregistry`; ext4 por `mkfs.ext4 -d` (e2fsprogs).
- O guest-runner atual (`pkg/guest`) é mantido, com o protocolo vsock de 4 bytes BE + JSON.

## Epic List

1. **Epic 1, fundação libkrun:** medir a libkrun nesta máquina e subir uma VM nossa com o guest-runner respondendo por vsock.
2. **Epic 2, imagens e warmup:** gerar imagens ext4 a partir de OCI, com warmup gravado, para o catálogo de linguagens.
3. **Epic 3, execução e API:** executar jobs de verdade com pool pré-bootado, pela API HTTP e pela CLI.
4. **Epic 4, limpeza e entrega:** remover o Firecracker, fechar os gates e reescrever docs e release.

## Epic Details

### Epic 1: fundação libkrun

Goal: provar, com números medidos, que a libkrun sobe uma VM com o nosso guest e sem rede, e deixar o
binding e o subcomando `vmm` prontos para o resto.

#### Story 1.1 — Spike de medição da libkrun

As a mantenedor, I want números medidos de boot, RSS, kill e rede da libkrun nesta máquina, so that as decisões de pool e de root se apoiem em dado, não em estimativa.

Acceptance Criteria
1. Boot frio p50/p95 em 100 boots, do spawn até o guest aceitar conexão vsock, registrado em `docs/architecture.md`.
2. RSS de uma VM parada, registrado.
3. Um `SIGKILL` no processo da VM não deixa processo vivo.
4. `wget` de dentro do guest falha com vsock sem TSI.
5. Comparação entre root por disco e root por virtiofs (tempo e exposição), registrada.

#### Story 1.2 — Binding purego e subcomando `vmm`

As a serviço, I want lançar uma VM por spawn de `rc vmm --spec`, so that cada VM seja um processo descartável.

Acceptance Criteria
1. `internal/adapters/krun` carrega libkrun por purego e expõe as chamadas usadas.
2. A spec (JSON) define cpus, memória, disco root, disco de escrita, socket vsock e exec; a montagem da config é testada sem libkrun (fake).
3. Rede desligada: `krun_disable_implicit_vsock` + `krun_add_vsock(ctx, 0)`.
4. No Linux, `Pdeathsig`; no macOS, pipe de vigia. Matar o pai mata a VM.
5. No macOS, o binário é assinado com o entitlement do hypervisor via `task sign`.

#### Story 1.3 — Guest-runner sob libkrun

As a host, I want o guest-runner atual rodando como o processo de exec da libkrun, so that o protocolo vsock existente funcione sem o handshake do Firecracker.

Acceptance Criteria
1. O guest monta o disco de escrita em `/workspace` e um tmpfs em `/tmp`.
2. O guest escuta vsock 5000, e o host conecta pelo socket Unix mapeado (`listen=true`).
3. Um job Python simples devolve stdout e exit code corretos pela VM real.

### Epic 2: imagens e warmup

Goal: transformar a imagem OCI oficial de cada linguagem num ext4 read-only pronto para boot, já aquecido.

#### Story 2.1 — `image build` OCI → ext4

As a operador, I want `rc image build --stack <l>`, so that a imagem da linguagem seja gerada sem Docker e sem root.

Acceptance Criteria
1. Pull e achatamento das camadas com go-containerregistry, com whiteouts respeitados.
2. Guest-runner injetado em `/opt/runner/guest` e `mkfs.ext4 -d` gerando o disco.
3. Cache pelo digest: um segundo build do mesmo digest não refaz nada.
4. Template do disco de escrita gerado uma vez e clonado por VM.

#### Story 2.2 — Catálogo e warmup na imagem

As a usuário da API, I want as linguagens começarem quentes, so that a primeira execução não pague JIT nem cache frio.

Acceptance Criteria
1. `languages.yaml` embutido, com imagem, cpus, memória e comandos de warmup por linguagem.
2. O warmup roda dentro de uma VM no build e o resultado é gravado no ext4 (CDS da JVM, `.pyc`, cache de build de Go e Rust).
3. Medição antes/depois do warmup para Python e Java, registrada.

### Epic 3: execução e API

Goal: executar jobs de verdade, com a VM nova por job e o boot fora do caminho do pedido.

#### Story 3.1 — Domínio, ExecuteService e pool

As a serviço, I want um pool por linguagem e um serviço de execução, so that cada job pegue uma VM pronta e a destrua no fim.

Acceptance Criteria
1. Validação de job no domínio (linguagem conhecida, stdin ≤ 1 MB, arquivos com nome seguro).
2. O pool mantém N VMs prontas, entrega cada uma uma única vez e repõe em background.
3. Pool vazio → boot frio; timeout → kill e resposta de timeout.
4. Testes com fakes cobrindo os ramos de erro.

#### Story 3.2 — API HTTP e CLI

As a cliente, I want `/api/v1/run`, `/execute`, `/languages` e os subcomandos `run`, `serve`, `languages`, `doctor`, so that o runner.codes seja usável de ponta a ponta.

Acceptance Criteria
1. O contrato atual da API é preservado e testado com httptest.
2. `serve` sobe o pool e encerra todas as VMs no shutdown.
3. Um `curl` real em `/api/v1/run` no Mac devolve a saída de um programa Python.

### Epic 4: limpeza e entrega

Goal: tirar o Firecracker do repositório e fechar a entrega com gates, docs e release.

#### Story 4.1 — Remoção do Firecracker e gates

As a mantenedor, I want o repositório só com o caminho libkrun e gates que reprovam, so that não reste código morto.

Acceptance Criteria
1. Removidos: `host-agent/`, `guest-runner/`, `proxy/`, `setup/`, `aws/`, `internal/{rootfs,snapshot,firecracker,s3,config}`, o Firecracker de `pkg/host`, `Makefile`, `Taskfile.cli.yaml` e as tasks `aws:*`.
2. `task test:cov` com prova de falha; `task test:targets` reprova `GOOS` sem decisão.
3. e2e no Mac, duas rodadas seguidas, verde.

#### Story 4.2 — Docs, README e release

As a usuário, I want docs que descrevam o sistema real, so that ninguém siga instrução de Firecracker.

Acceptance Criteria
1. Páginas Docusaurus de Firecracker, rootfs, AWS e S3 removidas; arquitetura, instalação, linguagens, API e segurança reescritas.
2. README e `CLAUDE.md` atualizados.
3. `release.yaml` gera linux/amd64, linux/arm64 e darwin/arm64.
4. `npm run build` do Docusaurus sem link quebrado.

## Next Steps

### Architect Prompt

Detalhar em `docs/architecture.md` o caminho de boot sobre a API C da libkrun 1.19.6 e o desenho do pool. Incluir
o sandbox do VMM por SO e a tabela multi-alvo, e registrar os números do spike (story 1.1).

### UX Expert Prompt

Não se aplica: o produto é API e CLI.
