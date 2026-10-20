---
title: 'Segurança'
description: 'O que isola o código não confiável, o que é provado por teste e o que ainda falta'
---

## Modelo

O código do job é tratado como hostil. A barreira principal é a VM; a segunda é o confinamento do processo que roda a VM.

A documentação da libkrun é explícita: **o guest e o VMM ficam no mesmo contexto de segurança.** Uma fuga da VM cai dentro do processo `rc vmm`, com os privilégios dele. Por isso o isolamento é a VM **mais** o confinamento desse processo — e esse confinamento hoje é diferente por plataforma.

## Camadas de um job (nas duas plataformas)

| Camada | Como |
|--------|------|
| VM por job | cada job roda numa VM nova, destruída no fim; nada passa de um job ao seguinte |
| root read-only | o root é um ext4 montado read-only (`/dev/vda`); escrita só no tmpfs de `/tmp` (256 MiB) |
| estado efêmero | o tmpfs some com a VM |
| sem rede | `krun_disable_implicit_vsock` + `krun_add_vsock(ctx, 0)`: sem TSI, sem interface de rede |
| sem virtiofs | o root é disco, não compartilhamento de diretório do host; virtiofs não impediria o guest de alcançar outros diretórios do host |
| tetos por linguagem | vCPU e RAM limitados no catálogo; timeout padrão 10 s, máximo 300 s, com SIGKILL no grupo do processo |
| limites de entrada | stdin ≤ 1.000.000 bytes, até 32 arquivos, código ≤ 4 MiB, nomes sem `/`, sem `..`, sem começar com `.` |
| pai morre, VM morre | nenhuma VM fica órfã se o host cair |

## Por plataforma

| Item | macOS | Linux |
|------|-------|-------|
| hypervisor | Hypervisor.framework | KVM |
| permissão para criar VM | entitlement `com.apple.security.hypervisor` no binário | acesso a `/dev/kvm` |
| VM morre com o pai | pipe de vigia no fd 3 | `Pdeathsig` |
| timeout | SIGKILL no grupo do processo | SIGKILL no grupo do processo |
| uid/gid sem privilégio no processo da VM | — | suportado pela spec (`krun_setuid`/`krun_setgid`) |
| Landlock | não existe no macOS | aplicado ao processo `vmm` de cada VM de job |
| seccomp | não existe no macOS | denylist aplicada a todas as threads (TSYNC) |

No macOS o confinamento do processo `vmm` se resume ao entitlement e ao grupo de processo.

### Confinamento no Linux

Antes de entregar o processo à libkrun, o `vmm` de uma VM de job (root em disco, sem rede) aplica:

| Mecanismo | O que fica liberado | O que fica negado |
|---|---|---|
| Landlock (arquivos) | leitura do disco root e das bibliotecas já carregadas, `/proc`, `/sys`; escrita no console e no diretório próprio da VM (`run/<id>/`); `/dev/kvm` com ioctl | todo o resto do sistema de arquivos |
| Landlock (rede, ABI ≥ 4) | nada | todo `bind` e `connect` TCP (e UDP na ABI 10) |
| Landlock (escopo, ABI ≥ 6) | — | sinais e sockets abstratos fora do próprio processo |
| Landlock (socket Unix, ABI ≥ 9) | o socket da própria VM | `connect` no socket de outra VM |
| seccomp | o resto | `execve`, `ptrace`, `mount`, `unshare`/`setns`, módulos de kernel, `kexec`, `bpf`, `perf_event_open`, chaveiro, relógio, swap, reboot, `userfaultfd` e afins (`EPERM`) |

A libkrunfw é carregada antes do Landlock, porque a libkrun só a abre dentro de `krun_start_enter` e esse `open`
falharia depois do confinamento. O seccomp é uma denylist, e não uma allowlist, de propósito: a allowlist exigiria
medir cada syscall da libkrun num Linux com KVM, e uma que faltasse derrubaria todas as VMs.

**Medido** no Ubuntu 26.04 (kernel 7.0, aarch64, Landlock ABI 8), com o próprio processo confinado: leitura de
arquivo fora da política, gravação ao lado do disco, TCP, `execve` e `personality` negados; leitura do disco e
criação do próprio socket permitidas. Sem o confinamento, a mesma sonda consegue tudo (prova de falha).

**Não medido:** uma microVM completa sob o confinamento num Linux com KVM (a VM de teste não tem `/dev/kvm`).

**Lacuna conhecida:** com Landlock ABI < 9 o kernel não controla `connect` em socket Unix por caminho; o processo
de uma VM ainda alcança o socket de outra. O `rc doctor` avisa. `RUNNER_SANDBOX=off` desliga o confinamento para
diagnóstico, com aviso no stderr.

## Máquinas persistentes

Uma máquina abre mão de parte do isolamento de um job (ela guarda estado e pode ter rede), e cada abertura é
declarada e tem piso:

| Camada | Como |
|--------|------|
| rede desligada por padrão | sem `--net` (ou `net`, `-p` sozinho), a máquina não tem placa de rede; com `-p` sem `--net`, o host alcança a porta, mas o guest não sai |
| política de saída | gateway em userspace no próprio processo `vmm`, sem TSI: allow-host por nome, CIDR, `--block-private`; nome fora da lista recebe NXDOMAIN; IP só vale se veio de um nome permitido. Veja [Rede e volumes](/cli/network) |
| piso que nenhuma regra abre | metadata de nuvem e todo o link-local; o loopback do host só com `--allow-cidr 127.0.0.1`, nunca por IP aprendido em DNS (DNS rebinding) |
| recusas registradas | toda saída negada vai para `egress-denials.log` no diretório da máquina |
| credencial sem ler | o guest vê um placeholder; o gateway troca pelo valor real só no TLS do host da binding, com uma CA da própria máquina (name constraints). Placeholder na URL é recusado com 403. Veja [Credenciais](/cli/credentials) |
| segredos | `--secret-env`/`--secret-file` são lidos no start e mandados ao guest só em memória; não ficam no registro nem no disco da máquina |
| agente SSH | o guest usa as chaves do host por um socket encaminhado; a chave privada não entra na VM |
| volume `:ro` | a proteção está no dispositivo, não só na opção de montagem: `mount -o remount,rw` pelo root do guest não abre o volume (testado) |
| virtualização aninhada | `--nested` é recusado onde o host não aninha; pela API, só com `rc serve --allow-nested-virt` |
| tela | o visualizador escuta só no `127.0.0.1` do host, sem senha |

O que uma máquina **não** protege: o root do guest lê o ambiente dos próprios processos, então um segredo entregue
à máquina está ao alcance do código que roda nela. A substituição de credencial também não impede que o servidor
devolva o valor no corpo da resposta.

## O que o teste ponta a ponta prova

| Propriedade | Prova |
|-------------|-------|
| sem rede | conexão a `1.1.1.1` falha dentro da VM; com a rede ligada de propósito, o teste reprova |
| root read-only | gravar em `/etc` falha |
| sem estado entre jobs | o arquivo gravado em `/tmp` por um job não existe no job seguinte |
| teto de memória | alocar 2 GiB numa VM de 512 MiB falha, sem afetar o host |
| timeout | o job em laço infinito é morto e nenhuma VM sobrevive |

## A API

A API HTTP **não tem autenticação**. O padrão é escutar só em `127.0.0.1`. Não exponha fora de loopback sem um proxy autenticado na frente.

## Build de imagem

O pull da imagem acontece no host. O resto do build roda numa VM própria, com rede, porque a instalação de e2fsprogs e GNU tar e o warmup (`apk add`, por exemplo) precisam dela. A rede da VM de build não chega à VM do job. O tarball base do Alpine usado pela VM de build tem o sha256 conferido antes do uso. As imagens de linguagem vêm das imagens OCI oficiais.

## Pendências

- validar uma microVM completa sob Landlock + seccomp num Linux com KVM;
- autenticação na API (hoje delegada a um proxy);
- isolamento de socket Unix entre VMs em kernel com Landlock ABI < 9.
