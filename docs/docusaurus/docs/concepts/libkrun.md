---
title: 'libkrun'
description: 'Como o Runner Codes usa a libkrun: boot, processo por VM, rede desligada e root read-only'
---

## O que é

A [libkrun](https://github.com/containers/libkrun) é um VMM em forma de biblioteca: o processo que a chama vira a VM. O kernel vem da libkrunfw, também como biblioteca. Versões em uso:

| Componente | Versão |
|------------|--------|
| libkrun | 1.19.6 (macOS, derivação própria em `nix/`); 1.19.5 no nixpkgs (Linux) |
| libkrunfw | 5.6.2, kernel Linux 6.12.109 (macOS, `nix/`); 5.5.0 no nixpkgs (Linux) |

O Runner Codes chama a libkrun diretamente, sem camada intermediária, e a carrega em tempo de execução com purego — não há cgo no host.

| Plataforma | Hypervisor |
|------------|-----------|
| macOS Apple Silicon | Hypervisor.framework (HVF) |
| Linux x86_64 / aarch64 | KVM (`/dev/kvm`) |
| Windows | não suportado: a libkrun upstream não tem backend WHP |

## Um processo por VM

`krun_start_enter` não retorna enquanto a VM roda: ele assume o processo. Por isso cada VM é um processo próprio, `rc vmm --spec <arquivo>`, criado pelo launcher.

- **Spawn, nunca fork.** Fork de um processo Go (multi-thread) é morto pelo HVF cerca de 2 s depois.
- **Timeout:** SIGKILL no grupo do processo da VM.
- **Pai morre, VM morre** (VMs de job): no Linux, `Pdeathsig`; no macOS, que não tem equivalente, o filho vigia um pipe herdado no fd 3 e sai quando ele fecha. A VM de uma máquina persistente é a exceção: roda destacada, em sessão própria, e sobrevive ao `rc` que a ligou.
- **Biblioteca no macOS:** o processo `vmm` precisa de `DYLD_LIBRARY_PATH` apontando para o diretório da libkrun para achar `libkrunfw.5.dylib`; o launcher define isso.
- **Entitlement no macOS:** o binário precisa do `com.apple.security.hypervisor`. Sem ele, `krun_start_enter` devolve `-22`.

## Root: disco ext4 read-only

A VM monta como root um disco ext4 read-only:

- `krun_add_disk2` anexa o ext4 da linguagem;
- `krun_set_root_disk_remount` monta `/dev/vda` como root, read-only.

O primeiro processo de uma VM de job é `/opt/runner/start.sh`, que:

1. monta um tmpfs de 256 MiB em `/tmp`;
2. exporta o ambiente da config OCI (`/opt/runner/env`: `PATH`, `JAVA_HOME`...) e define `HOME=/tmp/home`;
3. para Go, copia o cache de build aquecido de `/opt/runner/gocache` para `/tmp/go-cache`;
4. executa o guest na porta 5000.

Toda escrita vai para o tmpfs, que some com a VM. Numa máquina, o primeiro processo é o `/opt/runner/machine.sh`, que
monta o disco da máquina e um overlayfs por cima da imagem; veja [machine](/cli/machine).

**Por que disco e não virtiofs:** com virtiofs, o compartilhamento não impede o guest de alcançar outros diretórios do host (documentação da libkrun), e não foi mais rápido.

## Rede: desligada nos jobs, por política nas máquinas

Por padrão a libkrun liga sozinha o TSI (transparent socket impersonation), que dá rede ao guest por meio do host. O
Runner Codes desliga:

- `krun_disable_implicit_vsock` impede o vsock implícito;
- `krun_add_vsock(ctx, 0)` adiciona um vsock sem nenhuma flag de TSI.

Resultado: a VM de job não tem rede. O teste ponta a ponta confirma isso tentando conectar em `1.1.1.1`, e prova o
próprio teste, reprovando quando a rede é ligada de propósito.

Máquina com `--net` também não usa TSI: a libkrun liga uma virtio-net a um socketpair
(`krun_add_net_unixstream`), e do lado do host fica um gateway em userspace, dentro do próprio processo `vmm`, que
aplica a política de saída. Veja [Rede e volumes](/cli/network).

## Canal com o guest

`krun_add_vsock_port2` com `listen=true` liga a porta vsock 5000 do guest a um socket Unix no host. O protocolo está em [Comunicação vsock](/concepts/vsock-communication).

## Argumentos do processo do guest

A libkrun leva o argv do primeiro processo pela cmdline do kernel. Argumento com espaço chega partido no guest. Por isso tudo que é comando composto vai num arquivo (é o caso do `start.sh` e do warmup no build), nunca como argumento.

## Memória e CPU

A memória da libkrun é elástica: o host só paga as páginas que o guest toca. Configurar de 256 a 2048 MiB não mudou o tempo de boot na medição. Os valores de vCPU e RAM por linguagem do catálogo são teto contra abuso, não reserva.
