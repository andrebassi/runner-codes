---
title: 'Build'
description: 'Compilação do host e do guest, assinatura no macOS e build das imagens'
---

## task build

```bash
task build
```

Roda `scripts/build.sh`, que:

1. compila o host `bin/rc` para o sistema atual, com `CGO_ENABLED=0` (a libkrun é carregada por purego em tempo de execução);
2. compila o guest `bin/rc-guest-linux-<arch>` a partir de `./cmd/runner-guest`: sempre Linux, estático, da mesma arquitetura do host — a libkrun não emula outra arquitetura;
3. **reprova** se o guest sair dinâmico, porque um binário dinâmico não roda na imagem musl (`not found`);
4. no macOS, assina o host com `codesign` e o entitlement `com.apple.security.hypervisor` (`build/hv.entitlements`). Sem ele, `krun_start_enter` falha.

A versão gravada nos binários vem de `VERSION` ou de `git describe`; sem git, `dev`. É ela que aparece em `GET /health`.

### Por que dois binários

O purego força link dinâmico contra a glibc no Linux. As imagens das linguagens são Alpine (musl). Por isso o guest é um `main` separado, sem purego, compilado estático.

## task image:build

```bash
task image:build -- --stack python
task image:build -- --all
```

Depende de `task build` (o guest precisa existir para ser injetado). Veja [image](/cli/image) e [Imagens](/concepts/images).

## Instalação da libkrun

```bash
task krun:install
```

| Sistema | Como |
|---------|------|
| macOS | `task krun:install`: libkrun 1.19.6 / libkrunfw 5.6.2 das derivações em `nix/` |
| Linux | `task krun:install`: libkrun / libkrunfw do nixpkgs, com virtio-blk ligado |

Os dois instalam `.#krun` no perfil do Nix (`~/.nix-profile/lib`), onde o `rc` procura por padrão.

## Windows

Não há build para Windows. Compilar para `windows/amd64` falha de propósito, com uma mensagem que diz o motivo (a libkrun upstream não tem backend WHP). O gate `task test:targets` confere que essa recusa continua acontecendo.
