---
title: 'Solução de problemas'
description: 'Erros conhecidos, causa e correção'
---

## Primeiro passo

```bash
rc doctor
```

Confere plataforma, libkrun, GPU, CUDA, virtualização aninhada, guest, confinamento e imagens. Cada linha começa com
`ok`, `info`, `warn` ou `FAIL`, e a falha traz o motivo.

## Couldn't find or load libkrunfw.5.dylib

**Causa:** a libkrun upstream abre a libkrunfw só pelo nome (`dlopen("libkrunfw.5.dylib")`), e o loader dinâmico não a acha fora dos caminhos padrão.

**Correção:** a libkrun do `.#krun` (macOS) já abre a libkrunfw pelo caminho absoluto no store do Nix, então o erro só aparece com uma libkrun de outra origem. Nesse caso o launcher ainda define `DYLD_LIBRARY_PATH` com o diretório de `RUNNER_KRUN_LIB`: confira se ele é um caminho completo e se a libkrunfw está no mesmo diretório.

## krun_start_enter rc=-22

**Causa:** no macOS, o binário não tem o entitlement `com.apple.security.hypervisor`.

**Correção:** compile com `task build`, que assina o host. Um `go build` direto gera um binário sem assinatura.

## /opt/runner/guest: not found

**Causa:** o guest foi compilado dinâmico. As imagens são Alpine (musl) e não têm o loader da glibc; o kernel acha o arquivo, mas não o interpretador, e reporta `not found`.

**Correção:** compile com `task build`, que gera o guest estático e reprova se ele sair dinâmico. Depois reconstrua as imagens.

## Comportamento antigo depois de mudar o guest

**Causa:** a imagem em uso foi construída com o guest anterior.

**Correção:** rode `rc image build --stack <l>` de novo. A chave do cache inclui o guest, então uma imagem nova é gerada sozinha; não é preciso apagar nada.

## Argumento com espaço chega partido no guest

**Causa:** a libkrun leva o argv do primeiro processo pela cmdline do kernel, que separa por espaço.

**Correção:** não passe comando composto como argumento. Grave num arquivo e execute o arquivo — é o que o `start.sh` e o warmup do build fazem.

## VM for X did not become ready ... console: ...

**Causa:** o guest não respondeu ao job `sh true` da espera de prontidão. A mensagem traz o fim do console do guest, que normalmente diz o motivo (guest dinâmico, `start.sh` falhando, imagem corrompida).

**Correção:** leia o trecho de console da mensagem. Os casos mais comuns são os desta página: guest dinâmico ou imagem desatualizada.

## machine X did not become ready / failed to start

**Causa:** o processo `vmm` da máquina morreu no boot ou o guest não respondeu. A mensagem cita a última linha do
`vmm.log` escrita **neste** start (o log é em append), por exemplo `nested virtualization is not supported on this
host`.

**Correção:** leia a linha citada. O log inteiro é o `vmm.log` do diretório que `rc machine data-dir --name X`
imprime; o console sai em `rc machine logs --name X`.

## INVALID_STATE: machine X is running; stop it first or use force

**Causa:** `delete` (e outras operações de máquina parada) numa máquina ligada.

**Correção:** `rc machine stop --name X` antes, ou `--force` para matar.

## machine X is paused; resume it (rc machine resume) or stop it first

**Causa:** `start` ou `exec` numa máquina pausada.

**Correção:** `rc machine resume --name X`, ou `stop` para desligar.

## resolve secret 'NOME': env_unset

**Causa:** um `--secret-env` (ou `[secrets]` com `from_env`) aponta para uma variável que não existe no ambiente de
quem deu o `start`.

**Correção:** exporte a variável antes do `start`; o valor é lido a cada start e nunca fica gravado.

## O programa na máquina não resolve um nome (`bad address`)

**Causa:** a política de saída não permite o nome; o DNS do gateway responde NXDOMAIN de propósito.

**Correção:** confira o `egress-denials.log` do diretório da máquina (`rc machine data-dir`) e libere com
`rc machine update --name X --allow-host <nome>`, com a máquina ligada. Veja [Rede e volumes](/cli/network).

## E007

**Causa:** `--gpu` (ou `"gpu": true`) numa stack sem `gpu` no catálogo.

**Correção:** use uma stack com GPU, como a `llama`. CUDA não existe: a GPU é Vulkan (Venus). Veja [GPU](/advanced/gpu).

## E006: unsupported language

**Causa:** a linguagem não está no catálogo **ou** a imagem dela ainda não foi construída neste host.

**Correção:** `rc languages` mostra o catálogo e `ready`. Construa com `rc image build --stack <l>`.

## E005: stdin too long

**Causa:** stdin acima de 1.000.000 bytes.

**Correção:** reduza a entrada ou envie os dados como arquivo em `files` (até 4 MiB somando todos os arquivos).

## sem acesso a /dev/kvm (Linux)

**Causa:** o usuário não tem permissão de leitura e escrita em `/dev/kvm`, ou o KVM não está habilitado.

**Correção:** habilite o KVM no host e dê ao usuário acesso ao dispositivo (em geral, o grupo `kvm`).

## Windows

Não é suportado: a libkrun upstream não tem backend WHP. O build para Windows falha de propósito.

## A API respondeu "status": "success", mas o programa falhou

Não é erro. `status` diz que a API executou o job. O resultado do programa está em `stderr` e `exception` (`"Timeout"`, `"Compilation failed"`).
