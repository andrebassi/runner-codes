---
title: 'Imagens'
description: 'Como a imagem OCI oficial vira um ext4 read-only: build, cache e warmup'
---

## O que é uma imagem aqui

Cada linguagem do catálogo roda sobre um arquivo ext4 read-only, que vira o root da VM. Ele é gerado a partir da imagem OCI **oficial** da linguagem (por exemplo `python:3.12-alpine`), com três coisas acrescentadas:

- o guest estático em `/opt/runner/guest`;
- o `/opt/runner/start.sh`, primeiro processo da VM;
- o resultado do warmup da linguagem, quando houver.

## Como o build funciona

```bash
rc image build --stack python
rc image build --all
```

1. **Pull sem Docker.** O host baixa a imagem com go-containerregistry, para a arquitetura do host.
2. **Achatamento no host.** As camadas viram um único tar.
3. **VM de build.** Uma VM Alpine 3.22.6 (o tarball base oficial do Alpine, com sha256 conferido) recebe o tar, com e2fsprogs e GNU tar instalados via `apk`. Dentro dela, como root:
   - extrai o tar num tmpfs preservando dono e permissões (`--numeric-owner`);
   - copia o guest e o `start.sh` para `/opt/runner/`;
   - roda o warmup em chroot, com rede, só nesta etapa;
   - gera o ext4 com `mkfs.ext4 -d`.
4. O ext4 vai para o cache e um symlink `<lang>.ext4` passa a apontar para ele.

### Por que uma VM de build

- O APFS do macOS não diferencia maiúsculas de minúsculas: `Case` e `case` viram o mesmo arquivo. Dentro da VM o filesystem é Linux, sensível a caixa.
- O e2fsprogs do Homebrew não lê tar, então não dá para gerar o ext4 direto do tar no Mac.
- Extrair como root preserva uid, gid e permissões da imagem.

## Cache

A chave da imagem é:

```
sha256(digest da imagem + warmup + build.sh + start.sh + guest)[:12]
```

| Sistema | Caminho |
|---------|---------|
| macOS | `~/Library/Caches/runner-codes/images/<lang>-<chave>.ext4` |
| Linux | `~/.cache/runner-codes/images/<lang>-<chave>.ext4` |

com o symlink `<lang>.ext4` no mesmo diretório. A raiz do cache muda com `RUNNER_CACHE_DIR`.

Consequência: mudar o guest, o `start.sh`, o script de build, o warmup ou a imagem de origem gera uma chave nova e força o rebuild na próxima execução de `image build`. Nada precisa ser invalidado à mão. Se a imagem existente foi construída com um guest antigo, basta rodar o build de novo.

## Warmup

O warmup é uma linha de `sh` declarada no `languages.yaml`. Ele roda **na VM de build**, com rede, e o resultado fica gravado na imagem. Nada do warmup roda na VM do job.

| Linguagem | Warmup | Por quê |
|-----------|--------|---------|
| python | `python3 -m compileall` na stdlib | a imagem oficial não traz `.pyc`; compilar tira esse custo do primeiro import. A imagem passou de 66 para 85 MiB e o warmup levou ~1 s |
| go | compila um programa com `GOCACHE=/opt/runner/gocache` | popula o cache de build; como o root é read-only, o `start.sh` copia o cache para `/tmp/go-cache` no boot (o caminho que o executor força), antes de a VM entrar no pool |
| c, cpp | `apk add build-base` | a imagem base é `alpine:3.22`, sem compilador |
| perl | `apk add perl` | idem |
| jq | `apk add jq` | idem |
| sqlite | `apk add sqlite` | idem |

As demais linguagens não têm warmup.

## Imagens fora do catálogo

Máquinas aceitam uma imagem OCI qualquer (`rc machine create --image alpine:3.20`, ou `image = "..."` no
[runner.code](/cli/runner-code)). O build é o mesmo, com o warmup vazio, e a imagem entra no cache como as outras. Um
[pack](/cli/pack) extraído vira a imagem `pack:<chave>`.

## Limpar o cache

```bash
rc image prune          # builds antigos, que nenhum nome aponta mais
rc image prune --all    # também as imagens e packs que nenhuma máquina usa
```

## Tempos medidos

Na máquina medida (macOS 27.0.1, Apple Silicon), a imagem Python levou ~8–10 s para construir, dos quais 2,7 s de
pull e achatamento. Nas imagens grandes o `mkfs.ext4` domina: 28 s dos 35 s do Go, porque o ext4 é escrito no host via
virtiofs. A tabela de cada stack está no [Relatório de testes](/advanced/test-report).
