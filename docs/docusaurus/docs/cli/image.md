---
title: 'image'
description: 'Construir o ext4 read-only de uma linguagem a partir da imagem OCI oficial'
---

## Uso

```bash
rc image build --stack python
rc image build --stack python --stack go
rc image build --stack python,go
rc image build --all
```

| Flag | Descrição |
|------|-----------|
| `--stack` | linguagem do catálogo; repetível ou separada por vírgula |
| `--all` | todas as linguagens do catálogo |

Sem `--stack` nem `--all`, o comando falha com `informe --stack ou --all`. Linguagem fora do catálogo falha com E006.

Pelo Taskfile (que compila antes):

```bash
task image:build -- --stack python
task image:build -- --all
```

## Saída

Uma linha por linguagem:

```
python <caminho do ext4> cached=false elapsed=<tempo> <métricas do build>
```

- `cached=true` quando a chave já existia no cache e nada foi reconstruído.
- As métricas vêm da VM de build: tempo de extração, de warmup e de `mkfs`, e o tamanho usado e do disco.

## O que o build faz

1. Baixa a imagem OCI com go-containerregistry (sem Docker) e achata as camadas num tar.
2. Sobe uma VM de build Alpine 3.22.6 (minirootfs com sha256 conferido; e2fsprogs e GNU tar via `apk`).
3. Dentro dela, como root: extrai o tar num tmpfs, injeta o guest e o `start.sh` em `/opt/runner/`, roda o warmup em chroot e gera o ext4 com `mkfs.ext4 -d`.
4. Grava `<lang>-<chave>.ext4` no cache e aponta o symlink `<lang>.ext4` para ele.

O porquê de cada etapa, a chave do cache e o warmup de cada linguagem estão em [Imagens](/concepts/images).

## Quando reconstruir

A chave inclui o digest da imagem, o warmup, o `build.sh`, o `start.sh` e o guest. Depois de mudar qualquer um deles — inclusive recompilar o guest com outro código —, rode o build de novo: a chave muda sozinha e uma imagem nova é gerada.

## Tempo

Na máquina medida (macOS 27.0.1, Apple Silicon), a imagem Python levou ~8–10 s, sendo 2,7 s de pull e achatamento. O build precisa de rede (pull e warmup); a VM do job não tem.
