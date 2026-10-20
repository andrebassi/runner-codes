---
sidebar_position: 8
title: Pack (artefato portátil)
---

# Pack: uma máquina num arquivo

`rc pack create` transforma uma imagem, ou uma máquina com o que foi instalado nela, num artefato que se copia para
outro host e roda sem pull, sem build e sem download.

```bash
rc pack create --stack python --output ./py312         # stub ./py312 + payload ./py312.rcmachine
./py312 run -- python3 --version

rc pack create --from-vm dev --output ./dev --single-file   # um executável só
./dev run -- python3 -c 'import meu_pacote'
```

| Saída | O que é |
|-------|---------|
| `py312` | o stub: este `rc`, assinado com o entitlement de hypervisor no macOS |
| `py312.rcmachine` | o payload: a imagem ext4 (e, de máquina, o disco dela) e o manifesto |
| `--single-file` | o payload anexado ao stub, com um trailer de 16 bytes no fim |

Os dois arquivos viajam juntos. O stub sozinho mostra o uso e sai.

## Origens

| Flag | O que vai no payload |
|------|----------------------|
| `--stack python` | a imagem da stack (construída antes, se ainda não existir) |
| `--image alpine:3.20` | a imagem OCI construída |
| `--from-vm NOME` | a imagem da máquina **e o disco dela**: o que foi instalado e gravado viaja junto. A máquina precisa estar parada (disco ligado não é consistente) |

As flags de máquina (`--cpus`, `--mem`, `-e`, `--workdir`, `--net`, `--allow-host`, `-p`, `--` workload...) entram no
manifesto e viram o padrão do artefato. Não viajam:
- volume de diretório do host (o caminho é de outra máquina), que é recusado com o motivo; volume `s3://` viaja;
- credenciais, porque CA e placeholders são da máquina de origem;
- rede nomeada.

## Subcomandos do artefato

```bash
./dev run [-- COMMAND...]    # máquina efêmera, apagada no fim (o código de saída volta)
./dev start                  # máquina persistente com o nome do arquivo (criada na 1ª vez)
./dev exec -- pip install x  # nela
./dev shell | logs | status | stop
./dev inspect                # o manifesto, sem extrair
```

Também: `rc machine create --name x --from dev.rcmachine` (ou `--from ./dev`), `rc machine run --from ...` e
`rc pack inspect ARQUIVO`.

## Como é por dentro

- **Formato próprio, esparso.** Um disco de máquina tem 10 GiB lógicos e quase tudo buraco, e o `archive/tar` do Go não
  grava esparso. O payload guarda só os trechos com dado (achados por `SEEK_DATA`/`SEEK_HOLE`) num fluxo gzip; a
  extração recria os buracos. Medido: a imagem python (134 MiB) vira 31 MiB; uma máquina python com pacote instalado,
  em arquivo único com o stub, fica em 46 MiB.
- **Extraído uma vez** em `<cache>/packs/<chave>`, onde a chave é a impressão do manifesto, e registrado como imagem
  `pack:<chave>`. Um segundo `run` não extrai de novo.
- **O payload vem de fora**: nome de arquivo com `/` ou `..`, trecho fora do arquivo, dado cortado, manifesto acima de
  1 MiB e versão de formato futura são recusados (testado).

## Prova: conferir um valor, não um boot

Um pack que perdeu o disco ainda liga e sai com 0. O e2e (`TestPack`) grava um marcador e um módulo Python numa
máquina, empacota, **apaga a máquina de origem** e lê o marcador pelo artefato. O defeito plantado (um pack sem o
disco) é pego.

## O que o artefato não leva

O stub não embute o runtime da VM: usa a **libkrun do host de destino** (`task krun:install`; o
`rc doctor` confere): a libkrun do Nix no macOS linka virglrenderer e MoltenVK por caminho absoluto do `/nix/store`, e
levar esse fechamento exigiria reescrever os caminhos das bibliotecas. O que viaja é a imagem e o estado da máquina,
que é o que não dá para reproduzir no destino. O stub é do sistema e da arquitetura de onde foi criado
(`host_platform` no manifesto).
