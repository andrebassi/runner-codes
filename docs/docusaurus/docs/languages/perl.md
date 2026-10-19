---
title: 'Perl'
description: 'Perl no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `perl` |
| imagem OCI de origem | `alpine:3.22` + `perl` |
| teto de recursos por job | 1 vCPU, 512 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

`apk add perl`: a imagem base não traz Perl. O pacote é instalado na VM de build e fica gravado no ext4. Veja [Imagens](/concepts/images).

## Como o código é executado

O arquivo principal (o primeiro de `files`) é executado com `perl <arquivo>`. Sem rede, módulos do CPAN não são instalados; só o que vem no pacote `perl` do Alpine. Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job.

## Construir a imagem

```bash
rc image build --stack perl
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "perl",
    "files": [
      {"name": "main.pl", "content": "my $l = <STDIN>;\nchomp $l;\nprint scalar reverse($l), \"\\n\";\n"}
    ],
    "stdin": "abc"
  }'
```

`stdout` esperado: `cba`.
