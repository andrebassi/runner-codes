---
title: 'jq'
description: 'jq no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `jq` |
| imagem OCI de origem | `alpine:3.22` + `jq` |
| teto de recursos por job | 1 vCPU, 256 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

`apk add jq`: a imagem base não traz o `jq`. O pacote é instalado na VM de build e fica gravado no ext4. Veja [Imagens](/concepts/images).

## Como o código é executado

O arquivo principal (o primeiro de `files`) é o filtro, executado com `jq --null-input -f <arquivo>`. Com `--null-input`, a entrada inicial do filtro é `null`; para ler o JSON do stdin do job, use `input` (um valor) ou `inputs` (todos). Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job, sem rede.

## Construir a imagem

```bash
runner.code image build --lang jq
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "jq",
    "files": [
      {"name": "main.jq", "content": "input | .a + .b\n"}
    ],
    "stdin": "{\"a\": 40, \"b\": 2}"
  }'
```

`stdout` esperado: `42`.
