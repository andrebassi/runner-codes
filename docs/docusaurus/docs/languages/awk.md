---
title: 'AWK'
description: 'AWK no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `awk` |
| imagem OCI de origem | `alpine:3.22` |
| teto de recursos por job | 1 vCPU, 256 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

Nenhum: a imagem base já traz o `awk`.

## Como o código é executado

O arquivo principal (o primeiro de `files`) é o programa AWK, executado com `awk -f <arquivo>`. O stdin do job é a entrada que o programa processa, linha a linha. O `awk` é o da imagem Alpine (BusyBox). Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job, sem rede.

## Construir a imagem

```bash
rc image build --stack awk
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "awk",
    "files": [
      {"name": "main.awk", "content": "{ soma += $1 } END { print soma }\n"}
    ],
    "stdin": "10\n20\n12\n"
  }'
```

`stdout` esperado: `42`.
