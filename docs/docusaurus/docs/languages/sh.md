---
title: 'sh'
description: 'Shell POSIX (sh) no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `sh` |
| imagem OCI de origem | `alpine:3.22` |
| teto de recursos por job | 1 vCPU, 256 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

Nenhum: a imagem base já traz o `sh`.

## Como o código é executado

O arquivo principal (o primeiro de `files`) é executado com `sh <arquivo>` — o shell da imagem Alpine (BusyBox). Para recursos próprios do Bash (arrays, `[[ ]]`), use a linguagem [`bash`](/languages/bash). Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job, sem rede.

## Construir a imagem

```bash
runner.code image build --lang sh
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "sh",
    "files": [
      {"name": "main.sh", "content": "read n\necho $((n * 2))\n"}
    ],
    "stdin": "21"
  }'
```

`stdout` esperado: `42`.
