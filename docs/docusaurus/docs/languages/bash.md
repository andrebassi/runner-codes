---
title: 'Bash'
description: 'Bash no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `bash` |
| imagem OCI de origem | `bash:5.2` |
| teto de recursos por job | 1 vCPU, 256 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

Nenhum: a imagem oficial já traz o necessário.

## Como o código é executado

O arquivo principal (o primeiro de `files`) é executado com `bash <arquivo>`. Os utilitários disponíveis são os da imagem oficial; não há rede, então `curl`, `wget` e gerenciadores de pacote não alcançam nada. Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job.

## Construir a imagem

```bash
runner.code image build --lang bash
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "bash",
    "files": [
      {"name": "main.sh", "content": "read nome\necho \"olá, $nome\"\n"}
    ],
    "stdin": "mundo"
  }'
```

`stdout` esperado: `olá, mundo`.
