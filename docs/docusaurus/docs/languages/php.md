---
title: 'PHP'
description: 'PHP no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `php` |
| imagem OCI de origem | `php:8.3-cli-alpine` |
| teto de recursos por job | 1 vCPU, 512 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

Nenhum: a imagem oficial já traz o necessário.

## Como o código é executado

O arquivo principal (o primeiro de `files`) é executado com `php <arquivo>` (CLI). Os demais arquivos ficam no mesmo diretório e podem ser incluídos com `require`. Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job, sem rede.

## Construir a imagem

```bash
rc image build --stack php
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "php",
    "files": [
      {"name": "main.php", "content": "<?php\necho strtoupper(trim(fgets(STDIN))), PHP_EOL;\n"}
    ],
    "stdin": "abc"
  }'
```

`stdout` esperado: `ABC`.
