---
title: 'Python'
description: 'Python no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `python` |
| imagem OCI de origem | `python:3.12-alpine` |
| teto de recursos por job | 1 vCPU, 512 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

`python3 -m compileall` na stdlib. A imagem oficial não traz `.pyc`; compilar na imagem tira esse custo do primeiro import. A imagem passou de 66 para 85 MiB e o warmup levou ~1 s.

O warmup roda uma vez, na VM de build, e o resultado fica gravado no ext4. Veja [Imagens](/concepts/images).

## Como o código é executado

O arquivo principal (o primeiro de `files`) é executado com `python3 <arquivo>`. Os demais arquivos ficam no mesmo diretório e podem ser importados. Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job, sem rede.

## Construir a imagem

```bash
runner.code image build --lang python
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "python",
    "files": [
      {"name": "main.py", "content": "from util import dobro\nprint(dobro(int(input())))\n"},
      {"name": "util.py", "content": "def dobro(x):\n    return 2 * x\n"}
    ],
    "stdin": "21"
  }'
```

`stdout` esperado: `42`.

## Números medidos

No macOS 27.0.1, Apple Silicon: boot até a 1ª resposta com p50 260 ms e p95 266 ms; job pela CLI com boot frio em ~240 ms no total (execução 31–38 ms); pela API com pool cheio, 26–43 ms. Build da imagem: ~8–10 s. Veja [Desempenho](/advanced/performance).
