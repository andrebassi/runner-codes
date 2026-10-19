---
title: 'C++'
description: 'C++ no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `cpp` |
| imagem OCI de origem | `alpine:3.22` + `build-base` |
| teto de recursos por job | 1 vCPU, 512 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

`apk add build-base`: a imagem base não traz compilador. O pacote é instalado na VM de build e fica gravado no ext4; o job não precisa de rede. Veja [Imagens](/concepts/images).

## Como o código é executado

O arquivo principal (o primeiro de `files`) é compilado com `g++ -o <binário> <arquivo>` e o binário é executado com o stdin do job. Erro de compilação devolve `exception: "Compilation failed"`, com a saída do `g++` em `stderr`. Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job, sem rede.

## Construir a imagem

```bash
runner.code image build --lang cpp
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "cpp",
    "files": [
      {"name": "main.cpp", "content": "#include <iostream>\n\nint main() {\n    int n;\n    std::cin >> n;\n    std::cout << n * 2 << std::endl;\n}\n"}
    ],
    "stdin": "21"
  }'
```

`stdout` esperado: `42`.
