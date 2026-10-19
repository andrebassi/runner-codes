---
title: 'Job response'
description: 'O resultado que o guest devolve ao host pelo vsock'
---

## Formato

```json
{
  "trace_id": "tr-123",
  "status": "success",
  "stdout": "42\n",
  "stderr": null,
  "exception": null,
  "executionTime": 34,
  "exit_code": 0
}
```

O valor de `executionTime` é ilustrativo.

## Campos

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `trace_id` | string | o mesmo do job |
| `status` | string | `"success"` quando o guest executou o job; `"failed"` quando recusou (linguagem desconhecida, stdin grande demais) ou não conseguiu preparar o diretório |
| `stdout` | string | saída padrão |
| `stderr` | string ou `null` | `null` quando vazio |
| `exception` | string ou `null` | `"Timeout"`, `"Compilation failed"` ou erro de preparação |
| `executionTime` | inteiro | ms medidos dentro do guest, do início da compilação/execução ao fim |
| `exit_code` | inteiro | código de saída do programa; omitido quando 0 |
| `error` | string | motivo de recusa; omitido quando vazio |

## Como ler o resultado

`"status": "success"` diz que a execução aconteceu, não que o programa deu certo.

| Situação | `status` | `exception` | `stderr` | `exit_code` |
|----------|----------|-------------|----------|-------------|
| programa terminou bem | success | null | o que o programa escreveu, ou null | 0 |
| programa saiu com erro | success | null | o erro | ≠ 0 |
| falha de compilação | success | `"Compilation failed"` | saída do compilador | — |
| estourou o timeout | success | `"Timeout"` | o que houver | — |

## O que a API faz com o resultado

A API repassa `status`, `stdout`, `stderr`, `exception`, `executionTime` e `error`. O `exit_code` não aparece na resposta HTTP; a CLI o mostra na linha de resumo e sai com rc 1 quando ele é diferente de zero.

Se o guest não responde dentro do timeout do job mais uma folga do host, a VM é morta e a API devolve `"status": "failed"` com `"error": "E001: operation timed out"`.
