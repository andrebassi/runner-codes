---
title: 'Job response'
description: 'El resultado que el guest devuelve al host por vsock'
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

El valor de `executionTime` es ilustrativo.

## Campos

| Campo | Tipo | Descripción |
|-------|------|-----------|
| `trace_id` | string | el mismo del job |
| `status` | string | `"success"` cuando el guest ejecutó el job; `"failed"` cuando lo rechazó (lenguaje desconocido, stdin demasiado grande) o no pudo preparar el directorio |
| `stdout` | string | salida estándar |
| `stderr` | string o `null` | `null` cuando está vacía |
| `exception` | string o `null` | `"Timeout"`, `"Compilation failed"` o error de preparación |
| `executionTime` | entero | ms medidos dentro del guest, desde el inicio de la compilación/ejecución hasta el final |
| `exit_code` | entero | código de salida del programa; se omite cuando es 0 |
| `error` | string | motivo del rechazo; se omite cuando está vacío |

## Cómo leer el resultado

`"status": "success"` indica que la ejecución ocurrió, no que el programa haya funcionado.

| Situación | `status` | `exception` | `stderr` | `exit_code` |
|----------|----------|-------------|----------|-------------|
| el programa terminó bien | success | null | lo que escribió el programa, o null | 0 |
| el programa salió con error | success | null | el error | ≠ 0 |
| fallo de compilación | success | `"Compilation failed"` | salida del compilador | — |
| se excedió el timeout | success | `"Timeout"` | lo que haya | — |

## Qué hace la API con el resultado

En `/api/v1/run` y `/api/v1/execute`, la API reenvía `status`, `stdout`, `stderr`, `exception`, `executionTime` y `error`; el `exit_code` no aparece. Aparece como `exitCode` en el evento `result` de `/api/v1/execute/stream` y en la respuesta del exec de máquina. La CLI lo muestra en la línea de resumen de `rc run` y sale con rc 1 cuando es distinto de cero; `rc machine exec` sale con el propio código del comando.

Si el guest no responde dentro del timeout del job más un margen del host, la VM se mata y la API devuelve `"status": "failed"` con `"error": "E001: operation timed out"`.
