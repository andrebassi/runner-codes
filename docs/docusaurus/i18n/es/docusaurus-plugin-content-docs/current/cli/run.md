---
title: 'run y shell'
description: 'Ejecutar código o abrir un shell en una microVM nueva desde la línea de comandos'
---

## Uso

```bash
rc run --stack <linguagem> (--code '<código>' | --file <arquivo>) [--stdin '<texto>'] [--timeout 5s]
```

| Flag | Valor por defecto | Descripción |
|------|--------|-----------|
| `--stack` | — | lenguaje del catálogo |
| `--code` | — | código del archivo principal |
| `--file` | — | lee el código de un archivo local (sustituye a `--code`) |
| `--stdin` | vacío | entrada estándar del programa |
| `--timeout` | `10s` | duración en formato Go (`500ms`, `5s`, `2m`); máximo 300 s |
| `--gpu` | desactivado | VM con la GPU del host vía Vulkan (virtio-gpu/Venus); solo en stacks con `gpu` en el catálogo, si no E007. Consulte [GPU](/advanced/gpu) |

## Qué sucede

`run` no usa pool: arranca una VM nueva (arranque en frío), ejecuta el job y destruye la VM. En la máquina medida, un job de Python tardó ~240 ms en total, con 31–38 ms de ejecución.

La salida se entrega en **streaming**: cada fragmento aparece en la terminal mientras el programa se ejecuta, no solo al final.

- El stdout del programa va al stdout.
- El stderr del programa va al stderr, seguido de una línea de resumen:

```
[status=success exit=0 exec=33ms total=241ms timeout=false]
```

(valores ilustrativos)

### Buffer del lenguaje

El streaming entrega lo que el programa **escribe** en el pipe. Cuando la salida no es una terminal, varios lenguajes
acumulan el stdout en un buffer y solo escriben al final (o por bloques):

| Lenguaje | Comportamiento en streaming |
|-----------|----------------------------|
| Python | línea por línea: el runner activa `PYTHONUNBUFFERED=1` en modo streaming |
| Node.js, Go, Bash, sh | línea por línea, sin configuración |
| C, C++ (`printf`), Ruby, Perl | por bloques: llame a `fflush(stdout)`, `$stdout.sync = true` o `$\| = 1` en el programa |
| stderr | sin buffer en todos |

## Código de salida

| Situación | rc |
|----------|----|
| el programa terminó con 0 | 0 |
| el programa terminó con código ≠ 0, falló al compilar o excedió el timeout | 1 |
| job rechazado (lenguaje, límites) o falla de infraestructura | 1, con el mensaje de error |

## Ejemplos

```bash
# Python con stdin
rc run --stack python --code 'print(input().upper())' --stdin 'olá'

# Archivo local
rc run --stack go --file ./main.go

# Timeout corto
rc run --stack bash --code 'sleep 10' --timeout 2s
```

Mediante el Taskfile:

```bash
task run -- --stack python --code 'print(1)'
```

## shell

```bash
rc shell --stack python [--gpu]
task shell -- --stack python
```

Abre un shell interactivo en una VM nueva de la stack, con la misma imagen de los jobs: root de solo lectura, sin red
y `/tmp` escribible. La VM desaparece con `exit`.

- **En una terminal**, el guest recibe un TTY real (consola virtio en lugar de la consola implícita) y la terminal del
  host queda en modo raw durante la sesión: Ctrl+C interrumpe solo el comando en el guest, y el tamaño de la ventana
  llega al guest (`stty size`).
- **Con stdin por pipe**, la consola es la normal, sin TTY: `echo 'python3 -V' | rc shell --stack python` funciona.
- Es `bash` si la imagen lo tiene; en las imágenes Alpine, `sh`.

Para un shell que guarde lo que instale, use una máquina: `rc machine run -it --stack python` (desaparece al final) o
`rc machine shell` en una máquina encendida. Consulte [machine](/cli/machine).

## Requisito previo

La imagen del lenguaje debe estar construida (`rc image build --stack <l>`); sin ella, el job se rechaza con E006.
