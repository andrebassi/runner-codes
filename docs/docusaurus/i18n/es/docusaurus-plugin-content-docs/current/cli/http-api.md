---
title: 'API HTTP (serve)'
description: 'Levantar la API HTTP con un pool de VMs pre-arrancadas'
---

## Uso

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
```

| Flag | Valor por defecto | Descripción |
|------|--------|-----------|
| `--listen` | `127.0.0.1:8080` (o `RUNNER_LISTEN`) | dirección de escucha |
| `--pool-size` | `2` | VMs listas por lenguaje |
| `--allow-nested-virt` | desactivado | acepta `nestedVirt` en el create de máquina; sin la flag, quien alcanza el puerto no puede entregar la virtualización del host a un guest |

Al arrancar, `serve` imprime la dirección, el tamaño del pool y los lenguajes con imagen lista. Solo esos entran en el [pool](/concepts/pool).

SIGINT o SIGTERM cierran el servidor y matan todas las VMs.

## Seguridad

La API **no tiene autenticación**. Por defecto escucha solo en loopback. Para exponerla a otra máquina, ponga un proxy autenticado delante; no cambie `--listen` a `0.0.0.0` sin más.

## Endpoints

| Método | Ruta | Para qué |
|--------|---------|----------|
| GET | `/health` | estado, versión y VMs listas por stack |
| GET | `/api/v1/languages` | catálogo con `ready` |
| POST | `/api/v1/run` | job con `files` y `stdin` (string, o lista: una VM por elemento) |
| POST | `/api/v1/execute` | job con `code` suelto, `timeout` y `gpu` |
| POST | `/api/v1/execute/stream` | lo mismo, en SSE: la salida mientras se ejecuta y `result` al final |
| POST/GET/DELETE | `/api/v1/machines[/:name]` | crear (por campos o por `runner_code`), listar, ver y borrar máquinas |
| POST | `/api/v1/machines/:name/{start,stop,pause,resume,sync,branch}` | ciclo de vida |
| POST | `/api/v1/machines/:name/exec[/stream]` | comando en la máquina encendida, con `exitCode` |
| PUT/GET | `/api/v1/machines/:name/files` | grabar y leer archivos |
| GET | `/api/v1/machines/:name/logs` | consola |
| PUT | `/api/v1/machines/:name/egress` | cambiar la política de salida, incluso con la máquina encendida |

Cuerpos, respuestas, códigos de error y límites en [Visión general de la API](/api-reference/overview). Clientes listos
en [SDKs Go y Python](/api-reference/sdks).

## Ejemplos

```bash
curl -s http://127.0.0.1:8080/health
```

```bash
curl -s http://127.0.0.1:8080/api/v1/languages
```

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{"language":"python","files":[{"name":"main.py","content":"print(int(input())*2)"}],"stdin":["1","2","3"]}'
```

Con stdin como lista, cada elemento se ejecuta en una VM nueva y la respuesta es una lista con el stdin repetido.

```bash
curl -s http://127.0.0.1:8080/api/v1/execute \
  -H 'Content-Type: application/json' \
  -d '{"language":"bash","code":"echo $((6*7))","timeout":5}'
```

```bash
curl -s -X POST http://127.0.0.1:8080/api/v1/machines -d '{"name":"dev","stack":"python"}'
curl -s -X POST http://127.0.0.1:8080/api/v1/machines/dev/start
curl -s -X POST http://127.0.0.1:8080/api/v1/machines/dev/exec -d '{"command":["python3","-c","print(42)"]}'
```

## Latencia

Con el pool lleno, la solicitud HTTP completa tardó 26–43 ms en la máquina medida (macOS 27.0.1, Apple Silicon). Con el pool vacío, el job espera un arranque en frío.
