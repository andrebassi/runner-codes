---
sidebar_position: 6
title: Credenciales sin leer
---

# Credenciales que la máquina usa sin leerlas

Una API key o un token que el programa de la máquina **usa**, pero que no puede **leer**. El guest recibe solo un
placeholder en la variable que espera. El gateway sustituye el placeholder por el valor real en las solicitudes HTTPS a
los hosts permitidos, y solo dentro de una cabecera. El valor nunca entra en la máquina, en su registro ni en disco.

```bash
rc machine create --name notas --stack python --credential notion=NOTION_API_KEY@api.notion.com
NOTION_API_KEY=secret_... rc machine start --name notas

rc machine exec --name notas -- sh -c 'echo "$NOTION_API_KEY"'
# RC_PLACEHOLDER_NOTION_7F3A...        <- todo lo que tiene el guest
rc machine exec --name notas -- python3 -c '
import os, urllib.request
r = urllib.request.Request("https://api.notion.com/v1/users/me",
    headers={"Authorization": "Bearer " + os.environ["NOTION_API_KEY"], "Notion-Version": "2022-06-28"})
print(urllib.request.urlopen(r).status)'
# 200                                  <- Notion recibió el valor real
```

## Declarar

```bash
--credential NOME=VARIAVEL@HOST[,HOST...][:body][:set-header=CABECALHO]   # repetible; implica --net
```

```toml
[[network.credentials]]
name = "github"
environment_variable = "GITHUB_TOKEN"
allowed_hosts = ["api.github.com"]
methods = ["GET", "HEAD"]                            # opcional; por defecto: todos
set_header = "authorization"                         # opcional (más abajo)
injection_location = { header = true, body = true }  # opcional: cuerpo de formulario
```

Por la API, el mismo objeto en `credentials` en el `POST /api/v1/machines`.

Reglas verificadas en el create:
- `allowed_hosts` son nombres DNS en minúsculas, o `*.exemplo.com` (todo subdominio, no el propio `exemplo.com`). Sin
  IP, esquema, puerto ni `*.com`, y la lista no puede estar vacía: por defecto, una credencial no va a ningún lado.
- Con `allow_hosts` en la máquina, todo host de credencial debe estar dentro de él: la credencial no amplía la red, y la
  red no amplía la credencial.
- Nombre y variable únicos en la máquina; dos `set_header` iguales para hosts que se superponen se rechazan.

## De dónde viene el valor

Del **entorno de quien ejecuta el `start`** (CLI, `machine run` o `rc serve`). El proceso de la VM hereda ese entorno,
y el gateway lee la variable **en cada solicitud**. El valor no va a la spec, al registro ni al guest. Cambiar la
variable después del start no tiene efecto hasta el siguiente start.

## Lo que ve el guest

- `VARIAVEL=RC_PLACEHOLDER_<NOME>_<32 hexa>`, creado en el create y estable mientras exista la máquina (`branch` lo
  hereda).
- `/run/rc/credentials/ca.pem`: la CA de la máquina. Está restringida por name constraints a los `allowed_hosts`, así
  que el cliente rechaza cualquier certificado que ella firme para otro dominio (probado).
- `/run/rc/ca-bundle.pem`: las raíces de la imagen más la CA de la máquina.
- `SSL_CERT_FILE`, `CURL_CA_BUNDLE`, `REQUESTS_CA_BUNDLE` y `GIT_SSL_CAINFO` apuntan al paquete;
  `NODE_EXTRA_CA_CERTS` y `DENO_CERT`, a `ca.pem`. curl, Python, Node, Deno, Git y OpenSSL funcionan sin cambios.
  El `-e` de la máquina tiene prioridad sobre esas variables.

## Qué sucede con una solicitud

Una conexión al puerto 443, ya aprobada por la política de salida, pasa por el interceptor, que lee el nombre pedido
en el TLS (SNI):
- **host de un binding**: el TLS termina en el gateway con un certificado de la CA de la máquina. En la solicitud
  (HTTP/1.1) se sustituye el placeholder en su lugar: el `Bearer ` que escribió el guest se mantiene, solo cambia el
  placeholder. Sigue en una conexión TLS nueva, verificada, **hacia la misma IP que resolvió el guest**. La respuesta
  vuelve en streaming (los server-sent events llegan en vivo).
- **cualquier otro host**: los bytes pasan intactos, el guest ve el certificado real, y un placeholder ahí es solo
  texto.

Rechazos, con una línea de motivo que nunca incluye el valor:

| Status | Cuándo |
|--------|--------|
| 403 | placeholder en la URL (ruta o query), en una cabecera de enrutamiento, de encuadre o de cookie (`Host`, `Cookie`, ...), en el cuerpo sin `:body`, placeholder que la máquina no emitió, más de un placeholder, `Upgrade`/`CONNECT` |
| 405 | binding que no aplica al host o al método |
| 415 | con `:body`, un cuerpo que no es `application/x-www-form-urlencoded` |
| 413 | cuerpo por encima de 8 MiB (el cuerpo se lee completo para buscar el placeholder) |
| 502 | `runner.code credentials: credential unavailable`: variable ausente, vacía o con un carácter de control (un `\r\n` inyectaría una cabecera). El detalle va al log del proceso de la VM |

Con `set_header`, el host define la cabecera completa (incluido `Bearer ...`) en toda solicitud a esos hosts y
métodos, por encima de lo que envió el guest. Sirve para clientes que no se pueden apuntar a un placeholder (un
`git clone`, por ejemplo).

## Límites

- Solo se intercepta el puerto 443. HTTP en claro y los demás puertos pasan sin sustitución.
- Del lado del guest, HTTP/1.1. WebSocket y `CONNECT` se rechazan en los hosts de credencial.
- El servidor aún puede devolver la credencial en el cuerpo de la respuesta: esto es sustitución, no prevención de
  fugas.
- Fuera de alcance: `--secret-file`/`[secrets]` como fuente del valor, `PUT .../credential-values` de la API y el
  interceptor externo (`--egress-interceptor`). El valor viene solo del entorno del start.
