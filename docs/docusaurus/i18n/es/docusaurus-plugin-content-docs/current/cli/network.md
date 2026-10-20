---
title: 'Red y volúmenes'
description: 'Red de salida con política (allow_hosts, CIDRs, bloqueo de privadas), puertos publicados en 127.0.0.1 y directorios del host montados vía virtiofs'
---

Una máquina nace **sin red y sin nada del host**. Ambos se abren por declaración, máquina por máquina, y cada apertura
es algo que el código dentro de la VM pasa a poder usar. Dele a un workload no confiable solo lo que necesita.

## En una pantalla

```bash
rc machine create --name web --stack python --net                       # internet abierta (con el piso)
rc machine create --name api --stack python --allow-host api.openai.com # solo ese nombre y sus subdominios
rc machine create --name srv --stack python -p 8080:8000                # puerto publicado, sin salida
rc machine create --name dev --stack python -v ./src:/app -v ~/models:/models:ro
rc machine update --name api --allow-host pypi.org                      # con la máquina encendida
```

## Cómo funciona la red

```
guest (eth0 100.96.0.2/30)                         host (proceso `rc vmm` de la máquina)
  │ virtio-net                                     ┌──────────────────────────────────────┐
  └──── socketpair (4 bytes + frame Ethernet) ────▶│ gateway: netstack de gVisor          │
                                                   │  DNS  ─ filtro de nombres ─▶ resolvedor del host
                                                   │  TCP  ─ política en SYN   ─▶ conexión del host
                                                   │  UDP  ─ política          ─▶ socket del host
                                                   │  -p   ◀─ 127.0.0.1:HOST   ─ conexión entrante
                                                   └──────────────────────────────────────┘
```

- **Gateway en userspace, dentro del proceso de la VM.** El netstack de gVisor (la misma pila de gvproxy) queda del
  lado del host de un socketpair que libkrun conecta a la virtio-net del guest (`krun_add_net_unixstream`). Vive y
  muere con la VM.
- **NAT:** cada conexión TCP o flujo UDP del guest se convierte en una conexión del host hacia el mismo destino.
  Ningún paquete del guest llega en crudo a la red del host.
- **El guest configura su propia red en el arranque:** `eth0` `100.96.0.2/30`, ruta por defecto y DNS en `100.96.0.1`
  (el gateway), vía netlink. La imagen no necesita tener `ip`.
- **Sin TSI.** Toda máquina con red pasa por el gateway, incluso con solo `net = true`, para que el piso descrito
  abajo se aplique siempre: TSI no aplica ningún filtro.

## Política de salida

| Declaración | Efecto |
|------------|--------|
| nada | sin tarjeta de red |
| `--net` / `net = true` | internet abierta, con el piso |
| `--allow-host example.com` | `example.com` y sus subdominios; el resto no resuelve |
| `--allow-host =example.com` | solo el nombre exacto |
| `--allow-host '*.example.com'` | solo subdominios |
| `--allow-host-pattern registry.npmjs.org` | nombre exacto (o `*.x` para subdominios) |
| `--allow-cidr 1.1.1.1/32` | la IP o el rango |
| `--deny-cidr 10.0.0.0/8` | denegado antes de cualquier permiso |
| `--block-private` | cierra RFC 1918, CGNAT (100.64/10) y ULA; internet sigue abierta |
| `--allow-private 192.168.1.0/24` | reabre un rango privado (implica `--block-private`) |
| solo `-p`, sin `--net` | el host alcanza el puerto; el guest **no** sale ni resuelve nada |

Cualquier `allow` invierte el valor por defecto: solo sale lo que esté permitido. Toda regla de salida implica `--net`.

**Cómo un nombre se convierte en una IP permitida.** El guest consulta al DNS del gateway, y:
- un nombre fuera de la lista recibe **NXDOMAIN**: el programa ve `bad address` al instante, sin esperar un timeout;
- un nombre permitido va al resolvedor del host, y las IPs de la respuesta quedan permitidas durante su TTL, limitado
  a 60 s–1 h;
- cualquier consulta UDP/53, a cualquier IP (un `8.8.8.8` fijo en el código, por ejemplo), cae en el mismo filtro.

Una conexión a una IP que no vino de un nombre permitido se rechaza.

**Solo `--allow-cidr`, sin lista de hosts:** el DNS resuelve cualquier nombre (no hay nada que filtrar), pero la
conexión solo sale hacia los CIDRs listados. Resolver no autoriza: hasta el 19/10/2026 toda IP resuelta contaba como
"aprendida" y pasaba, y una máquina con `--allow-cidr 127.0.0.1/32` alcanzaba `example.com`. Ese agujero está cerrado,
con prueba unitaria y e2e (`TestMachineEgressCIDROnly`).

**El piso, que ninguna regla abre:**
- los metadatos de la nube (`169.254.169.254`, `fd00:ec2::254`, `100.100.100.200`) y todo el link-local. Es el
  ataque clásico de SSRF para robar la credencial de la cuenta.
- el **loopback del host** (`127.0.0.1`, que el guest alcanza por la IP del gateway, `100.96.0.1`) queda cerrado por
  defecto. Solo se abre con `--allow-cidr 127.0.0.1`, y nunca por una IP aprendida vía DNS: un dominio permitido no
  puede apuntar al loopback (DNS rebinding).

**El rechazo es rápido.** El SYN denegado recibe un RST antes de que exista un socket en el host, y el programa falla
en milisegundos. Todo rechazo va a `<cache>/machines/<nome>/egress-denials.log`:

```
2026-10-19T22:44:24Z deny dns google.com: name not in the allow list
2026-10-19T22:45:02Z deny tcp 1.2.3.4:443: not in the allow list
```

## Cambiar la política con la máquina encendida

```bash
rc machine update --name api --allow-host pypi.org --allow-cidr 1.1.1.1
rc machine update --name api --remove-allow-host pypi.org
```

- El cambio se graba en el registro y se entrega al gateway por el socket de control (`egress.sock`, modo 0600).
- Se aplica en la siguiente conexión o consulta. Las IPs aprendidas de un nombre eliminado se olvidan al instante.
- Una máquina encendida **sin** tarjeta de red rechaza el update con `INVALID_STATE`: deténgala, actualícela y vuelva
  a encenderla.
- Por la API: `PUT /api/v1/machines/:name/egress` con la política completa en el cuerpo.

## Puertos publicados

```bash
rc machine create --name srv --stack python -p 8080:8000 -p 5353:53/udp
```

- `PORTA`, `HOST:GUEST`, un rango del mismo tamaño en ambos lados (`5173-5180:5173-5180`, o solo `5173-5180`) y el
  sufijo `/udp`. El rango se convierte en un puerto por número; hasta 64 por máquina, contando cada uno.
- Escuchan **solo en el `127.0.0.1`** del host. Exponer en la red del host es decisión de quien opera el host, no de
  quien escribe el `runner.code`.
- La política de salida no se aplica al puerto (es tráfico de entrada). Un puerto sin `--net` permite que el guest
  sirva sin poder conectarse hacia fuera.
- El UDP publicado obtiene una sesión por cliente, cerrada tras 60 s sin tráfico.

## Volúmenes

```bash
rc machine create --name dev --stack python -v ./src:/app -v ~/models:/models:ro
```

| Punto | Cómo es |
|-------|--------|
| formato | `HOST:GUEST`, `HOST:GUEST:ro`, `HOST:GUEST:rw` (rw por defecto) |
| transporte | virtio-fs (`krun_add_virtiofs3`), una etiqueta por volumen (`vol0`, `vol1`...) |
| `:ro` | impuesto por libkrun en el **dispositivo**: el root del guest no lo deshace con un remount |
| cuándo se monta | en cada `start`, antes del workdir y del init (el workdir puede ser un volumen) |
| ruta del host | debe ser un directorio existente; la ruta real (sin enlace simbólico) se graba en el `create` |
| relativa | en la CLI, al directorio actual; en el `runner.code`, a la carpeta del archivo |
| `~` | se expande al HOME |
| destino prohibido | `/`, `/proc`, `/sys`, `/dev`, `/tmp`, `/opt/runner`, `/persist` y lo que esté debajo |
| límite | 16 por máquina |
| por la API | **rechazado**: un volumen por la API sería lectura y escritura arbitraria en el disco del servidor para quien alcance el puerto |
| archivo nuevo que crea el guest | en macOS queda `0600` en el host (directorio `0700`): el virtio-fs de libkrun guarda el modo del guest en el xattr `user.containers.override_stat` (medido). Un archivo existente sobrescrito mantiene su modo |

### Volumen de disco

`-v /var/lib/docker` (solo la ruta del guest) es un directorio en el ext4 de la máquina, fuera del overlay, enlazado a
la ruta en cada start. Es lo que necesita el overlay2 de Docker, y persiste como el resto del disco.

### Volumen staged: `-v HOST:GUEST:staged`

```bash
rc machine create --name dev --stack python -v ./repo:/work:staged
rc machine start --name dev        # 1.er start: copia ./repo al disco de la máquina
rc machine sync --name dev         # devuelve la copia al host
```

| Punto | Cómo es |
|-------|--------|
| dónde vive | en el ext4 de la máquina (el mismo lugar que un volumen de disco en `GUEST`): el workload no hace un ida y vuelta de virtio-fs por archivo |
| semilla | solo en el primer start; un start posterior no pisa lo que el workload cambió |
| sync | espejo: lo que desapareció de la copia desaparece del host, y lo que el host creó o editó después del primer start se sobrescribe. Edite solo desde la máquina |
| cuándo sincroniza | `rc machine sync`, `POST /api/v1/machines/{name}/sync`, el stop ordenado y el final de `machine run`. `stop --timeout 0` no sincroniza |
| sync que falla | el stop no apaga la máquina: la copia sigue en su disco |
| modos | el sync aplica en el host el modo del guest (un script nuevo llega como `755`, no `0600`) |
| branch | la hija hereda la copia como un volumen de disco común y no devuelve nada al host |

## Volumen S3 (AWS, R2, MinIO)

```bash
rc machine create --name etl --stack python --net \
  -e AWS_ACCESS_KEY_ID=... -e AWS_SECRET_ACCESS_KEY=... \
  -e AWS_ENDPOINT_URL=https://<conta>.r2.cloudflarestorage.com \
  -v s3://meu-bucket/datasets:/data:ro
```

| Punto | Cómo es |
|-------|--------|
| formato | `s3://BUCKET[/PREFIX]:/GUEST[:ro]`, en `-v`, en `volumes` del `runner.code` y por la API (el directorio del host por la API sigue rechazado; un bucket no toca el disco del servidor) |
| quién monta | el **guest**, vía FUSE (`go-fuse`, `mount(2)` directo): la imagen no necesita herramientas FUSE |
| credenciales | del `-e` de la máquina, nunca del host: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`, `AWS_REGION` (`us-east-1` por defecto) y `AWS_ENDPOINT_URL` (R2, MinIO...). Sin par de claves, lectura anónima |
| red | requiere `--net` (o una política de salida): el tráfico sale del guest por el gateway, sujeto a la política. S3 en el host: `AWS_ENDPOINT_URL=http://100.96.0.1:PORTA` y `--allow-cidr 127.0.0.1/32` |
| protocolo | path-style, firma SigV4 propia (el vector oficial de AWS está en las pruebas) |
| start | el guest lista el prefijo antes de montar: un bucket inexistente o una clave incorrecta **hacen fallar el start** con el motivo de S3 |
| lectura | bajo demanda, por rango (`Range`); no se descarga nada completo al montar |
| escritura | en una copia local en el tmpfs del guest; se sube completa en el `close`/`fsync` (y en el stop, que desmonta) |
| `mv` | `CopyObject` + `Delete`: no es atómico; una copia que falla deja el origen intacto |
| `:ro` | `EROFS` en cualquier escritura, sin tocar el bucket |

Límites: sin enlaces simbólicos; propietario, permisos y fecha no persisten; el rename de directorios no existe;
`mkdir` vale en el montaje y el directorio pasa a existir en el bucket con el primer archivo dentro. La grabación de
un marcador `x/` se descartó porque un backend S3 compatible (`rclone serve s3`) lo graba como archivo, y el directorio
se rompe. Una escritura perdida con la VM antes del `close` no llega al bucket.

## Red con nombre

```bash
rc machine run -d --name db --stack python --network lab -- python3 -m http.server 8080
rc machine create --name app --stack python --network lab && rc machine start --name app
rc machine exec --name app -- wget -qO- http://db.lab.internal:8080/
```

| Punto | Cómo es |
|-------|--------|
| dirección | de `100.98.0.10` en adelante, asignada en el `create` y fija mientras exista la máquina |
| nombre | `<máquina>.<rede>.internal` resuelve en el DNS de la máquina |
| aislamiento | redes con nombres distintos no se alcanzan ni se resuelven entre sí; cada red cuenta sus IPs por separado |
| `--net` | **no** es implícito: el tráfico dentro de la red no es salida, y sin `--net` la máquina solo habla con las de la red |
| protocolo | TCP; el servidor ve la IP de red de quien se conectó |
| máquina detenida | desaparece de la red al instante: la conexión se rechaza y el nombre da NXDOMAIN; el stop y el delete limpian incluso después de un `SIGKILL` |
| en el `runner.code` | `name = "lab"` en la tabla `[network]`; `--network` en la CLI tiene prioridad |
| por la API | campo `"network": "lab"` en el `POST /api/v1/machines` |

Por dentro: cada gateway publica la máquina en un socket Unix `<ip>.sock` y en un archivo `<nome>.ip`, en el
directorio de la red (`~/Library/Caches/runner-codes/n/<hash do nome>`, en Linux `~/.cache/runner-codes/n/...`). Una
conexión a `100.98.x.y` no pasa por la política de salida: va directo al socket de la máquina dueña de la IP, con 6
bytes de cabecera (IP de origen y puerto).

## En el `runner.code`

```toml
stack = "python"
net = true
ports = ["8080:8000"]
volumes = ["./src:/app", "~/models:/models:ro"]

[network]
allow_hosts = ["pypi.org", "files.pythonhosted.org"]
block_private = true

[dev]
ports = ["3000"]          # sustituye la lista de arriba
volumes = [".:/workspace"] # sustituye la lista de arriba
```

`net_backend = "virtio-net"` se acepta; `"tsi"` se rechaza con el motivo.

## Números medidos (macOS 27, M1 Pro)

| Medida | Valor |
|--------|-------|
| `start` de una máquina con red | 190 ms |
| nombre bloqueado hasta el error en el guest | < 1 s (NXDOMAIN) |
| metadatos rechazados | inmediato (RST) |

## Límites conocidos

- **Solo IPv4.** El guest no recibe dirección IPv6, y las conexiones IPv6 no salen.
- **ICMP:** el `ping` sale por el host en un socket ICMP de datagrama (sin root en macOS; en Linux, el usuario debe
  estar en `net.ipv4.ping_group_range`), bajo la misma política que TCP, y la respuesta solo vuelve al guest si el
  host recibió la suya. Lo denegado va a `egress-denials.log` como `deny icmp`. Hasta 32 pings en vuelo por máquina.
- La red con nombre solo admite TCP: el UDP entre máquinas de la red no pasa. El `ping` a una IP de la red responde si
  la máquina dueña de ella está encendida en la red (no hay un kernel del otro lado que reenvíe el echo; el tráfico de
  la red es por conexión).
- En Linux, una máquina con red se ejecuta sin Landlock/seccomp: el gateway en el mismo proceso abre sockets.
