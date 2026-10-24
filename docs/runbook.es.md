# Runbook — probar rc a mano, paso a paso

Secuencia para verificar en una terminal nueva, un comando a la vez, que `rc` hace lo que promete. Cada paso indica lo que
se debe ver.

**Este runbook es ejecutable.** `task runbook` (`scripts/runbook.sh`) ejecuta exactamente estos pasos y verifica cada
resultado esperado. Falla si alguno sale distinto. Regla del proyecto: **todo recurso nuevo recibe una sección aquí y
el paso correspondiente en `scripts/runbook.sh`, en la misma entrega.**

| Última ejecución | Host | Resultado |
|-----------------|------|-----------|
| 20/10/2026 | macOS 27, Apple M1 Pro | `task runbook`: 0 fallas en 116 verificaciones (hasta el 8u: usuario, secretos, puentes de socket, ping, staged, rechazos) |
| 19/10/2026 | macOS 27, Apple M1 Pro | `task runbook`: 0 fallas en 50 verificaciones (con volúmenes y red, pasos 8c y 8d). Prueba de falla: con el DNS del gateway sin filtro, el 8d falló en 2 verificaciones |

Requisitos previos: `task krun:install`, `task build` y las imágenes (`./bin/rc image build --all`; el paso 9 necesita la
`llama`).

> Ejecuta con el e2e detenido: las pruebas verifican con `pgrep` que no quedó ninguna VM, y una máquina tuya encendida
> las hace fallar.

## 0. Preparar

```bash
cd ~/works/labs/github/runner.codes
./bin/rc doctor
```

Esperado: `ok` en `platform`, `libkrun`, `gpu` y `guest`, y una línea `ok image <stack>` por cada imagen construida.

## 1. Job efímero con salida en streaming

```bash
./bin/rc run --stack python --code $'import time\nprint("early")\ntime.sleep(1.5)\nprint("late")'
```

Esperado: `early` aparece enseguida (~0,3 s, el boot) y `late` 1,5 s después, mientras el programa aún se ejecuta. Al final,
`[status=success exit=0 ...]` en stderr. La VM se destruye al terminar.

## 2. Crear y encender una máquina

```bash
./bin/rc machine create --name teste --stack python
./bin/rc machine start --name teste
./bin/rc machine ls
```

Esperado:
- `created teste (stack=python cpus=4 memory=8192MiB disk=10GiB gpu=false)`;
- `started teste (pid N) in ~200ms`;
- `teste` con `running` en el `ls`.

La máquina sigue encendida después de que el comando termina.

## 3. Escribir cosas en tres lugares

```bash
./bin/rc machine exec --name teste -- sh -c 'echo persistiu > /etc/minha-config; echo dados > /workspace/arquivo.txt; echo some > /tmp/temporario.txt'
```

Esperado: nada en la salida, y rc 0.

## 4. Detener y encender de nuevo

```bash
./bin/rc machine stop --name teste
./bin/rc machine start --name teste
```

Esperado: `stopped teste` y `started teste (pid N) in ~200ms`. El stop hace `sync` del disco antes de apagar.

## 5. Verificar lo que quedó

```bash
./bin/rc machine exec --name teste -- sh -c 'cat /etc/minha-config /workspace/arquivo.txt; cat /tmp/temporario.txt'
```

Esperado:

```
persistiu
dados
cat: can't open '/tmp/temporario.txt': No such file or directory
```

`/etc` (capa escribible sobre la imagen) y `/workspace` (disco de la máquina) persisten; `/tmp` es tmpfs y vuelve vacío.

## 6. Copiar un archivo a la máquina y de vuelta

```bash
head -c 20000000 /dev/urandom > /tmp/rc-up.bin
./bin/rc machine cp /tmp/rc-up.bin teste:/workspace/up.bin
./bin/rc machine exec --name teste -- sha256sum /workspace/up.bin
shasum -a 256 /tmp/rc-up.bin
./bin/rc machine cp teste:/workspace/up.bin /tmp/rc-down.bin
cmp /tmp/rc-up.bin /tmp/rc-down.bin && echo "idênticos"
```

Esperado:
- `uploaded 20000000 bytes ... in ~230ms`;
- el mismo sha256 dentro y fuera de la máquina;
- `downloaded ... in ~230ms`;
- `idênticos`.

## 7. Código de salida y streaming dentro de la máquina

```bash
./bin/rc machine exec --name teste -- sh -c 'echo a; sleep 1; echo b; exit 7'; echo "rc=$?"
```

Esperado: `a`, `b` un segundo después y `rc=7`. El rc de `rc` es el del comando.

## 7b. Terminal interactiva en la máquina

```bash
./bin/rc machine shell --name teste
```

Dentro del shell que se abra, escribe:

```bash
test -t 0 && echo IS-TTY; stty size     # IS-TTY e o tamanho do seu terminal
sleep 30                                 # aperte Ctrl+C: interrompe só o sleep
echo ALIVE                               # o shell continua vivo
exit 4
```

Fuera, `echo $?` muestra `4`. La máquina sigue encendida: el shell es una sesión, no la VM.

Esperado: terminal de verdad, con Ctrl+C, flechas, historial, `vi` y redimensionamiento de la ventana. Es `bash` si la imagen
lo tiene; en la python (Alpine) es `sh`.

## 8. Logs de la máquina

```bash
./bin/rc machine logs --name teste --tail 10
```

Esperado: las últimas líneas de la consola (el guest registra cada operación). Funciona también con la máquina detenida.

## 8b. Máquina declarada en un `runner.code`

El [`runner.code`](docusaurus/docs/cli/runner-code.md) es la máquina declarada en un archivo: stack o imagen, recursos, env, init y
workload en un TOML versionable.

```bash
mkdir -p /tmp/rc-demo && cd /tmp/rc-demo
cat > runner.code <<'TOML'
stack = "python"
cpus = 2
env = ["GREETING=hello"]
workdir = "/app"
init = ["echo $(( $(cat count 2>/dev/null || echo 0) + 1 )) > count"]
entrypoint = ["sh", "-c"]
cmd = ["trap 'echo saved > saved; exit 0' INT; echo \"up $GREETING in $(pwd)\"; while :; do sleep 0.2; done"]
stop_signal = "SIGINT"
stop_timeout = 5
TOML
~/works/labs/github/runner.codes/bin/rc machine create --name web --cpus 1
cd ~/works/labs/github/runner.codes
./bin/rc machine status --name web --json
./bin/rc machine start  --name web
./bin/rc machine logs   --name web --source workload
./bin/rc machine exec   --name web -- sh -c 'cat count; echo $GREETING; pwd'
./bin/rc machine stop   --name web
./bin/rc machine start  --name web
./bin/rc machine exec   --name web -- sh -c 'cat count saved'
./bin/rc machine stop   --name web
./bin/rc machine delete --name web
```

Esperado:
- el `create` sin `-f` dice `from runner.code`, y el status muestra `"cpus": 1`: la flag gana sobre el archivo;
- `logs --source workload` muestra `up hello in /app`: el workdir fue creado y env y workdir llegaron al workload;
- el primer `exec` imprime `1`, `hello`, `/app`;
- el segundo imprime `1` y `saved`: el init **no** se ejecutó de nuevo en el segundo start, y el `SIGINT` del stop llegó al
  workload, que escribió el archivo antes del sync.

Una clave que `runner.code` aún no implementa se rechaza, nunca se ignora:

```bash
printf 'stack = "python"\n[health]\nexec = ["true"]\n' > /tmp/rc-demo/bad.code
./bin/rc machine create --name x -f /tmp/rc-demo/bad.code
```

Esperado: `key "health" is not supported yet: health checks`, y nada creado.

## 8c. Volúmenes: directorio del host dentro de la máquina

```bash
mkdir -p /tmp/rc-vol/ro && echo "do host" > /tmp/rc-vol/hello.txt
./bin/rc machine create --name vol --stack python -v /tmp/rc-vol:/data -v /tmp/rc-vol/ro:/ro:ro -w /data
./bin/rc machine start  --name vol
./bin/rc machine exec   --name vol -- sh -c 'cat hello.txt; echo do-guest > from-guest.txt; echo y > /ro/y; echo ro=$?'
cat /tmp/rc-vol/from-guest.txt
./bin/rc machine stop   --name vol
./bin/rc machine delete --name vol
```

Esperado:
- `do host`: el guest lee el directorio del host;
- `Read-only file system` y `ro=1`: el `:ro` lo impone la libkrun en el device, y el root del guest no lo deshace;
- en el host, `cat` muestra `do-guest`: la escritura del guest aparece en el host al instante.

## 8d. Red: salida, política y puerto

La máquina nace **sin red**. `--net` habilita la salida; `--allow-host` la habilita y la restringe; `-p` publica un puerto en el
`127.0.0.1` del host.

```bash
./bin/rc machine create --name net --stack python --allow-host example.com -p 18080:8000
./bin/rc machine start  --name net
./bin/rc machine exec   --name net -- sh -c 'wget -qO /dev/null http://example.com; echo ok=$?'
./bin/rc machine exec   --name net -- sh -c 'wget -qO /dev/null http://iana.org'
./bin/rc machine exec   --name net -- sh -c 'wget -qO- -T 3 http://169.254.169.254; echo meta=$?'
./bin/rc machine update --name net --allow-host iana.org
./bin/rc machine exec   --name net -- sh -c 'wget -qO /dev/null http://iana.org; echo ok=$?'
./bin/rc machine exec   --name net -- sh -c 'cd /tmp && echo do-guest > i.html && (python3 -m http.server 8000 >/dev/null 2>&1 &)'
curl -s http://127.0.0.1:18080/i.html
cat ~/Library/Caches/runner-codes/machines/net/egress-denials.log
./bin/rc machine stop   --name net
./bin/rc machine delete --name net
```

Esperado:
- `ok=0` para `example.com`;
- `wget: bad address 'iana.org'` **al instante**: el DNS del gateway responde NXDOMAIN, sin esperar timeout;
- `meta=1`: la metadata de nube (`169.254.169.254`) se rechaza incluso con la red abierta;
- después del `update`, `iana.org` pasa **sin reiniciar** la máquina;
- `curl` en el host recibe `do-guest` por el puerto publicado;
- el log de rechazos tiene `deny dns iana.org: name not in the allow list`.

## 8e. Máquina efímera: `machine run`

```bash
./bin/rc machine run --stack python -- python3 -c 'print("ola"); raise SystemExit(3)'; echo "rc=$?"
echo dados | ./bin/rc machine run -i --stack python -- sh -c 'read x; echo leu:$x'
./bin/rc machine run --net --image alpine -- uname -a
./bin/rc machine run -it --image alpine -- /bin/sh        # terminal; `exit` sai e apaga a máquina
./bin/rc machine ls
```

Esperado:
- `ola` y `rc=3`: el rc es el del comando;
- `leu:dados`: el `-i` reenvía el stdin (sin `-i` el stdin no se lee);
- el `uname -a` del kernel de la libkrunfw en una imagen fuera del catálogo (construida en el primer uso);
- el `ls` sin ninguna `run-…`: la máquina efímera desaparece al final, incluso con Ctrl+C.

## 8f. Un puerto publicado alcanza un servidor que solo escucha en localhost

```bash
./bin/rc machine run -d --name web --stack python -p 18080:9000 -- python3 -m http.server 9000 --bind 127.0.0.1
curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:18080/
./bin/rc machine delete --name web -f
```

Esperado: `200`. El puerto publicado entra por el guest (op `dial` en el vsock) y marca el `127.0.0.1` de allí, entonces el
servidor que escucha solo en localhost, como vite u openclaw, responde.

## 8g. Pantalla en el navegador: display, teclado y mouse

```bash
./bin/rc machine create --name tela --image alpine:latest --gpu --net --display 1024x768
./bin/rc machine start --name tela
./bin/rc machine exec --name tela -- sh -c 'apk add -q --no-cache libdrm-tests evtest; cat /sys/class/input/event*/device/name'
./bin/rc machine exec --name tela -- sh -c 'c=$(modetest -M virtio_gpu -c | awk "/connected/ {print \$1; exit}"); (modetest -M virtio_gpu -s $c:1024x768 >/dev/null 2>&1 </dev/null &)'
open http://localhost:6080
./bin/rc machine exec --name tela -- sh -c '(timeout 25 evtest /dev/input/event0 > /tmp/k.log 2>&1 </dev/null &)'
# haz clic en la página y escribe algunas letras; después:
./bin/rc machine exec --name tela -- grep 'value 1' /tmp/k.log
./bin/rc machine delete --name tela -f
```

Esperado:
- el guest lista `runner.code keyboard` y `runner.code tablet`;
- la página muestra las barras de color de `modetest`, en vivo, en 1024x768;
- cada tecla que se escribe en la página aparece en el `evtest` del guest (`KEY_R`, `KEY_C`...).

`task runbook` verifica el mismo camino sin necesitar ojo humano: el cuadro de 1024x768 llega al visualizador y un
`KEY_A` enviado por `POST /input` aparece en `evtest`.

## 8h. Pantalla adaptable y control de pantalla

La misma máquina con pantalla del 8g, ahora controlada por la CLI, por la API y por el MCP, y con la resolución que pide
la página:

```bash
./bin/rc machine create --name tela --image alpine:latest --gpu --net --display 1024x768
./bin/rc machine start --name tela
./bin/rc machine exec --name tela -- sh -c 'apk add -q --no-cache libdrm-tests evtest; mkdir -p /etc/rc; printf "#!/bin/sh\necho \"\$1 \$2\" > /tmp/resize.txt\n" > /etc/rc/display-resize; chmod 755 /etc/rc/display-resize'
curl -s -X POST localhost:6080/resize -d '{"width":900,"height":600}'
./bin/rc machine exec --name tela -- cat /tmp/resize.txt
./bin/rc machine exec --name tela -- sh -c 'c=$(modetest -M virtio_gpu -c | awk "/connected/ {print \$1; exit}"); (sleep 90 | modetest -M virtio_gpu -s $c:1024x768 >/dev/null 2>&1 &)'
./bin/rc machine screen --name tela screenshot -o tela.jpg
./bin/rc machine exec --name tela -- sh -c '(timeout 8 evtest /dev/input/event0 > /tmp/k.log &); (timeout 8 evtest /dev/input/event1 > /tmp/t.log &)'
./bin/rc machine screen --name tela key ctrl+c
./bin/rc machine screen --name tela click 1023 767
./bin/rc machine exec --name tela -- sh -c 'sleep 8; grep -h "value 1" /tmp/k.log /tmp/t.log | head'
./bin/rc machine screen --name tela key hyper
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' '{"jsonrpc":"2.0","id":2,"method":"tools/list"}' | ./bin/rc mcp --name tela
./bin/rc machine delete --name tela -f
```

Esperado:
- `900 600` en el `/tmp/resize.txt`: el tamaño que pide la página llega al hook `/etc/rc/display-resize` del guest (en
  el escritorio, ejecuta `xrandr`; sin el hook, no pasa nada);
- `screen 1024x768 -> tela.jpg`, un JPEG de la pantalla;
- en el `evtest`: `KEY_LEFTCTRL` y `KEY_C` presionadas, `ABS_X` en `32767` (el borde derecho) y `BTN_LEFT`;
- la página del visor dibuja una flecha naranja donde la CLI hizo clic (`GET /pointer` registra el `click`);
- la página de la pantalla muestra `runner.codes` en la barra (`display_title` en el `runner.code` cambia el nombre);
- `key hyper` sale con error (`unknown key "hyper"`), sin enviar nada a la máquina;
- el `rc mcp` responde al `initialize` y lista 9 herramientas: `screenshot`, `click`, `move`, `drag`, `type`, `key`,
  `scroll`, `actions` y `exec`.

`task runbook` hace lo mismo y verifica también la API: `GET /api/v1/machines/:name/screen` con `X-Screen-Width: 1024`,
400 para una acción inválida y 204 para una válida en `POST .../screen/actions`.

## 8i. Branch y pausa

```bash
./bin/rc machine create --name mom --stack python && ./bin/rc machine start --name mom
./bin/rc machine exec   --name mom -- sh -c 'echo antes > /root/marca'
./bin/rc machine pause  --name mom && ./bin/rc machine status --name mom
./bin/rc machine branch --from mom --name kid
./bin/rc machine resume --name mom
./bin/rc machine start  --name kid && ./bin/rc machine exec --name kid -- cat /root/marca
./bin/rc machine delete --name kid -f && ./bin/rc machine delete --name mom -f
```

Esperado:
- `status` muestra `paused`, y un `exec` en ese estado se rechaza con el motivo;
- la hija muestra `antes`: el branch lleva lo que la madre escribió, incluso con ella pausada (`rc` la reanuda por un instante
  para el `sync`);
- después de eso, madre e hija son independientes.

## 8j. Red con nombre

```bash
./bin/rc machine run -d --name db --stack python --network lab -- python3 -m http.server 8080
./bin/rc machine create --name app --stack python --network lab && ./bin/rc machine start --name app
./bin/rc machine exec --name app -- wget -qO- -T 3 http://100.98.0.10:8080/
./bin/rc machine exec --name app -- wget -qO- -T 3 http://db.lab.internal:8080/
./bin/rc machine delete --name db -f
./bin/rc machine exec --name app -- nslookup db.lab.internal
./bin/rc machine delete --name app -f
```

Esperado:
- las dos máquinas reciben IPs consecutivas de la red (`db` es `100.98.0.10`, `app` es `100.98.0.11`) y se alcanzan por IP
  y por el nombre `<máquina>.<rede>.internal`, sin `--net`: el tráfico dentro de la red no es salida;
- una máquina de otra red no alcanza estas, ni resuelve sus nombres;
- `db` borrada desaparece del DNS de la red: el `nslookup` ya no devuelve su IP.

## 8k. Volumen S3

Un S3 local, sirviendo una carpeta (MinIO salió de nixpkgs el 11/10/2026):

```bash
mkdir -p /tmp/s3/bkt/data && echo 'ola do bucket' > /tmp/s3/bkt/data/hello.txt
nix shell nixpkgs#rclone -c rclone serve s3 /tmp/s3 --addr 127.0.0.1:19000 --auth-key chave,segredo123 &
./bin/rc machine create --name s3 --stack python --net --allow-cidr 127.0.0.1/32 \
  -e AWS_ACCESS_KEY_ID=chave -e AWS_SECRET_ACCESS_KEY=segredo123 -e AWS_ENDPOINT_URL=http://100.96.0.1:19000 \
  -v s3://bkt/data:/data
./bin/rc machine start --name s3
./bin/rc machine exec --name s3 -- sh -c 'cat /data/hello.txt; echo do-guest > /data/novo.txt'
cat /tmp/s3/bkt/data/novo.txt
./bin/rc machine delete --name s3 -f
```

Esperado:
- el `start` monta el bucket (con una clave incorrecta, falla con `s3: 403` y el motivo);
- el guest lee `ola do bucket`, y `novo.txt` aparece en la carpeta del servidor con `do-guest`.

## 8l. Credencial que la máquina usa sin leer

```bash
./bin/rc machine create --name cred --stack python --credential tok=RB_TOKEN@postman-echo.com
RB_TOKEN=valor-do-runbook ./bin/rc machine start --name cred
./bin/rc machine exec --name cred -- python3 -c '
import json, os, urllib.request
tok = os.environ["RB_TOKEN"]
print("guest:", tok)
r = urllib.request.Request("https://postman-echo.com/headers", headers={"Authorization": "Bearer " + tok, "User-Agent": "runbook/1"})
print("server:", json.load(urllib.request.urlopen(r, timeout=20))["headers"]["authorization"])'
./bin/rc machine delete --name cred -f
```

Esperado:
- `guest: RC_PLACEHOLDER_TOK_...`: el guest solo tiene el placeholder;
- `server: Bearer valor-do-runbook`: el servidor recibió el valor real, que vino del entorno del `start`.

El User-Agent es propio porque el Cloudflare de postman-echo rechaza `Python-urllib` (403, error code 1010).

## 8m. Pack: la máquina en un archivo

```bash
./bin/rc machine create --name pk --stack python && ./bin/rc machine start --name pk
./bin/rc machine exec --name pk -- sh -c 'echo marca-do-runbook > /root/marca'
./bin/rc machine stop --name pk
./bin/rc pack create --from-vm pk --output /tmp/one --single-file
./bin/rc machine delete --name pk
/tmp/one run -- cat /root/marca
/tmp/one
```

Esperado:
- `marca-do-runbook`: el artefacto llevó el disco de la máquina, incluso con el origen borrado (un pack que perdiera el disco
  aún arrancaría y saldría con 0; por eso se verifica un valor);
- `/tmp/one` solo muestra `usage: one run ...`.

## 8n. Usuario, volumen de disco y `docker.sock`

```bash
./bin/rc machine create --name usr --stack python --user nobody
./bin/rc machine start --name usr
./bin/rc machine exec --name usr -- id -un
./bin/rc machine exec --name usr --user root -- id -un
./bin/rc machine create --name dk --stack python --docker-socket -v /var/lib/dados -- python3 -c '
import os, socket
s = socket.socket(socket.AF_UNIX); s.bind("/var/run/docker.sock"); s.listen()
while True:
    c, _ = s.accept(); c.sendall(b"do-guest:" + c.recv(64)); c.close()'
./bin/rc machine start --name dk
./bin/rc machine exec --name dk -- stat -f -c %T /var/lib/dados
python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.connect(sys.argv[1]); s.sendall(b"ping"); print(s.recv(64).decode())' "$(./bin/rc machine data-dir --name dk)/docker.sock"
```

Esperado:
- `nobody`, y `root` con `--user root` (el `--user` del exec gana sobre el de la máquina);
- `ext2/ext3`: el volumen de disco es ext4 de verdad, fuera del overlay (lo que necesita el overlay2 de Docker);
- `do-guest:ping`: el host habla con el servidor del guest por el `docker.sock` del directorio de la máquina.

## 8o. Secretos y agente SSH

```bash
ssh-keygen -q -t ed25519 -N "" -f /tmp/rb-id && ssh-agent -D -a /tmp/rb-agent.sock & sleep 1
SSH_AUTH_SOCK=/tmp/rb-agent.sock ssh-add -q /tmp/rb-id
echo do-arquivo > /tmp/rb-sec.txt
./bin/rc machine create --name sec --image alpine --net --init 'apk add -q --no-cache openssh-client' \
  --secret-env API=RB_SECRET --secret-file ARQ=/tmp/rb-sec.txt --ssh-agent
RB_SECRET=valor-secreto SSH_AUTH_SOCK=/tmp/rb-agent.sock ./bin/rc machine start --name sec
./bin/rc machine exec --name sec -- sh -c 'echo $API $ARQ; ssh-add -l'
grep -rl valor-secreto --exclude='*.ext4' "$(./bin/rc machine data-dir --name sec)"
```

Esperado:
- `valor-secreto do-arquivo`: los dos secretos en el entorno de todo comando;
- `ssh-add -l` lista la clave de `/tmp/rb-id` (la privada queda en el agente del host);
- el `grep` no encuentra nada: el valor no queda en el registro de la máquina.

El agente y la clave son desechables, de este paso: nada toca el agente de quien ejecuta.

## 8p. Puentes de socket en ambas direcciones

```bash
python3 -c 'import socket; s=socket.socket(socket.AF_UNIX); s.bind("/tmp/rb-h.sock"); s.listen()
while True:
    c,_=s.accept(); c.sendall(b"do-host:"+c.recv(64)); c.close()' &
./bin/rc machine create --name skt --stack python --expose-socket /run/app.sock --mount-socket /tmp/rb-h.sock:/run/h/h.sock -- python3 -c '
import socket
s = socket.socket(socket.AF_UNIX); s.bind("/run/app.sock"); s.listen()
while True:
    c, _ = s.accept(); c.sendall(b"do-guest:" + c.recv(64)); c.close()'
./bin/rc machine start --name skt
python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.connect(sys.argv[1]); s.sendall(b"ping"); print(s.recv(64).decode())' "$(./bin/rc machine data-dir --name skt)/app.sock"
./bin/rc machine exec --name skt -- python3 -c 'import socket; s=socket.socket(socket.AF_UNIX); s.connect("/run/h/h.sock"); s.sendall(b"pong"); print(s.recv(64).decode())'
```

Esperado: `do-guest:ping` (el host alcanza el socket en el que escucha el workload) y `do-host:pong` (el guest alcanza el
socket del host, en un directorio que la imagen no tenía).

## 8q. Ping y rango de puertos

```bash
./bin/rc machine create --name png --stack python --net -p 18301-18303:18301-18303 -- python3 -m http.server 18302
./bin/rc machine start --name png
curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:18302/
./bin/rc machine exec --name png -- ping -c1 -W3 1.1.1.1
./bin/rc machine create --name pngd --stack python --allow-cidr 198.51.100.0/24
./bin/rc machine start --name pngd
./bin/rc machine exec --name pngd -- ping -c1 -W2 1.1.1.1
grep 'deny icmp' "$(./bin/rc machine data-dir --name pngd)/egress-denials.log"
```

Esperado:
- `200`: el rango publica un puerto por número (hasta 64), y el del medio responde;
- el ping a `1.1.1.1` vuelve con la red abierta, y solo si el host recibió la respuesta (no hay respuesta inventada);
- fuera de la allowlist el ping no vuelve, y la denegación `deny icmp 1.1.1.1` va al log.

## 8r. Volumen staged y sync

```bash
mkdir -p /tmp/rb-repo && echo original > /tmp/rb-repo/a.txt && echo x > /tmp/rb-repo/old.txt
./bin/rc machine create --name stg --stack python -v /tmp/rb-repo:/work:staged
./bin/rc machine start --name stg
./bin/rc machine exec --name stg -- sh -c 'stat -f -c %T /work; echo mudou > /work/a.txt; rm /work/old.txt; printf "#!/bin/sh\n" > /work/run.sh; chmod 755 /work/run.sh'
cat /tmp/rb-repo/a.txt
./bin/rc machine sync --name stg
cat /tmp/rb-repo/a.txt; ls /tmp/rb-repo; ls -l /tmp/rb-repo/run.sh
```

Esperado:
- `ext2/ext3`: la copia está en el disco de la máquina, sin ida y vuelta de virtio-fs;
- antes del sync el host sigue `original`;
- después: `mudou`, sin `old.txt` (el sync espeja: lo que desapareció de la copia desaparece del host) y `run.sh` con `-rwxr-xr-x`.

El stop ordenado y el final de `machine run` también sincronizan; `--timeout 0` no. La hija de un branch no devuelve nada
al host.

## 8s. Rechazos que dicen el motivo

```bash
./bin/rc doctor | grep -E 'cuda|nested'
./bin/rc machine create --name x --image alpine --cuda
time ./bin/rc machine run --nested --image alpine -- ls -l /dev/kvm
```

Esperado:
- `info  cuda: not supported (no CUDA remoting; --gpu is Vulkan)` y la línea de nested de este host;
- `--cuda is not supported: rc has no CUDA remoting ...`, sin crear máquina;
- en un host que no anida (M1/M2), `failed to start: Error: nested virtualization is not supported on this host ...`
  en menos de 1 s (antes: 30 s y `did not become ready`); en un host que anida, `/dev/kvm` dentro de la VM.

## 8t. Directorio de la máquina y limpieza de la caché

```bash
ls "$(./bin/rc machine data-dir --name usr)"
./bin/rc image prune
./bin/rc pack prune
```

Esperado: `machine.json` en el directorio de la máquina; los dos prunes salen con 0 (sin `--all`, `image prune` solo borra
el ext4 al que ya no apunta ningún nombre, el build anterior de una imagen, y `pack prune` conserva los 5 usados más recientemente).

## 8u. Las guías

```bash
task guides DIRS=gpu-cuda      # uma pasta; sem DIRS, todas (cada uma liga as suas VMs)
```

Esperado: `falhas: 0` en cada carpeta. Lo que prueba cada carpeta está en `examples/guides/README.md`.

## 8v. Terminal web

```bash
./bin/rc machine web-terminal --name demo                 # abre el navegador con el shell de la máquina
./bin/rc machine web-terminal --name demo --no-open -- sh -c 'echo wt-ok:$((6*7))'
```

Esperado:
- `web terminal: http://127.0.0.1:PUERTO/?t=<32 hex>`: solo loopback, con token; el navegador se abre solo
  (`--no-open` solo imprime);
- en el navegador, un terminal real (xterm.js): Ctrl+C, flechas, editores a pantalla completa y el tamaño de la
  ventana llegan al guest, y un F5 vuelve a mostrar la pantalla reciente;
- con el comando, la pantalla muestra `wt-ok:42`, la página avisa que el programa salió y `rc` sale con su código;
- `/stream` sin el token responde 403, y un `Host` que no sea de loopback también (DNS rebinding).
- pestañas `claude` (el comando), `shell` (un terminal aparte, con el primer clic), `eventos` y, al final, una pestaña por puerto: un servidor que el programa levante en la VM (cualquier puerto) aparece con la app en un iframe y responde en el `127.0.0.1` del host.

Para abrir Claude Code en el navegador así:
`rc machine web-terminal --name agent -- claude --dangerously-skip-permissions`.

## 8w. Playground público (claude-in-chrome.runner.codes)

Cada visitante recibe una Fly Machine propia (Fly no expone `/dev/kvm`, así que la microVM es la de Fly), con Claude
Code en el terminal web y los puertos en `https://<puerto>-<id>.runner.codes/`. Máximo 3 máquinas; 30 min sin uso la
apaga, 24 h sin uso la borra.

```bash
scripts/playground-cloudflare.sh     # túnel claude-in-chrome, ingress y CNAMEs (idempotente)
scripts/playground-fly.sh setup      # apps runner-codes-playground y runner-codes-visitors, red propia, secrets
scripts/playground-image.sh all      # imágenes visitor y front con Nix en el builder de Fly
scripts/playground-fly.sh deploy     # publica el front
scripts/playground-token-check.sh    # el token del front solo alcanza la app de visitantes
curl -s https://claude-in-chrome.runner.codes/ | grep -c FingerprintJS.load
```

Esperado:
- la página abre, calcula el visitorId del navegador y lleva al terminal `/v/<id>/?t=<token>` con Claude Code;
- `/api/session` con visitante inválido responde 400; con 3 máquinas en uso, 503 "playground is full";
- `https://3000-0000000000.runner.codes/` (máquina que no existe) responde 502, y `docs.runner.codes` sigue en 200;
- `scripts/playground-token-check.sh`: 200 en la app de visitantes, 4xx en el sitio y en el propio front;
- dentro de la máquina del visitante, abrir conexión a la red privada de Fly (`_api.internal`) falla, y DNS e internet
  funcionan (`scripts/fly-firewall-probe.sh` mide el mismo firewall en una máquina desechable).

## 8x. Runtime de Kubernetes (k3s y microk8s)

Cada pod con `runtimeClassName: rc` se vuelve una microVM de libkrun con kernel propio: containerd llama a
`containerd-shim-rc-v2`, el contenedor pause arranca la VM y los contenedores de la app corren dentro de ella. La VM
toma la IP que el CNI le dio al pod (un TAP en el netns del pod con `tc mirred`). Necesita un nodo Linux con
`/dev/kvm`; los de prueba son droplets de DigitalOcean (`task k8s:kvm-probe` lo mide).

```bash
K8S_NODE=k3s task k8s:node -- create && K8S_NODE=k3s task k8s:node -- provision   # igual con K8S_NODE=microk8s
# en el nodo: curl -sfL https://get.k3s.io | sh -   o   snap install microk8s --classic
task k8s:e2e -- k3s        # lleva el árbol, compila, corre deploy/k3s/install.sh y deploy/k8s/e2e.sh k3s
task k8s:e2e -- microk8s   # lo mismo con deploy/microk8s/install.sh
task k8s:defects           # planta 20 defectos en el código del runtime y comprueba que los tests los atrapan
```

Esperado (19 comprobaciones de `deploy/k8s/e2e.sh`, `falhas: 0` en los dos):
- `rc-hello` queda Ready y su log muestra `RC_K8S_OK`, el kernel de la VM (6.12.91, no el del nodo) y la IP del pod en `eth0`;
- `kubectl exec` sin TTY, con TTY y con stdin; el código de salida del exec (7) y del contenedor (3) vuelven bien;
- DNS del clúster, pod rc ↔ pod runc en los dos sentidos y un Service ClusterIP hasta el pod rc;
- dos contenedores en un pod: la misma VM (mismo `boot_id`) y el mismo emptyDir;
- el resumen de estadísticas del kubelet responde (el uso de la VM va al primer contenedor);
- tras el delete, no queda ningún directorio en `/var/lib/rc-shim/pods` ni ningún proceso `rc vmm`.

Límites declarados: port-forward, hostNetwork, hostIPC y la propagación de montajes no cruzan la VM; los volúmenes se
copian al share del pod al crear el contenedor (lo que el kubelet cambie después, como el token de serviceaccount, no
llega); los límites de CPU/memoria valen para la VM entera (anotaciones `runner.codes/cpus` y
`runner.codes/memory-mib` del pod); el vmm del pod corre sin Landlock/seccomp.

## 9. GPU

Solo si la imagen `llama` está construida (`rc doctor` muestra `ok image llama`).

```bash
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'
./bin/rc run --stack llama --gpu --timeout 120s --code 'llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -ngl 99 -dev Vulkan0'
```

Esperado:
- `deviceName = Virtio-GPU Venus (Apple M1 Pro)`, y también una línea de `llvmpipe` (Vulkan en software);
- la tabla de `llama-bench` con `Vulkan0`, `pp128` en el rango de 900–1.365 t/s y `tg32` de ~57 t/s.

Detalles en [GPU](docusaurus/docs/advanced/gpu.md).

## 10. API HTTP

En una terminal:

```bash
./bin/rc serve --pool-size 0
```

En otra:

```bash
curl -s -X POST localhost:8080/api/v1/machines/teste/exec -H 'content-type: application/json' -d '{"command":["python3","-c","print(6*7)"]}'
curl -s localhost:8080/api/v1/machines | head -c 300; echo
```

Esperado:
- `{"exitCode":0,"stdout":"42\n","stderr":"",...}`;
- la lista con `"name":"teste"` y `"state":"running"`.

Los cuerpos alternativos y el stream por línea, en el mismo servidor:

```bash
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t2","image":"alpine","allowedHost":["a.com"]}'
curl -s -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t3","image":"alpine","allowedHosts":["api.example.com"],"memoryMb":512}'
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t4","image":"alpine","nestedVirt":true}'
curl -sN -X POST localhost:8080/api/v1/machines/teste/exec/stream -H 'content-type: application/json' -d '{"command":["sh","-c","echo a; echo b"]}'
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines/teste/sync
```

Esperado:
- `400`: un campo desconocido (`allowedHost`, con error de tipeo) se rechaza, y no se ignora dejando la salida abierta;
- `"allow_hosts":["api.example.com"]`, `"enabled":true` y `"memory_mib":512`: los nombres alternativos valen;
- `400` en `nestedVirt` (solo pasa con `rc serve --allow-nested-virt`);
- `event: stdout` / `data: a`, después `data: b`: una línea por evento, texto crudo, y el `event: exit` al final;
- `204` en el `sync` (máquina sin volumen staged: nada que hacer).

`Ctrl+C` termina el serve. La máquina sigue encendida, porque está desacoplada.

## 10b. SDKs Go y Python

Con el `serve` todavía en marcha:

```bash
go run ./examples/sdk-go http://127.0.0.1:8080
PYTHONPATH=sdk/python python3 examples/sdk-python/main.py http://127.0.0.1:8080
```

Esperado, en ambos:
- `job: 42` (job efímero);
- `machine: sdk-... running` y `exec: ola do sdk go` (o `python`): máquina creada, archivo escrito y ejecutado;
- `error: NOT_FOUND`: el rechazo de la API llega como error tipado, con el código estable;
- no queda ninguna máquina `sdk-*` en `./bin/rc machine ls`.

## 10c. SDK Go en un proyecto externo (go get)

El módulo Go se llama `runner.codes`, y `https://runner.codes` responde al `go get` con la meta `go-import` que apunta
a GitHub. En un directorio fuera del repositorio, con el `serve` todavía en marcha:

```bash
curl -s 'https://runner.codes/pkg/client?go-get=1'
mkdir /tmp/sdkcheck && cp examples/sdk-go/main.go /tmp/sdkcheck/ && cd /tmp/sdkcheck
go mod init sdkcheck
go get runner.codes/pkg/client@latest
go run . http://127.0.0.1:8080
```

Esperado:
- el `curl` devuelve `<meta name="go-import" content="runner.codes git https://github.com/andrebassi/runner-codes">`;
- el `go get` descarga `runner.codes v1.1.0` (o más nueva);
- la salida del `go run` es la misma del 10b: `job: 42`, `exec: ola do sdk go` y `error: NOT_FOUND`.

Lo mismo para Python, en un venv limpio, instalando desde GitHub:

```bash
python3 -m venv /tmp/sdkcheck/venv
/tmp/sdkcheck/venv/bin/pip install "git+https://github.com/andrebassi/runner-codes@v1.1.1#subdirectory=sdk/python"
/tmp/sdkcheck/venv/bin/python -I <repositorio>/examples/sdk-python/main.py http://127.0.0.1:8080
```

Esperado: el `pip` instala `runner-code 1.1.1`, y la salida es la misma del 10b (`job: 42`, `exec: ola do sdk python`,
`error: NOT_FOUND`). El `-I` garantiza que el paquete viene del venv, no del repositorio.

## 10d. Recetas de los SDKs

Las siete recetas de la [página de SDKs](docusaurus/docs/api-reference/sdks.md), en Go y en Python, con el `serve` todavía en marcha:

```bash
go run ./examples/sdk-go-recipes http://127.0.0.1:8080
PYTHONPATH=sdk/python python3 examples/sdk-python-recipes/main.py http://127.0.0.1:8080
```

Esperado, en ambos (el tiempo varía):

```
untrusted: stdout=45 stderr=aviso tempo=58ms
files: 42
timeout: exception=Timeout
compile: exception=Compilation failed stderr_tem_erro=true
machine: sdk-go pypi=200 example=bloqueado
branch: feito antes do branch
errors: 404 NOT_FOUND
```

En la línea `machine`, `sdk-python` en lugar de `sdk-go` en el segundo programa. No queda ninguna máquina `recipe-*` en
`./bin/rc machine ls`.

## 10e. Escritorio con Claude Code por el SDK Go

Con el `serve` todavía en marcha, la máquina del `examples/desktop-claude/runner.code` (XFCE, Firefox, Claude Code),
creada y verificada por el SDK Go:

```bash
go run ./examples/desktop-claude -server http://127.0.0.1:8080 -name desk -display-port 6081
open http://127.0.0.1:6081
```

Esperado, en la salida:
- `claude  2.1.x (Claude Code)` y `navegador padrão  /usr/lib/firefox-esr/firefox-esr`;
- `atalhos  Arquivos.desktop Claude.desktop Navegador.desktop Terminal.desktop`;
- `tela responde  ... current 1200 x 700`: después de 200 movimientos de mouse, la pantalla sigue en pie y en el
  tamaño pedido;
- `terminal maximizado  _NET_WM_STATE_MAXIMIZED_HORZ`;
- `sessão de volta em 1200x700`: el programa cierra la sesión XFCE (como un logout) y vuelve sola.

En el navegador: fondo negro, los cuatro accesos directos, y la pantalla del tamaño de la ventana.

## 10f. Tareas al Claude Code de la máquina

Necesita el Claude con sesión iniciada en la máquina: el token entra por la credencial del `runner.code` (el guest solo
ve un placeholder), desde el entorno de quien hizo el start. Con el `serve` iniciado con el token en el entorno:

```bash
CLAUDE_CODE_OAUTH_TOKEN="$(pass show bassi/anthropic/claude-code-token)" ./bin/rc serve --listen 127.0.0.1:8080 &
go run ./examples/desktop-claude -server http://127.0.0.1:8080 -name desk -display-port 6081
go run ./examples/desktop-claude-agent -server http://127.0.0.1:8080 -name desk
```

Esperado: `✓ /root/agent/headless.txt`, `✓ /root/agent/gui.txt` y `✓ /root/agent/keys.txt`, cada uno con
`feito pelo modo <modo>`. Son tres formas de enviar la tarea: `claude -p` por exec, escrita en la ventana con `xdotool`,
y escrita por la API de pantalla. Las pantallas de las dos últimas quedan en `agent-gui.jpg` y `agent-keys.jpg`.

`task runbook` solo ejecuta este paso con `RB_CLAUDE=1` y el token en el entorno, porque usa la suscripción de Claude.

## 11. Limpiar

```bash
./bin/rc machine stop --name teste
./bin/rc machine delete --name teste
rm -rf /tmp/rc-up.bin /tmp/rc-down.bin /tmp/rc-demo /tmp/rc-vol
```

Esperado: `stopped teste` y `deleted teste`. `./bin/rc machine ls` vuelve vacío.

## Cuando algo salga distinto

| Síntoma | Dónde mirar |
|---------|------------|
| `image for X not built` | `./bin/rc image build --stack X` |
| `read stream: the guest in this image does not support streaming` | imagen de antes del streaming: reconstruir |
| `INVALID_STATE: machine X is not running` | `./bin/rc machine start --name X` |
| `CONFLICT: machine X already exists` | otro nombre, o `delete` antes |
| `start` falla con el final de la consola | `./bin/rc machine logs --name X` |
| GPU: `ERROR_OUT_OF_HOST_MEMORY` | [GPU › Por qué Fedora](docusaurus/docs/advanced/gpu.md) |
| máquina que no se detiene | `./bin/rc machine stop --name X --timeout 0` la mata al instante |
