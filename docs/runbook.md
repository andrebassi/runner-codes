# Runbook — testar o rc à mão, passo a passo

Sequência para conferir num terminal novo, um comando por vez, que o `rc` faz o que promete. Cada passo diz o que você
deve ver.

**Este runbook é executável.** `task runbook` (`scripts/runbook.sh`) roda exatamente estes passos e confere cada
resultado esperado. Ele reprova se algum sair diferente. Regra do projeto: **todo recurso novo ganha uma seção aqui e
o passo correspondente no `scripts/runbook.sh`, na mesma entrega.**

| Última execução | Host | Resultado |
|-----------------|------|-----------|
| 20/10/2026 | macOS 27, Apple M1 Pro | `task runbook`: 0 falhas em 116 conferências (até o 8u: usuário, segredos, pontes de socket, ping, staged, recusas) |
| 19/10/2026 | macOS 27, Apple M1 Pro | `task runbook`: 0 falhas em 50 conferências (com volumes e rede, passos 8c e 8d). Prova de falha: com o DNS do gateway sem filtro, o 8d reprovou em 2 conferências |

Pré-requisitos: `task krun:install`, `task build` e as imagens (`./bin/rc image build --all`; o passo 9 precisa da
`llama`).

> Rode com o e2e parado: os testes conferem com `pgrep` que nenhuma VM sobrou, e uma máquina sua ligada faz eles
> reprovarem.

## 0. Preparar

```bash
cd ~/works/labs/github/runner.codes
./bin/rc doctor
```

Esperado: `ok` em `platform`, `libkrun`, `gpu` e `guest`, e uma linha `ok image <stack>` por imagem construída.

## 1. Job efêmero com saída em streaming

```bash
./bin/rc run --stack python --code $'import time\nprint("early")\ntime.sleep(1.5)\nprint("late")'
```

Esperado: `early` aparece logo (~0,3 s, o boot) e `late` 1,5 s depois, enquanto o programa ainda roda. No fim,
`[status=success exit=0 ...]` no stderr. A VM é destruída ao terminar.

## 2. Criar e ligar uma máquina

```bash
./bin/rc machine create --name teste --stack python
./bin/rc machine start --name teste
./bin/rc machine ls
```

Esperado:
- `created teste (stack=python cpus=4 memory=8192MiB disk=10GiB gpu=false)`;
- `started teste (pid N) in ~200ms`;
- `teste` com `running` no `ls`.

A máquina continua ligada depois que o comando sai.

## 3. Gravar coisas em três lugares

```bash
./bin/rc machine exec --name teste -- sh -c 'echo persistiu > /etc/minha-config; echo dados > /workspace/arquivo.txt; echo some > /tmp/temporario.txt'
```

Esperado: nada na saída, e rc 0.

## 4. Parar e ligar de novo

```bash
./bin/rc machine stop --name teste
./bin/rc machine start --name teste
```

Esperado: `stopped teste` e `started teste (pid N) in ~200ms`. O stop faz `sync` do disco antes de desligar.

## 5. Conferir o que ficou

```bash
./bin/rc machine exec --name teste -- sh -c 'cat /etc/minha-config /workspace/arquivo.txt; cat /tmp/temporario.txt'
```

Esperado:

```
persistiu
dados
cat: can't open '/tmp/temporario.txt': No such file or directory
```

`/etc` (camada gravável sobre a imagem) e `/workspace` (disco da máquina) persistem; `/tmp` é tmpfs e volta vazio.

## 6. Copiar um arquivo para a máquina e de volta

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
- o mesmo sha256 dentro e fora da máquina;
- `downloaded ... in ~230ms`;
- `idênticos`.

## 7. Código de saída e streaming dentro da máquina

```bash
./bin/rc machine exec --name teste -- sh -c 'echo a; sleep 1; echo b; exit 7'; echo "rc=$?"
```

Esperado: `a`, `b` um segundo depois e `rc=7`. O rc do `rc` é o do comando.

## 7b. Terminal interativo na máquina

```bash
./bin/rc machine shell --name teste
```

Dentro do shell que abrir, digite:

```bash
test -t 0 && echo IS-TTY; stty size     # IS-TTY e o tamanho do seu terminal
sleep 30                                 # aperte Ctrl+C: interrompe só o sleep
echo ALIVE                               # o shell continua vivo
exit 4
```

Fora, `echo $?` mostra `4`. A máquina continua ligada: o shell é uma sessão, não a VM.

Esperado: terminal de verdade, com Ctrl+C, setas, histórico, `vi` e redimensionamento da janela. É `bash` se a imagem
tiver; na python (Alpine) é o `sh`.

## 8. Logs da máquina

```bash
./bin/rc machine logs --name teste --tail 10
```

Esperado: as últimas linhas do console (o guest registra cada operação). Funciona também com a máquina parada.

## 8b. Máquina declarada num `runner.code`

O [`runner.code`](docusaurus/docs/cli/runner-code.md) é a máquina declarada num arquivo: stack ou imagem, recursos, env, init e
workload num TOML versionável.

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
- o `create` sem `-f` diz `from runner.code`, e o status mostra `"cpus": 1`: a flag vence o arquivo;
- `logs --source workload` mostra `up hello in /app`: o workdir foi criado e env e workdir chegaram ao workload;
- o primeiro `exec` imprime `1`, `hello`, `/app`;
- o segundo imprime `1` e `saved`: o init **não** rodou de novo no segundo start, e o `SIGINT` do stop chegou ao
  workload, que gravou o arquivo antes do sync.

Chave que o `runner.code` ainda não implementa é recusada, nunca ignorada:

```bash
printf 'stack = "python"\n[health]\nexec = ["true"]\n' > /tmp/rc-demo/bad.code
./bin/rc machine create --name x -f /tmp/rc-demo/bad.code
```

Esperado: `key "health" is not supported yet: health checks`, e nada criado.

## 8c. Volumes: diretório do host dentro da máquina

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
- `do host`: o guest lê o diretório do host;
- `Read-only file system` e `ro=1`: o `:ro` é imposto pela libkrun no device, e o root do guest não o desfaz;
- no host, `cat` mostra `do-guest`: a escrita do guest aparece no host na hora.

## 8d. Rede: saída, política e porta

Máquina nasce **sem rede**. `--net` liga a saída; `--allow-host` liga e restringe; `-p` publica uma porta no
`127.0.0.1` do host.

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
- `wget: bad address 'iana.org'` **na hora**: o DNS do gateway responde NXDOMAIN, sem esperar timeout;
- `meta=1`: a metadata de nuvem (`169.254.169.254`) é recusada mesmo com a rede aberta;
- depois do `update`, `iana.org` passa **sem reiniciar** a máquina;
- `curl` no host recebe `do-guest` pela porta publicada;
- o log de recusas tem `deny dns iana.org: name not in the allow list`.

## 8e. Máquina efêmera: `machine run`

```bash
./bin/rc machine run --stack python -- python3 -c 'print("ola"); raise SystemExit(3)'; echo "rc=$?"
echo dados | ./bin/rc machine run -i --stack python -- sh -c 'read x; echo leu:$x'
./bin/rc machine run --net --image alpine -- uname -a
./bin/rc machine run -it --image alpine -- /bin/sh        # terminal; `exit` sai e apaga a máquina
./bin/rc machine ls
```

Esperado:
- `ola` e `rc=3`: o rc é o do comando;
- `leu:dados`: o `-i` repassa o stdin (sem `-i` o stdin não é lido);
- o `uname -a` do kernel da libkrunfw numa imagem fora do catálogo (construída no primeiro uso);
- o `ls` sem nenhuma `run-…`: a máquina efêmera some no fim, inclusive no Ctrl+C.

## 8f. Porta publicada alcança servidor que só escuta em localhost

```bash
./bin/rc machine run -d --name web --stack python -p 18080:9000 -- python3 -m http.server 9000 --bind 127.0.0.1
curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:18080/
./bin/rc machine delete --name web -f
```

Esperado: `200`. A porta publicada entra pelo guest (op `dial` no vsock) e disca o `127.0.0.1` de lá, então o
servidor que escuta só em localhost, como o vite ou o openclaw, responde.

## 8g. Tela no navegador: display, teclado e mouse

```bash
./bin/rc machine create --name tela --image alpine:latest --gpu --net --display 1024x768
./bin/rc machine start --name tela
./bin/rc machine exec --name tela -- sh -c 'apk add -q --no-cache libdrm-tests evtest; cat /sys/class/input/event*/device/name'
./bin/rc machine exec --name tela -- sh -c 'c=$(modetest -M virtio_gpu -c | awk "/connected/ {print \$1; exit}"); (modetest -M virtio_gpu -s $c:1024x768 >/dev/null 2>&1 </dev/null &)'
open http://localhost:6080
./bin/rc machine exec --name tela -- sh -c '(timeout 25 evtest /dev/input/event0 > /tmp/k.log 2>&1 </dev/null &)'
# clique na página e digite algumas letras; depois:
./bin/rc machine exec --name tela -- grep 'value 1' /tmp/k.log
./bin/rc machine delete --name tela -f
```

Esperado:
- o guest lista `runner.code keyboard` e `runner.code tablet`;
- a página mostra as barras de cor do `modetest`, ao vivo, em 1024x768;
- cada tecla digitada na página aparece no `evtest` do guest (`KEY_R`, `KEY_C`...).

O `task runbook` confere o mesmo caminho sem precisar de olho humano: o quadro de 1024x768 chega ao visualizador e
um `KEY_A` mandado por `POST /input` aparece no `evtest`.

## 8h. Tela responsiva e controle de tela

A mesma máquina com tela do 8g, agora controlada pela CLI, pela API e pelo MCP, e com a resolução pedida pela página:

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
- `900 600` no `/tmp/resize.txt`: o tamanho que a página pede chega ao gancho `/etc/rc/display-resize` do guest (no
  desktop, ele roda `xrandr`; sem o gancho, nada acontece);
- `screen 1024x768 -> tela.jpg`, um JPEG da tela;
- no `evtest`: `KEY_LEFTCTRL` e `KEY_C` apertados, `ABS_X` em `32767` (o canto direito) e `BTN_LEFT`;
- a página do visualizador desenha uma seta laranja onde a CLI clicou (o `GET /pointer` registra o `click`);
- a página da tela traz `runner.codes` na barra (o `display_title` do `runner.code` troca o nome);
- `key hyper` sai com erro (`unknown key "hyper"`), sem mandar nada à máquina;
- o `rc mcp` responde ao `initialize` e lista 9 ferramentas: `screenshot`, `click`, `move`, `drag`, `type`, `key`,
  `scroll`, `actions` e `exec`.

O `task runbook` faz o mesmo e confere também a API: `GET /api/v1/machines/:name/screen` com `X-Screen-Width: 1024`,
400 para ação inválida e 204 para válida em `POST .../screen/actions`.

## 8i. Branch e pausa

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
- `status` mostra `paused`, e um `exec` nesse estado é recusado com o motivo;
- a filha mostra `antes`: o branch leva o que a mãe gravou, mesmo com ela pausada (o `rc` a retoma por um instante
  para o `sync`);
- depois disso, mãe e filha são independentes.

## 8j. Rede nomeada

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
- as duas máquinas recebem IPs seguidos da rede (`db` é `100.98.0.10`, `app` é `100.98.0.11`) e se alcançam pelo IP
  e pelo nome `<máquina>.<rede>.internal`, sem `--net`: tráfego dentro da rede não é saída;
- máquina de outra rede não alcança essas, nem resolve os nomes delas;
- `db` apagada some do DNS da rede: o `nslookup` não devolve mais o IP dela.

## 8k. Volume S3

Um S3 local, servindo uma pasta (o MinIO saiu do nixpkgs em 11/10/2026):

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
- o `start` monta o bucket (com chave errada, ele falha com `s3: 403` e o motivo);
- o guest lê `ola do bucket`, e o `novo.txt` aparece na pasta do servidor com `do-guest`.

## 8l. Credencial que a máquina usa sem ler

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
- `guest: RC_PLACEHOLDER_TOK_...`: o guest só tem o placeholder;
- `server: Bearer valor-do-runbook`: o servidor recebeu o valor real, que veio do ambiente do `start`.

O User-Agent é próprio porque o Cloudflare do postman-echo recusa o `Python-urllib` (403, error code 1010).

## 8m. Pack: a máquina num arquivo

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
- `marca-do-runbook`: o artefato levou o disco da máquina, mesmo com a origem apagada (um pack que perdesse o disco
  ainda ligaria e sairia 0; por isso se confere um valor);
- `/tmp/one` sozinho mostra `usage: one run ...`.

## 8n. Usuário, volume de disco e `docker.sock`

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
- `nobody`, e `root` com `--user root` (o `--user` do exec vence o da máquina);
- `ext2/ext3`: o volume de disco é ext4 de verdade, fora do overlay (o que o overlay2 do Docker precisa);
- `do-guest:ping`: o host fala com o servidor do guest pelo `docker.sock` do diretório da máquina.

## 8o. Segredos e agente SSH

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
- `valor-secreto do-arquivo`: os dois segredos no ambiente de todo comando;
- `ssh-add -l` lista a chave de `/tmp/rb-id` (a privada fica no agente do host);
- o `grep` não acha nada: o valor não fica no registro da máquina.

O agente e a chave são descartáveis, deste passo: nada toca no agente de quem roda.

## 8p. Pontes de socket nas duas direções

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

Esperado: `do-guest:ping` (o host alcança o socket que o workload escuta) e `do-host:pong` (o guest alcança o
socket do host, num diretório que a imagem não tinha).

## 8q. Ping e faixa de portas

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
- `200`: a faixa publica uma porta por número (até 64), e a do meio responde;
- o ping a `1.1.1.1` volta com a rede aberta, e só se o host recebeu a resposta (não há resposta inventada);
- fora da allowlist o ping não volta, e a negação `deny icmp 1.1.1.1` vai para o log.

## 8r. Volume staged e sync

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
- `ext2/ext3`: a cópia está no disco da máquina, sem ida e volta de virtio-fs;
- antes do sync o host continua `original`;
- depois: `mudou`, sem `old.txt` (o sync espelha: o que sumiu da cópia some do host) e `run.sh` com `-rwxr-xr-x`.

O stop gracioso e o fim do `machine run` também sincronizam; `--timeout 0` não. A filha de um branch não devolve nada
ao host.

## 8s. Recusas que dizem o motivo

```bash
./bin/rc doctor | grep -E 'cuda|nested'
./bin/rc machine create --name x --image alpine --cuda
time ./bin/rc machine run --nested --image alpine -- ls -l /dev/kvm
```

Esperado:
- `info  cuda: not supported (no CUDA remoting; --gpu is Vulkan)` e a linha do nested deste host;
- `--cuda is not supported: rc has no CUDA remoting ...`, sem criar máquina;
- num host que não aninha (M1/M2), `failed to start: Error: nested virtualization is not supported on this host ...`
  em menos de 1 s (antes: 30 s e `did not become ready`); num host que aninha, `/dev/kvm` dentro da VM.

## 8t. Diretório da máquina e limpeza do cache

```bash
ls "$(./bin/rc machine data-dir --name usr)"
./bin/rc image prune
./bin/rc pack prune
```

Esperado: `machine.json` no diretório da máquina; os dois prunes saem 0 (sem `--all`, o `image prune` só apaga
o ext4 para o qual nenhum nome aponta mais, o build anterior de uma imagem, e o `pack prune` guarda os 5 usados mais recentemente).

## 8u. Os guias

```bash
task guides DIRS=gpu-cuda      # uma pasta; sem DIRS, todas (cada uma liga as suas VMs)
```

Esperado: `falhas: 0` em cada pasta. O que cada pasta prova está em `examples/guides/README.md`.

## 8v. Terminal web

```bash
./bin/rc machine web-terminal --name demo                 # abre o navegador com o shell da máquina
./bin/rc machine web-terminal --name demo --no-open -- sh -c 'echo wt-ok:$((6*7))'
```

Esperado:
- `web terminal: http://127.0.0.1:PORTA/?t=<32 hexa>`: só loopback, com token; o navegador abre sozinho (`--no-open`
  só imprime);
- no navegador, um terminal de verdade (xterm.js): Ctrl+C, setas, editor de tela cheia e o tamanho da janela chegam
  ao guest, e um F5 mostra de novo a tela recente;
- com o comando, a tela mostra `wt-ok:42`, a página avisa que o programa saiu e o `rc` sai com o código dele;
- `/stream` sem o token responde 403, e um `Host` que não seja de loopback também (DNS rebinding).
- abas `claude` (o comando), `shell` (um terminal à parte, no 1º clique), `eventos` e, por último, uma aba por porta: um servidor que o programa sobe na VM (qualquer porta) aparece com a app num iframe e responde no `127.0.0.1` do host.

Para abrir o Claude Code no navegador assim:
`rc machine web-terminal --name agent -- claude --dangerously-skip-permissions`.

## 8w. Playground público (claude-in-chrome.runner.codes)

Cada visitante ganha uma Fly Machine própria (o Fly não expõe `/dev/kvm`, então a microVM é a do Fly), com o Claude
Code no terminal web e as portas em `https://<porta>-<id>.runner.codes/`. Teto de 3 máquinas; parada há 30 min
desliga, sem uso há 24 h é apagada.

```bash
scripts/playground-cloudflare.sh     # túnel claude-in-chrome, ingress e CNAMEs (idempotente)
scripts/playground-fly.sh setup      # apps runner-codes-playground e runner-codes-visitors, rede própria, secrets
scripts/playground-image.sh all      # imagens visitor e front pelo Nix no builder do Fly
scripts/playground-fly.sh deploy     # publica o front
scripts/playground-token-check.sh    # o token do front só alcança o app de visitantes
curl -s https://claude-in-chrome.runner.codes/ | grep -c FingerprintJS.load
```

Esperado:
- a página abre, calcula o visitorId do navegador e leva ao terminal `/v/<id>/?t=<token>` com o Claude Code;
- `/api/session` com visitante inválido responde 400; com 3 máquinas em uso, 503 "playground is full";
- `https://3000-0000000000.runner.codes/` (máquina que não existe) responde 502, e `docs.runner.codes` continua 200;
- `scripts/playground-token-check.sh`: 200 no app de visitantes, 4xx no site e no próprio front;
- dentro da máquina do visitante, abrir conexão para a rede privada do Fly (`_api.internal`) falha, e DNS e internet
  funcionam (`scripts/fly-firewall-probe.sh` mede o mesmo firewall numa máquina descartável).

## 8x. Runtime de Kubernetes (k3s e microk8s)

Cada pod com `runtimeClassName: rc` vira uma microVM da libkrun, com kernel próprio: o containerd chama o
`containerd-shim-rc-v2`, o container pause liga a VM e os containers da app rodam dentro dela. A VM assume o IP que o
CNI deu ao pod (TAP no netns do pod com `tc mirred`). Precisa de nó Linux com `/dev/kvm`; os de teste são droplets da
DigitalOcean (`task k8s:kvm-probe` mede).

```bash
K8S_NODE=k3s task k8s:node -- create && K8S_NODE=k3s task k8s:node -- provision   # idem com K8S_NODE=microk8s
# no nó: curl -sfL https://get.k3s.io | sh -   ou   snap install microk8s --classic
task k8s:e2e -- k3s        # leva a árvore, compila, roda deploy/k3s/install.sh e deploy/k8s/e2e.sh k3s
task k8s:e2e -- microk8s   # o mesmo com deploy/microk8s/install.sh
task k8s:defects           # planta 20 defeitos no código do runtime e confere que os testes pegam
```

Esperado (19 checagens do `deploy/k8s/e2e.sh`, `falhas: 0` nos dois):
- `rc-hello` fica Ready e o log mostra `RC_K8S_OK`, o kernel da VM (6.12.91, não o do nó) e o IP do pod na `eth0`;
- `kubectl exec` sem TTY, com TTY e com stdin; o código de saída do exec (7) e do container (3) voltam certos;
- DNS do cluster, pod rc ↔ pod runc nos dois sentidos e Service ClusterIP até o pod rc;
- dois containers no mesmo pod: mesma VM (mesmo `boot_id`) e mesmo emptyDir;
- o resumo de estatísticas do kubelet responde (o uso da VM vai para o primeiro container);
- depois do delete, nenhum diretório em `/var/lib/rc-shim/pods` e nenhum processo `rc vmm` sobra.

Limites declarados: port-forward, hostNetwork, hostIPC e propagação de montagem não atravessam a VM; os volumes são
copiados para o share do pod na criação do container (o que o kubelet trocar depois, como token de serviceaccount,
não chega); limites de CPU/memória valem para a VM inteira (anotações `runner.codes/cpus` e `runner.codes/memory-mib`
do pod); o vmm do pod roda sem Landlock/seccomp.

## 9. GPU

Só se a imagem `llama` estiver construída (o `rc doctor` mostra `ok image llama`).

```bash
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'
./bin/rc run --stack llama --gpu --timeout 120s --code 'llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -ngl 99 -dev Vulkan0'
```

Esperado:
- `deviceName = Virtio-GPU Venus (Apple M1 Pro)`, e também uma linha do `llvmpipe` (Vulkan em software);
- a tabela do `llama-bench` com `Vulkan0`, `pp128` na faixa de 900–1.365 t/s e `tg32` de ~57 t/s.

Detalhes em [GPU](docusaurus/docs/advanced/gpu.md).

## 10. API HTTP

Num terminal:

```bash
./bin/rc serve --pool-size 0
```

Em outro:

```bash
curl -s -X POST localhost:8080/api/v1/machines/teste/exec -H 'content-type: application/json' -d '{"command":["python3","-c","print(6*7)"]}'
curl -s localhost:8080/api/v1/machines | head -c 300; echo
```

Esperado:
- `{"exitCode":0,"stdout":"42\n","stderr":"",...}`;
- a lista com `"name":"teste"` e `"state":"running"`.

Os corpos alternativos e o stream por linha, no mesmo servidor:

```bash
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t2","image":"alpine","allowedHost":["a.com"]}'
curl -s -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t3","image":"alpine","allowedHosts":["api.example.com"],"memoryMb":512}'
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t4","image":"alpine","nestedVirt":true}'
curl -sN -X POST localhost:8080/api/v1/machines/teste/exec/stream -H 'content-type: application/json' -d '{"command":["sh","-c","echo a; echo b"]}'
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines/teste/sync
```

Esperado:
- `400`: campo desconhecido (`allowedHost`, com erro de digitação) é recusado, e não ignorado com a saída aberta;
- `"allow_hosts":["api.example.com"]`, `"enabled":true` e `"memory_mib":512`: os nomes alternativos valem;
- `400` no `nestedVirt` (só passa com `rc serve --allow-nested-virt`);
- `event: stdout` / `data: a`, depois `data: b`: uma linha por evento, texto cru, e o `event: exit` no fim;
- `204` no `sync` (máquina sem volume staged: nada a fazer).

`Ctrl+C` encerra o serve. A máquina continua ligada, porque é destacada.

## 10b. SDKs Go e Python

Com o `serve` ainda no ar:

```bash
go run ./examples/sdk-go http://127.0.0.1:8080
PYTHONPATH=sdk/python python3 examples/sdk-python/main.py http://127.0.0.1:8080
```

Esperado, nos dois:
- `job: 42` (job efêmero);
- `machine: sdk-... running` e `exec: ola do sdk go` (ou `python`): máquina criada, arquivo gravado e executado;
- `error: NOT_FOUND`: a recusa da API chega como erro tipado, com o código estável;
- nenhuma máquina `sdk-*` sobra em `./bin/rc machine ls`.

## 10c. SDK Go num projeto de fora (go get)

O módulo Go se chama `runner.codes`, e `https://runner.codes` responde ao `go get` com a meta `go-import` apontando
para o GitHub. Num diretório fora do repositório, com o `serve` ainda no ar:

```bash
curl -s 'https://runner.codes/pkg/client?go-get=1'
mkdir /tmp/sdkcheck && cp examples/sdk-go/main.go /tmp/sdkcheck/ && cd /tmp/sdkcheck
go mod init sdkcheck
go get runner.codes/pkg/client@latest
go run . http://127.0.0.1:8080
```

Esperado:
- o `curl` devolve `<meta name="go-import" content="runner.codes git https://github.com/andrebassi/runner-codes">`;
- o `go get` baixa `runner.codes v1.1.0` (ou mais nova);
- a saída do `go run` é a mesma do 10b: `job: 42`, `exec: ola do sdk go` e `error: NOT_FOUND`.

O mesmo para o Python, num venv limpo, instalando pelo GitHub:

```bash
python3 -m venv /tmp/sdkcheck/venv
/tmp/sdkcheck/venv/bin/pip install "git+https://github.com/andrebassi/runner-codes@v1.2.0#subdirectory=sdk/python"
/tmp/sdkcheck/venv/bin/python -I <repositório>/examples/sdk-python/main.py http://127.0.0.1:8080
```

Esperado: o `pip` instala `runner-code 1.1.1`, e a saída é a mesma do 10b (`job: 42`, `exec: ola do sdk python`,
`error: NOT_FOUND`). O `-I` garante que o pacote vem do venv, não do repositório.

## 10d. Receitas dos SDKs

As sete receitas da [página de SDKs](docusaurus/docs/api-reference/sdks.md), em Go e em Python, com o `serve` ainda no ar:

```bash
go run ./examples/sdk-go-recipes http://127.0.0.1:8080
PYTHONPATH=sdk/python python3 examples/sdk-python-recipes/main.py http://127.0.0.1:8080
```

Esperado, nos dois (o tempo varia):

```
untrusted: stdout=45 stderr=aviso tempo=58ms
files: 42
timeout: exception=Timeout
compile: exception=Compilation failed stderr_tem_erro=true
machine: sdk-go pypi=200 example=bloqueado
branch: feito antes do branch
errors: 404 NOT_FOUND
```

Na linha `machine`, `sdk-python` no lugar de `sdk-go` no segundo programa. Nenhuma máquina `recipe-*` sobra em
`./bin/rc machine ls`.

## 10e. Desktop com Claude Code pelo SDK Go

Com o `serve` ainda no ar, a máquina do `examples/desktop-claude/runner.code` (XFCE, Firefox, Claude Code), criada e
conferida pelo SDK Go:

```bash
go run ./examples/desktop-claude -server http://127.0.0.1:8080 -name desk -display-port 6081
open http://127.0.0.1:6081
```

Esperado, na saída:
- `claude  2.1.x (Claude Code)` e `navegador padrão  /usr/lib/firefox-esr/firefox-esr`;
- `atalhos  Arquivos.desktop Claude.desktop Navegador.desktop Terminal.desktop`;
- `tela responde  ... current 1200 x 700`: depois de 200 movimentos de mouse, a tela segue de pé e no tamanho pedido;
- `terminal maximizado  _NET_WM_STATE_MAXIMIZED_HORZ`;
- `sessão de volta em 1200x700`: o programa encerra a sessão XFCE (como um logout) e ela volta sozinha.

No navegador: fundo preto, os quatro atalhos, e a tela no tamanho da janela.

## 10f. Tarefas ao Claude Code da máquina

Precisa do Claude logado na máquina: o token entra pela credencial do `runner.code` (o guest só vê um placeholder), a
partir do ambiente de quem deu o start. Com o `serve` iniciado com o token no ambiente:

```bash
CLAUDE_CODE_OAUTH_TOKEN="$(pass show bassi/anthropic/claude-code-token)" ./bin/rc serve --listen 127.0.0.1:8080 &
go run ./examples/desktop-claude -server http://127.0.0.1:8080 -name desk -display-port 6081
go run ./examples/desktop-claude-agent -server http://127.0.0.1:8080 -name desk
```

Esperado: `✓ /root/agent/headless.txt`, `✓ /root/agent/gui.txt` e `✓ /root/agent/keys.txt`, cada um com
`feito pelo modo <modo>`. São três jeitos de mandar a tarefa: `claude -p` por exec, digitada na janela com `xdotool`, e
digitada pela API de tela. As telas dos dois últimos ficam em `agent-gui.jpg` e `agent-keys.jpg`.

O `task runbook` só roda este passo com `RB_CLAUDE=1` e o token no ambiente, porque ele usa a assinatura do Claude.

## 11. Limpar

```bash
./bin/rc machine stop --name teste
./bin/rc machine delete --name teste
rm -rf /tmp/rc-up.bin /tmp/rc-down.bin /tmp/rc-demo /tmp/rc-vol
```

Esperado: `stopped teste` e `deleted teste`. `./bin/rc machine ls` volta vazio.

## Quando algo sair diferente

| Sintoma | Onde olhar |
|---------|------------|
| `image for X not built` | `./bin/rc image build --stack X` |
| `read stream: the guest in this image does not support streaming` | imagem de antes do streaming: reconstruir |
| `INVALID_STATE: machine X is not running` | `./bin/rc machine start --name X` |
| `CONFLICT: machine X already exists` | outro nome, ou `delete` antes |
| `start` falha com o fim do console | `./bin/rc machine logs --name X` |
| GPU: `ERROR_OUT_OF_HOST_MEMORY` | [GPU › Por que Fedora](docusaurus/docs/advanced/gpu.md) |
| máquina que não para | `./bin/rc machine stop --name X --timeout 0` mata na hora |
