#!/usr/bin/env bash
# Executa o docs/runbook.md de ponta a ponta e confere o resultado esperado de cada passo.
# Existe para o runbook não mentir: comando que entra lá entra aqui também, e `task runbook`
# reprova se algum passo sair diferente do que o runbook promete.
# Uso: scripts/runbook.sh   (precisa de `task build` e das imagens python e, para o passo 9, llama)
set -uo pipefail
cd "$(dirname "$0")/.."
rc=./bin/rc
name=runbook-$$
port=18190
fails=0
tmp=$(mktemp -d)
serve_pid=""
s3_pid=""
agent_pid=""
hsrv_pid=""

# cleanup roda até no Ctrl+C: máquina e serve do runbook não podem sobrar.
cleanup() {
  [ -n "$serve_pid" ] && kill "$serve_pid" 2>/dev/null
  # O agente SSH e o servidor de socket são deste runbook (descartáveis), nunca os de quem roda.
  [ -n "$agent_pid" ] && kill "$agent_pid" 2>/dev/null
  [ -n "$hsrv_pid" ] && kill "$hsrv_pid" 2>/dev/null
  # O rclone roda como filho do nix shell: o grupo inteiro sai junto.
  [ -n "$s3_pid" ] && kill -- -"$s3_pid" 2>/dev/null
  $rc machine delete --name "$name" -f >/dev/null 2>&1
  $rc machine delete --name "$name-rc" -f >/dev/null 2>&1
  $rc machine delete --name "$name-vol" -f >/dev/null 2>&1
  $rc machine delete --name "$name-net" -f >/dev/null 2>&1
  $rc machine delete --name "$name-disp" -f >/dev/null 2>&1
  for ex in py node doom mom kid db app s3 cred pk one usr dk sec skt png pngd stg cu t2 t3 t4; do $rc machine delete --name "$name-$ex" -f >/dev/null 2>&1; done
  rm -rf "$tmp"
}
trap cleanup EXIT

# step imprime o título do passo, como no runbook.
step() { printf '\n== %s\n' "$*"; }

# expect confere uma condição e conta a falha sem abortar: o relatório mostra todos os passos.
expect() {
  local what=$1; shift
  if "$@"; then echo "   ok    $what"; else echo "   FAIL  $what"; fails=$((fails + 1)); fi
}

step "0. Preparar"
out=$($rc doctor 2>&1)
expect "doctor: libkrun e guest ok" grep -q "ok    guest" <<<"$out"

step "1. Job efêmero com saída em streaming"
# Mede a chegada de cada linha: a 2ª tem de vir ≥ 1 s depois da 1ª.
lines=$($rc run --stack python --code $'import time\nprint("early")\ntime.sleep(1.5)\nprint("late")' 2>/dev/null |
  python3 -u -c 'import sys,time
t0=time.time()
for l in sys.stdin: print(f"{time.time()-t0:.2f} {l.strip()}")')
early=$(awk '/early/{print $1}' <<<"$lines"); late=$(awk '/late/{print $1}' <<<"$lines")
expect "early chega ≥ 1 s antes de late ($early s / $late s)" python3 -c "import sys; sys.exit(0 if float('$late')-float('$early')>=1.0 else 1)"

step "2. Criar e ligar uma máquina"
expect "create" $rc machine create --name "$name" --stack python --disk-gib 2
expect "start" $rc machine start --name "$name"
expect "ls mostra running" sh -c "$rc machine ls | grep -q '$name .*running'"

step "3. Gravar coisas em três lugares"
expect "gravar /etc, /workspace e /tmp" $rc machine exec --name "$name" -- sh -c \
  'echo persistiu > /etc/minha-config; echo dados > /workspace/arquivo.txt; echo some > /tmp/temporario.txt'

step "4. Parar e ligar de novo"
expect "stop" $rc machine stop --name "$name"
expect "start" $rc machine start --name "$name"

step "5. Conferir o que ficou"
out=$($rc machine exec --name "$name" -- sh -c 'cat /etc/minha-config /workspace/arquivo.txt; cat /tmp/temporario.txt 2>&1' 2>&1)
expect "/etc e /workspace persistiram" grep -q $'persistiu' <<<"$out"
expect "/workspace persistiu" grep -q 'dados' <<<"$out"
expect "/tmp voltou vazio" grep -q 'No such file' <<<"$out"

step "6. Copiar um arquivo para a máquina e de volta"
head -c 20000000 /dev/urandom > "$tmp/up.bin"
expect "cp para a máquina" $rc machine cp "$tmp/up.bin" "$name:/workspace/up.bin"
inside=$($rc machine exec --name "$name" -- sha256sum /workspace/up.bin | cut -d' ' -f1)
outside=$(shasum -a 256 "$tmp/up.bin" | cut -d' ' -f1)
expect "sha256 igual dentro e fora" test "$inside" = "$outside"
expect "cp da máquina" $rc machine cp "$name:/workspace/up.bin" "$tmp/down.bin"
expect "ida e volta idênticas" cmp -s "$tmp/up.bin" "$tmp/down.bin"

step "7. Código de saída e streaming dentro da máquina"
$rc machine exec --name "$name" -- sh -c 'echo a; sleep 1; echo b; exit 7' >/dev/null
code=$?
expect "rc do comando repassado (7)" test "$code" = 7

step "7b. Terminal interativo na máquina"
# Um pseudo-terminal de verdade (python pty) faz o papel de quem digita: confere TTY, o
# tamanho da janela, Ctrl+C no sleep e o rc do exit.
out=$(RC="$rc" NAME="$name" python3 - <<'PY'
import os, pty, time, select, struct, fcntl, termios
pid, fd = pty.fork()
if pid == 0:
    os.execv(os.environ["RC"], ["rc", "machine", "shell", "--name", os.environ["NAME"]])
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 33, 111, 0, 0))
buf = b""
def read(seconds):
    global buf
    end = time.time() + seconds
    while time.time() < end:
        if select.select([fd], [], [], 0.1)[0]:
            try: buf += os.read(fd, 4096)
            except OSError: return
read(2)
for keys, wait in ((b"test -t 0 && echo IS-T''TY; stty size\r", 1.5), (b"sleep 30\r", 1), (b"\x03", 1),
                   (b"echo ALIVE-$((40+2))\r", 1.5), (b"exit 4\r", 2)):
    os.write(fd, keys); read(wait)
_, st = os.waitpid(pid, 0)
text = buf.decode(errors="replace")
print("tty" if "IS-TTY" in text else "", "size" if "33 111" in text else "", "alive" if "ALIVE-42" in text else "", "rc=%d" % os.WEXITSTATUS(st))
PY
)
expect "shell enxerga um terminal" grep -q "tty" <<<"$out"
expect "tamanho da janela chega (33x111)" grep -q "size" <<<"$out"
expect "Ctrl+C interrompe só o comando" grep -q "alive" <<<"$out"
expect "rc do exit repassado (4)" grep -q "rc=4" <<<"$out"

step "8. Logs da máquina"
expect "logs não vazios" sh -c "$rc machine logs --name '$name' --tail 10 | grep -q ."

step "8b. Máquina declarada num runner.code"
# Mesmo arquivo do runbook: init uma vez por disco, workload com SIGINT no stop, flag vencendo o arquivo.
mkdir -p "$tmp/rc" && cat >"$tmp/rc/runner.code" <<'TOML'
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
out=$(cd "$tmp/rc" && "$OLDPWD/$rc" machine create --name "$name-rc" --cpus 1 2>&1)
expect "create lê ./runner.code sozinho" grep -q "from runner.code" <<<"$out"
expect "--cpus vence o arquivo" sh -c "$rc machine status --name '$name-rc' --json | grep -Eq '\"cpus\": ?1,'"
expect "start roda init e workload" $rc machine start --name "$name-rc"
sleep 1
out=$($rc machine logs --name "$name-rc" --source workload)
expect "logs --source workload: env e workdir chegaram" grep -q "up hello in /app" <<<"$out"
out=$($rc machine exec --name "$name-rc" -- sh -c 'cat count; echo $GREETING; pwd')
expect "exec herda env e workdir; init rodou 1 vez" test "$out" = $'1\nhello\n/app'
expect "stop manda SIGINT ao workload" $rc machine stop --name "$name-rc"
$rc machine start --name "$name-rc" >/dev/null
out=$($rc machine exec --name "$name-rc" -- sh -c 'cat count saved')
expect "init não roda de novo; o trap gravou antes do sync" test "$out" = $'1\nsaved'
expect "stop" $rc machine stop --name "$name-rc"
printf 'stack = "python"\n[health]\nexec = ["true"]\n' >"$tmp/rc/bad.code"
out=$($rc machine create --name "$name-bad" -f "$tmp/rc/bad.code" 2>&1)
expect "chave ainda não suportada recusada com o motivo" grep -q '"health" is not supported yet' <<<"$out"

step "8c. Volumes: diretório do host dentro da máquina"
mkdir -p "$tmp/vol/ro" && echo "do host" >"$tmp/vol/hello.txt" && echo x >"$tmp/vol/ro/x"
$rc machine create --name "$name-vol" --stack python -v "$tmp/vol:/data" -v "$tmp/vol/ro:/ro:ro" -w /data >/dev/null
expect "start monta os volumes" $rc machine start --name "$name-vol"
out=$($rc machine exec --name "$name-vol" -- sh -c 'cat hello.txt; echo do-guest > from-guest.txt; echo y > /ro/y 2>/dev/null; echo ro=$?')
expect "guest lê o arquivo do host" grep -q "do host" <<<"$out"
expect "volume :ro recusa escrita" grep -q "ro=1" <<<"$out"
expect "o que o guest grava aparece no host" grep -q "do-guest" "$tmp/vol/from-guest.txt"
expect "stop" $rc machine stop --name "$name-vol"

step "8d. Rede: saída, política e porta"
port=$((18200 + RANDOM % 500))
$rc machine create --name "$name-net" --stack python --allow-host example.com -p "$port:8000" >/dev/null
expect "start com rede" $rc machine start --name "$name-net"
out=$($rc machine exec --name "$name-net" -- sh -c 'timeout 20 wget -qO /dev/null -T 15 http://example.com; echo ok=$?; timeout 20 wget -qO /dev/null -T 15 http://iana.org 2>&1; timeout 5 wget -qO- -T 3 http://169.254.169.254 2>&1; echo meta=$?')
expect "host permitido responde" grep -q "ok=0" <<<"$out"
expect "host fora da lista: bad address" grep -q "bad address 'iana.org'" <<<"$out"
expect "metadata de nuvem recusada" grep -q "meta=1" <<<"$out"
expect "update ao vivo libera iana.org" $rc machine update --name "$name-net" --allow-host iana.org
out=$($rc machine exec --name "$name-net" -- sh -c 'timeout 20 wget -qO /dev/null -T 15 http://iana.org; echo ok=$?')
expect "iana.org passa sem reiniciar" grep -q "ok=0" <<<"$out"
$rc machine exec --name "$name-net" -- sh -c 'cd /tmp && echo do-guest > i.html && (python3 -m http.server 8000 >/dev/null 2>&1 &) && sleep 1' >/dev/null
expect "porta publicada serve o guest no host" sh -c "curl -s --max-time 5 http://127.0.0.1:$port/i.html | grep -q do-guest"
# Mesmo diretório de cache que o rc calcula: $RUNNER_CACHE_DIR, senão o cache do usuário.
if [ "$(uname)" = Darwin ]; then cache=${RUNNER_CACHE_DIR:-$HOME/Library/Caches/runner-codes}; else cache=${RUNNER_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/runner-codes}; fi
expect "log de recusas" grep -q "deny dns iana.org" "$cache/machines/$name-net/egress-denials.log"
expect "stop" $rc machine stop --name "$name-net"

step "8e. Máquina efêmera: machine run"
out=$($rc machine run --stack python -- python3 -c 'print("ola"); raise SystemExit(3)' </dev/null 2>&1); code=$?
expect "rc do comando repassado (3)" test "$code" = 3
expect "saída do comando" grep -q "ola" <<<"$out"
out=$(echo dados | $rc machine run -i --stack python -- sh -c 'read x; echo leu:$x' 2>&1)
expect "-i repassa o stdin" grep -q "leu:dados" <<<"$out"
expect "nenhuma máquina efêmera sobra" sh -c "! $rc machine ls | grep -q run-"
out=$($rc machine run --net --image alpine:latest -- cat /etc/alpine-release 2>/dev/null)
expect "imagem fora do catálogo (alpine) roda" grep -qE '^3\.' <<<"$out"

step "8f. Porta publicada alcança servidor que só escuta em localhost"
port=$((18700 + RANDOM % 200))
$rc machine run -d --name "$name-py" --stack python -p "$port:9000" -- python3 -m http.server 9000 --bind 127.0.0.1 >/dev/null
expect "servidor em 127.0.0.1 do guest responde pela porta do host" sh -c "for i in 1 2 3 4 5 6 7 8 9 10; do curl -s --max-time 2 -o /dev/null -w '%{http_code}' http://127.0.0.1:$port/ | grep -q 200 && exit 0; sleep 1; done; exit 1"
$rc machine delete --name "$name-py" -f >/dev/null

step "8g. Tela no navegador: display, teclado e mouse"
dport=$((18900 + RANDOM % 100))
$rc machine create --name "$name-disp" --image alpine:latest --gpu --net --display 1024x768 --display-port "$dport" >/dev/null 2>&1
expect "start com tela" $rc machine start --name "$name-disp"
out=$($rc machine exec --name "$name-disp" -- sh -c 'apk add -q --no-cache libdrm-tests evtest >/dev/null 2>&1; cat /sys/class/input/event*/device/name')
expect "teclado e tablet do rc no guest" sh -c "grep -q 'runner.code keyboard' <<<'$out' && grep -q 'runner.code tablet' <<<'$out'"
$rc machine exec --name "$name-disp" -- sh -c 'c=$(modetest -M virtio_gpu -c | awk "/connected/ {print \$1; exit}"); (sleep 60 | modetest -M virtio_gpu -s $c:1024x768 >/dev/null 2>&1 &); sleep 2' >/dev/null
expect "quadro 1024x768 no visualizador" sh -c "curl -s --max-time 10 -o '$tmp/frame.jpg' 'http://127.0.0.1:$dport/frame?after=0' && file '$tmp/frame.jpg' | grep -q '1024x768'"
($rc machine exec --name "$name-disp" -- sh -c '(timeout 5 evtest /dev/input/event0 > /tmp/k.log 2>&1 &); sleep 6; cat /tmp/k.log' > "$tmp/keys.txt" 2>&1 &)
sleep 2
curl -s -X POST "http://127.0.0.1:$dport/input" -d '[{"type":1,"code":30,"value":1},{"type":0,"code":0,"value":0},{"type":1,"code":30,"value":0},{"type":0,"code":0,"value":0}]' >/dev/null
sleep 6
expect "tecla do navegador chega ao guest (KEY_A desce e sobe)" sh -c "grep -q 'KEY_A), value 1' '$tmp/keys.txt' && grep -q 'KEY_A), value 0' '$tmp/keys.txt'"
expect "stop" $rc machine stop --name "$name-disp"

step "8h. Exemplos (examples/)"
out=$($rc machine run -s examples/python-app/runner.code -- python3 --version 2>/dev/null)
expect "python-app: python3 --version" grep -q '^Python 3\.12' <<<"$out"
out=$($rc machine run -s examples/node-app/runner.code -- node -e 'console.log("ola", 1+1)' 2>/dev/null)
expect "node-app: node -e" grep -q 'ola 2' <<<"$out"
$rc machine run -d --name "$name-doom" -s examples/doom-web/runner.code >/dev/null 2>&1
expect "doom-web: index.html servido do volume na porta 8080" sh -c "for i in 1 2 3 4 5 6 7 8 9 10; do curl -s --max-time 2 http://127.0.0.1:8080/ | grep -q 'DOOM' && exit 0; sleep 1; done; exit 1"
$rc machine delete --name "$name-doom" -f >/dev/null

step "8i. Branch e pausa"
$rc machine create --name "$name-mom" --stack python >/dev/null && $rc machine start --name "$name-mom" >/dev/null
$rc machine exec --name "$name-mom" -- sh -c 'echo antes > /root/marca'
expect "pause" $rc machine pause --name "$name-mom"
expect "status diz paused" sh -c "$rc machine status --name '$name-mom' | grep -q paused"
expect "branch da mãe pausada" $rc machine branch --from "$name-mom" --name "$name-kid"
expect "resume" $rc machine resume --name "$name-mom"
$rc machine start --name "$name-kid" >/dev/null
out=$($rc machine exec --name "$name-kid" -- cat /root/marca)
expect "a filha tem o que a mãe gravou antes do branch" test "$out" = antes
$rc machine delete --name "$name-kid" -f >/dev/null; $rc machine delete --name "$name-mom" -f >/dev/null

step "8j. Rede nomeada: máquinas que se alcançam por IP e por nome"
lab="lab$$"
$rc machine run -d --name "$name-db" --stack python --network "$lab" -- python3 -m http.server 8080 >/dev/null
$rc machine create --name "$name-app" --stack python --network "$lab" >/dev/null && $rc machine start --name "$name-app" >/dev/null
expect "app alcança db pelo IP da rede" sh -c "for i in 1 2 3 4 5 6 7 8 9 10; do $rc machine exec --name '$name-app' -- wget -qO- -T 3 http://100.98.0.10:8080/ >/dev/null 2>&1 && exit 0; sleep 1; done; exit 1"
expect "app alcança db pelo nome $name-db.$lab.internal" $rc machine exec --name "$name-app" -- wget -qO- -T 3 "http://$name-db.$lab.internal:8080/"
$rc machine delete --name "$name-db" -f >/dev/null
expect "db apagada some do DNS da rede" sh -c "! $rc machine exec --name '$name-app' -- nslookup '$name-db.$lab.internal' 2>/dev/null | grep -q 100.98.0.10"
$rc machine delete --name "$name-app" -f >/dev/null

step "8k. Volume S3 (servidor local: rclone serve s3)"
s3port=$((19200 + RANDOM % 200))
mkdir -p "$tmp/s3/rbbkt/data" && echo 'ola do bucket' > "$tmp/s3/rbbkt/data/hello.txt"
set -m
nix shell nixpkgs#rclone -c rclone serve s3 "$tmp/s3" --addr "127.0.0.1:$s3port" --auth-key rbkey,rbsecret123 >"$tmp/s3.log" 2>&1 &
s3_pid=$!
set +m
for _ in $(seq 1 120); do curl -s -o /dev/null --max-time 1 "http://127.0.0.1:$s3port/" && break; sleep 0.5; done
$rc machine create --name "$name-s3" --stack python --net --allow-cidr 127.0.0.1/32 \
  -e AWS_ACCESS_KEY_ID=rbkey -e AWS_SECRET_ACCESS_KEY=rbsecret123 -e "AWS_ENDPOINT_URL=http://100.96.0.1:$s3port" \
  -v s3://rbbkt/data:/data >/dev/null
expect "start monta o bucket" $rc machine start --name "$name-s3"
out=$($rc machine exec --name "$name-s3" -- sh -c 'cat /data/hello.txt; echo do-guest > /data/novo.txt')
expect "guest lê o que está no bucket" grep -q 'ola do bucket' <<<"$out"
expect "o que o guest grava chega ao bucket" grep -q do-guest "$tmp/s3/rbbkt/data/novo.txt"
$rc machine delete --name "$name-s3" -f >/dev/null

step "8l. Credencial que a máquina usa sem ler"
$rc machine create --name "$name-cred" --stack python --credential tok=RB_TOKEN@postman-echo.com >/dev/null
RB_TOKEN=valor-do-runbook $rc machine start --name "$name-cred" >/dev/null
out=$($rc machine exec --name "$name-cred" -- python3 -c '
import json, os, urllib.request
tok = os.environ["RB_TOKEN"]
print("guest:", tok)
r = urllib.request.Request("https://postman-echo.com/headers", headers={"Authorization": "Bearer " + tok, "User-Agent": "runbook/1"})
print("server:", json.load(urllib.request.urlopen(r, timeout=20))["headers"]["authorization"])')
expect "o guest só tem o placeholder" grep -q '^guest: RC_PLACEHOLDER_TOK_' <<<"$out"
expect "o servidor recebe o valor real" grep -q '^server: Bearer valor-do-runbook' <<<"$out"
$rc machine delete --name "$name-cred" -f >/dev/null

step "8m. Pack: a máquina num arquivo"
$rc machine create --name "$name-pk" --stack python >/dev/null && $rc machine start --name "$name-pk" >/dev/null
$rc machine exec --name "$name-pk" -- sh -c 'echo marca-do-runbook > /root/marca'
$rc machine stop --name "$name-pk" >/dev/null
expect "pack de máquina parada em arquivo único" $rc pack create --from-vm "$name-pk" --output "$tmp/one" --single-file
$rc machine delete --name "$name-pk" >/dev/null
out=$("$tmp/one" run -- cat /root/marca)
expect "o artefato traz o disco (marcador lido com a origem apagada)" test "$out" = marca-do-runbook
expect "o stub sozinho mostra o uso" sh -c "'$tmp/one' 2>&1 | grep -q 'usage: one run'"

step "8n. Usuário, volume de disco e docker.sock"
$rc machine create --name "$name-usr" --stack python --user nobody >/dev/null
$rc machine start --name "$name-usr" >/dev/null
expect "exec como o user da máquina" test "$($rc machine exec --name "$name-usr" -- id -un)" = nobody
expect "--user root no exec vence" test "$($rc machine exec --name "$name-usr" --user root -- id -un)" = root
$rc machine create --name "$name-dk" --stack python --docker-socket -v /var/lib/dados -- python3 -c '
import os, socket
s = socket.socket(socket.AF_UNIX); s.bind("/var/run/docker.sock"); s.listen()
while True:
    c, _ = s.accept(); c.sendall(b"do-guest:" + c.recv(64)); c.close()' >/dev/null
$rc machine start --name "$name-dk" >/dev/null
expect "volume de disco é ext4" test "$($rc machine exec --name "$name-dk" -- stat -f -c %T /var/lib/dados)" = ext2/ext3
out=""
for _ in $(seq 1 40); do
  out=$(python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.connect(sys.argv[1]); s.sendall(b"ping"); print(s.recv(64).decode())' "$($rc machine data-dir --name "$name-dk")/docker.sock" 2>/dev/null) && break
  sleep 0.25
done
expect "docker.sock: o host fala com o servidor do guest" test "$out" = do-guest:ping
$rc machine delete --name "$name-dk" -f >/dev/null

step "8o. Segredos e agente SSH"
ssh-keygen -q -t ed25519 -N "" -f "$tmp/id"
ssh-agent -D -a "$tmp/agent.sock" >/dev/null 2>&1 &
agent_pid=$!
for _ in $(seq 1 20); do [ -S "$tmp/agent.sock" ] && break; sleep 0.2; done
SSH_AUTH_SOCK="$tmp/agent.sock" ssh-add -q "$tmp/id"
echo do-arquivo >"$tmp/sec.txt"
$rc machine create --name "$name-sec" --image alpine --net --init 'apk add -q --no-cache openssh-client' \
  --secret-env API=RB_SECRET --secret-file "ARQ=$tmp/sec.txt" --ssh-agent >/dev/null
RB_SECRET=valor-secreto SSH_AUTH_SOCK="$tmp/agent.sock" $rc machine start --name "$name-sec" >/dev/null
out=$($rc machine exec --name "$name-sec" -- sh -c 'echo $API $ARQ; ssh-add -l')
expect "os dois segredos no ambiente" grep -q '^valor-secreto do-arquivo$' <<<"$out"
expect "o guest lista a chave do agente do host" grep -q "$(ssh-keygen -lf "$tmp/id.pub" | awk '{print $2}')" <<<"$out"
expect "o valor não fica no registro da máquina" test -z "$(grep -rl valor-secreto --exclude='*.ext4' "$($rc machine data-dir --name "$name-sec")")"
$rc machine delete --name "$name-sec" -f >/dev/null

step "8p. Pontes de socket nas duas direções"
python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.bind(sys.argv[1]); s.listen()
while True:
    c,_=s.accept(); c.sendall(b"do-host:"+c.recv(64)); c.close()' "$tmp/h.sock" >/dev/null 2>&1 &
hsrv_pid=$!
for _ in $(seq 1 20); do [ -S "$tmp/h.sock" ] && break; sleep 0.1; done
$rc machine create --name "$name-skt" --stack python --expose-socket /run/app.sock --mount-socket "$tmp/h.sock:/run/h/h.sock" -- python3 -c '
import socket
s = socket.socket(socket.AF_UNIX); s.bind("/run/app.sock"); s.listen()
while True:
    c, _ = s.accept(); c.sendall(b"do-guest:" + c.recv(64)); c.close()' >/dev/null
$rc machine start --name "$name-skt" >/dev/null
out=""
for _ in $(seq 1 40); do
  out=$(python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.connect(sys.argv[1]); s.sendall(b"ping"); print(s.recv(64).decode())' "$($rc machine data-dir --name "$name-skt")/app.sock" 2>/dev/null) && break
  sleep 0.25
done
expect "--expose-socket: o host alcança o guest" test "$out" = do-guest:ping
out=$($rc machine exec --name "$name-skt" -- python3 -c 'import socket; s=socket.socket(socket.AF_UNIX); s.connect("/run/h/h.sock"); s.sendall(b"pong"); print(s.recv(64).decode())')
expect "--mount-socket: o guest alcança o host" test "$out" = do-host:pong
$rc machine delete --name "$name-skt" -f >/dev/null

step "8q. Ping e faixa de portas"
$rc machine create --name "$name-png" --stack python --net -p 18301-18303:18301-18303 -- python3 -m http.server 18302 >/dev/null
$rc machine start --name "$name-png" >/dev/null
code=000
for _ in $(seq 1 40); do code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 2 http://127.0.0.1:18302/) && [ "$code" = 200 ] && break; sleep 0.25; done
expect "a porta do meio da faixa responde" test "$code" = 200
expect "ping 1.1.1.1 com a rede aberta" $rc machine exec --name "$name-png" -- ping -c1 -W3 1.1.1.1
$rc machine create --name "$name-pngd" --stack python --allow-cidr 198.51.100.0/24 >/dev/null
$rc machine start --name "$name-pngd" >/dev/null
expect "ping fora da allowlist não volta" sh -c "! $rc machine exec --name '$name-pngd' -- ping -c1 -W2 1.1.1.1 >/dev/null"
expect "a negação vai para o log" grep -q 'deny icmp 1.1.1.1' "$($rc machine data-dir --name "$name-pngd")/egress-denials.log"
$rc machine delete --name "$name-png" -f >/dev/null
$rc machine delete --name "$name-pngd" -f >/dev/null

step "8r. Volume staged e sync"
mkdir -p "$tmp/repo" && echo original >"$tmp/repo/a.txt" && echo x >"$tmp/repo/old.txt"
$rc machine create --name "$name-stg" --stack python -v "$tmp/repo:/work:staged" >/dev/null
$rc machine start --name "$name-stg" >/dev/null
out=$($rc machine exec --name "$name-stg" -- sh -c 'stat -f -c %T /work; echo mudou > /work/a.txt; rm /work/old.txt; printf "#!/bin/sh\n" > /work/run.sh; chmod 755 /work/run.sh')
expect "a cópia está no disco da máquina" test "$out" = ext2/ext3
expect "o host não muda antes do sync" test "$(cat "$tmp/repo/a.txt")" = original
expect "rc machine sync" $rc machine sync --name "$name-stg"
expect "depois do sync: mudou e old.txt sumiu" sh -c "[ \"\$(cat '$tmp/repo/a.txt')\" = mudou ] && [ ! -e '$tmp/repo/old.txt' ]"
expect "run.sh chega 755 ao host" test "$(python3 -c 'import os,sys; print(oct(os.stat(sys.argv[1]).st_mode & 0o777)[2:])' "$tmp/repo/run.sh")" = 755
$rc machine delete --name "$name-stg" -f >/dev/null

step "8s. Recusas que dizem o motivo"
out=$($rc doctor 2>&1)
expect "doctor: cuda not supported" grep -q '^info  cuda: not supported' <<<"$out"
expect "doctor: uma linha de nested" grep -qE '^(ok|info)  nested:' <<<"$out"
expect "--cuda recusado com o motivo, sem criar" sh -c "$rc machine create --name '$name-cu' --image alpine --cuda 2>&1 | grep -q 'no CUDA remoting' && ! $rc machine status --name '$name-cu' >/dev/null 2>&1"
if grep -q '^ok    nested: supported' <<<"$out"; then
  expect "--nested: /dev/kvm dentro da VM" $rc machine run --nested --image alpine -- ls /dev/kvm
else
  began=$(date +%s)
  out=$($rc machine run --nested --image alpine -- ls -l /dev/kvm 2>&1)
  expect "--nested recusado com o motivo" grep -q 'nested virtualization is not supported on this host' <<<"$out"
  expect "a recusa vem na hora (< 10 s)" test "$(($(date +%s) - began))" -lt 10
fi

step "8t. Diretório da máquina e limpeza do cache"
expect "data-dir tem o machine.json" test -f "$($rc machine data-dir --name "$name-usr")/machine.json"
expect "image prune" $rc image prune
expect "pack prune" $rc pack prune
$rc machine delete --name "$name-usr" -f >/dev/null

step "8u. Os guias (uma pasta, sem VM)"
expect "task guides DIRS=gpu-cuda" sh -c "task guides DIRS=gpu-cuda >'$tmp/docs.log' 2>&1"

step "8v. Terminal web (rc machine web-terminal)"
# Sobe o terminal web sem navegador, lê a URL impressa e a tela pelo SSE (base64), e confere o token.
$rc machine web-terminal --name "$name" --no-open -- sh -c 'sleep 1; echo wt-ok:$((6*7))' 2>"$tmp/wt.err" &
wt_pid=$!
wt_url=""
for _ in $(seq 1 100); do wt_url=$(grep -oE 'http://127\.0\.0\.1:[0-9]+/\?t=[0-9a-f]{32}' "$tmp/wt.err" | head -1); [ -n "$wt_url" ] && break; sleep 0.1; done
expect "URL só em 127.0.0.1, com token" test -n "$wt_url"
expect "sem token: 403" test "$(curl -s -o /dev/null -w '%{http_code}' "${wt_url%%/\?*}/stream")" = 403
wt_screen=$(timeout 20s curl -sN "${wt_url%%/\?*}/stream?${wt_url#*\?}" | python3 -c '
import base64, sys
for line in sys.stdin:
    if line.startswith("data: ") and not line.startswith("data: {"):
        sys.stdout.write(base64.b64decode(line[6:].strip()).decode("utf-8", "replace"))
    if line.startswith("event: exit"):
        break')
expect "a tela do PTY chega pelo SSE (wt-ok:42)" grep -q 'wt-ok:42' <<<"$wt_screen"
wait "$wt_pid"
expect "o rc sai com o código do programa (0)" test "$?" = 0
# Encaminhamento de portas: um servidor que o programa sobe na VM aparece em /ports e responde no 127.0.0.1 do host.
$rc machine web-terminal --name "$name" --no-open -- sh -c 'cd /tmp && timeout 15 python3 -m http.server 8133' 2>"$tmp/wtp.err" &
wtp_pid=$!
wtp_url=""
for _ in $(seq 1 100); do wtp_url=$(grep -oE 'http://127\.0\.0\.1:[0-9]+/\?t=[0-9a-f]{32}' "$tmp/wtp.err" | head -1); [ -n "$wtp_url" ] && break; sleep 0.1; done
wtp_host=""
for _ in $(seq 1 40); do
  wtp_host=$(curl -s "${wtp_url%%/\?*}/ports?${wtp_url#*\?}" | python3 -c 'import json,sys; print(next((str(f["host"]) for f in json.load(sys.stdin) if f["guest"] == 8133), ""))' 2>/dev/null)
  [ -n "$wtp_host" ] && break
  sleep 0.5
done
expect "a porta 8133 da VM aparece em /ports" test -n "$wtp_host"
expect "e responde no 127.0.0.1 do host" test "$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:${wtp_host:-1}/")" = 200
kill "$wtp_pid" 2>/dev/null
wait "$wtp_pid" 2>/dev/null

step "8w. Playground público (claude-in-chrome.runner.codes, Fly)"
# Sem criar máquina (custo zero): a página sai com o FingerprintJS, visitante inválido é 400 e porta de máquina que
# não existe é 502. Com RC_RUNBOOK_PLAYGROUND=1, abre uma sessão de verdade (cria ou religa uma Fly Machine) e confere
# que o terminal recusa token errado e aceita o certo.
pg=https://claude-in-chrome.runner.codes
pg_page=$(timeout 30s curl -s "$pg/")
expect "a página do playground sai com o FingerprintJS" grep -q 'FingerprintJS.load' <<<"$pg_page"
expect "visitante inválido: 400" test "$(timeout 30s curl -s -o /dev/null -w '%{http_code}' -X POST -d '{"visitor":"../x"}' "$pg/api/session")" = 400
expect "porta de máquina que não existe: 502" test "$(timeout 30s curl -s -o /dev/null -w '%{http_code}' https://3000-0000000000.runner.codes/)" = 502
expect "docs.runner.codes segue no Pages (o curinga não o engole)" test "$(timeout 30s curl -s -o /dev/null -w '%{http_code}' https://docs.runner.codes/)" = 200
if [ "${RC_RUNBOOK_PLAYGROUND:-0}" = 1 ]; then
  pg_sess=$(timeout 120s curl -s -X POST -d '{"visitor":"runbookvisitor0001"}' "$pg/api/session")
  pg_url=$(python3 -c 'import json,sys; print(json.loads(sys.argv[1]).get("url",""))' "$pg_sess" 2>/dev/null)
  expect "a sessão devolve /v/<id>/?t=<token>" grep -qE '^/v/[0-9a-f]{10}/\?t=[0-9a-f]{32}$' <<<"$pg_url"
  pg_base=${pg_url%%\?*}
  expect "terminal com token errado: 403" test "$(timeout 60s curl -s -o /dev/null -w '%{http_code}' "$pg${pg_base}state?t=errado")" = 403
  expect "terminal com o token: estado com a aba claude" grep -q '"label":"claude"' <<<"$(timeout 60s curl -s "$pg${pg_base}state?${pg_url#*\?}")"
else
  echo "   skip  sessão real (RC_RUNBOOK_PLAYGROUND=1 cria uma Fly Machine)"
fi

step "9. GPU (pula se a imagem llama não estiver construída)"
if $rc doctor 2>&1 | grep -q "ok    image llama"; then
  out=$($rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName' 2>&1)
  expect "device Venus no guest" grep -q "Virtio-GPU Venus" <<<"$out"
  out=$($rc run --stack llama --gpu --timeout 120s --code \
    'llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -ngl 99 -dev Vulkan0' 2>&1)
  expect "llama-bench rodou no Vulkan0" grep -q "Vulkan0" <<<"$out"
  grep -E "pp128|tg32" <<<"$out" | sed 's/^/   /'
else
  echo "   skip  imagem llama não construída"
fi

step "10. API HTTP"
$rc serve --listen 127.0.0.1:$port --pool-size 0 >"$tmp/serve.log" 2>&1 &
serve_pid=$!
for _ in $(seq 1 50); do curl -s -o /dev/null "localhost:$port/health" && break; sleep 0.2; done
out=$(curl -s -X POST "localhost:$port/api/v1/machines/$name/exec" -H 'content-type: application/json' \
  -d '{"command":["python3","-c","print(6*7)"]}')
expect "exec pela API devolve 42" grep -q '"stdout":"42\\n"' <<<"$out"
out=$(curl -s "localhost:$port/api/v1/machines")
expect "lista pela API mostra a máquina" grep -q "\"name\":\"$name\"" <<<"$out"
code=$(curl -s -o /dev/null -w '%{http_code}' -X POST "localhost:$port/api/v1/machines" -H 'content-type: application/json' -d "{\"name\":\"$name-t2\",\"image\":\"alpine\",\"allowedHost\":[\"a.com\"]}")
expect "campo desconhecido: 400" test "$code" = 400
out=$(curl -s -X POST "localhost:$port/api/v1/machines" -H 'content-type: application/json' -d "{\"name\":\"$name-t3\",\"image\":\"alpine\",\"allowedHosts\":[\"api.example.com\"],\"memoryMb\":512}")
expect "allowedHosts e memoryMb (nomes alternativos) valem" sh -c "grep -q '\"allow_hosts\":\\[\"api.example.com\"\\]' <<<'$out' && grep -q '\"enabled\":true' <<<'$out' && grep -q '\"memory_mib\":512' <<<'$out'"
code=$(curl -s -o /dev/null -w '%{http_code}' -X POST "localhost:$port/api/v1/machines" -H 'content-type: application/json' -d "{\"name\":\"$name-t4\",\"image\":\"alpine\",\"nestedVirt\":true}")
expect "nestedVirt sem --allow-nested-virt: 400" test "$code" = 400
out=$(curl -sN -X POST "localhost:$port/api/v1/machines/$name/exec/stream" -H 'content-type: application/json' -d '{"command":["sh","-c","echo a; echo b"]}')
expect "SSE: uma linha por evento, texto cru" test "$(grep '^data: ' <<<"$out" | head -2 | tr '\n' ' ')" = "data: a data: b "
expect "SSE: exit no fim" grep -q '^event: exit' <<<"$out"
code=$(curl -s -o /dev/null -w '%{http_code}' -X POST "localhost:$port/api/v1/machines/$name/sync")
expect "sync sem volume staged: 204" test "$code" = 204
$rc machine delete --name "$name-t3" -f >/dev/null

step "10b. SDKs Go e Python"
for lang in go python; do
  if [ "$lang" = go ]; then out=$(go run ./examples/sdk-go "http://127.0.0.1:$port" 2>&1)
  else out=$(PYTHONPATH=sdk/python python3 examples/sdk-python/main.py "http://127.0.0.1:$port" 2>&1); fi
  expect "sdk $lang: job, máquina, exec e erro tipado" sh -c "grep -q '^job: 42' <<<'$out' && grep -q '^exec: ola do sdk' <<<'$out' && grep -q '^error: NOT_FOUND' <<<'$out'"
done
expect "nenhuma máquina dos SDKs sobrou" sh -c "! $rc machine ls | grep -q '^sdk-'"

step "11. Limpar"
expect "stop" $rc machine stop --name "$name"
expect "delete" $rc machine delete --name "$name"

printf '\nfalhas: %d\n' "$fails"
exit "$fails"
