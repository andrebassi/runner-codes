#!/usr/bin/env bash
# Guia reference: os temas de referência do rc, uma etapa por tema que tem o que provar numa
# VM. Os temas fora do escopo estão no README.md, com o motivo.
#
# uso: run.sh [etapa ...]   (sem argumento: todas; ex.: run.sh runner-code limitations)
source "$(dirname "$0")/../lib.sh"
p=refdoc-$$
work=$(mktemp -d /tmp/rcrefdocXXXX)
srv=""
trap '[ -n "$srv" ] && kill $srv 2>/dev/null; cleanup_prefix refdoc-; rm -rf "$work"' EXIT
x() { "$RC" machine exec --name "$1" -- "${@:2}" 2>/dev/null | tr -d '\r'; }
want() { [ $# -eq 0 ] || [ -z "${ONLY:-}" ] || [[ " $ONLY " == *" $1 "* ]]; }
ONLY="$*"
base=$((18100 + RANDOM % 400))

if want runner-code; then
	step "runner-code: o arquivo da máquina com todas as chaves da referência"
	cat >"$work/runner.code" <<EOF
image = "python:3.12-alpine"
cpus = 2
memory = 1024
gpu_vram = 4096                      # ignorado sem gpu
stop_signal = "SIGRTMIN+3"           # 37, a numeração da glibc
stop_timeout = 30
cmd = ["python3", "-m", "http.server", "$((base + 1))", "--bind", "0.0.0.0"]
ports = ["$base-$((base + 2)):$base-$((base + 2))"]
env = ["TOP=1"]

[network]
allow_hosts = ["pypi.org"]

[dev]
env = ["APP_MODE=dev"]
workdir = "/tmp"
EOF
	check "create -s runner.code" "$RC" machine create --name "$p-sf" -s "$work/runner.code"
	reg="$("$RC" machine data-dir --name "$p-sf")/machine.json"
	# O machine.json é gravado indentado: "chave": valor, com espaço.
	check "faixa de portas: 3 mapeamentos" test "$(grep -cE '"host": *[0-9]+' "$reg")" = 3
	check "stop_signal SIGRTMIN+3 guardado" grep -qE '"stop_signal": *"SIGRTMIN\+3"' "$reg"
	check "gpu_vram sem gpu: ignorado" sh -c "! grep -q gpu_vram '$reg'"
	check "start" "$RC" machine start --name "$p-sf"
	ok=0
	for _ in $(seq 1 40); do
		curl -s --max-time 2 -o /dev/null "http://127.0.0.1:$((base + 1))/" && { ok=1; break; }
		sleep 0.5
	done
	check "a porta do meio da faixa responde no host" test "$ok" = 1
	check "[dev].env soma ao env de cima" test "$(x "$p-sf" sh -c 'echo $TOP-$APP_MODE')" = 1-dev
	check "[dev].workdir" test "$(x "$p-sf" pwd)" = /tmp
	printf 'image = "alpine"\ncolour = "azul"\n' >"$work/Bad"
	check "chave desconhecida recusada com o nome" sh -c "'$RC' machine create --name '$p-bad' -s '$work/Bad' 2>&1 | grep -q colour"
	check "nada criado pela recusa" test "$(rc_state "$p-bad")" = absent
	"$RC" machine delete --name "$p-sf" --force >/dev/null
fi

if want named-networks; then
	step "named-networks: rede lab, por IP e por nome; outra rede isolada"
	"$RC" machine create --name "$p-db" --image alpine --network lab-$$ -- sleep 3600 >/dev/null 2>&1
	"$RC" machine create --name "$p-app" --image alpine --network lab-$$ -- sleep 3600 >/dev/null 2>&1
	"$RC" machine create --name "$p-out" --image alpine --network other-$$ -- sleep 3600 >/dev/null 2>&1
	for m in db app out; do check "start $m" "$RC" machine start --name "$p-$m"; done
	# O IP da rede (100.98.x) é do gateway, não aparece no eth0 (100.96.0.2, o NAT): sai do
	# DNS da própria rede.
	ipof() { x "$1" nslookup "$2" | awk '/^Address: /{print $2}' | tail -1; }
	ip=$(ipof "$p-app" "$p-db.lab-$$.internal")
	check "app resolve db.lab-$$.internal ($ip)" sh -c "echo '$ip' | grep -q '^100\.98\.'"
	check "app pinga db pelo IP da rede" sh -c "'$RC' machine exec --name '$p-app' -- ping -c1 -W3 '$ip' >/dev/null"
	check "outra rede não resolve o nome" test -z "$(ipof "$p-out" "$p-db.lab-$$.internal")"
	# Cada rede tem o seu pool, e o mesmo endereço pode ser da própria out (que responde). O
	# isolamento se prova com um IP da lab que a other não deu a ninguém.
	own=$(ipof "$p-out" "$p-out.other-$$.internal")
	target=$ip
	[ "$target" = "$own" ] && target=$(ipof "$p-db" "$p-app.lab-$$.internal")
	check "outra rede não alcança $target (a out é $own)" sh -c "[ '$target' != '$own' ] && ! '$RC' machine exec --name '$p-out' -- ping -c1 -W2 '$target' >/dev/null"
	for m in db app out; do "$RC" machine delete --name "$p-$m" --force >/dev/null; done
fi

if want nested-virtualization; then
	step "nested-virtualization: --nested, recusado na hora e com o motivo onde o host não aninha"
	doc=$("$RC" doctor 2>&1)
	if grep -q '^ok    nested: supported' <<<"$doc"; then
		check "host com nested: /dev/kvm no guest" sh -c "'$RC' machine run --nested --image alpine -- ls /dev/kvm"
	else
		check "doctor diz que este host não aninha" grep -q '^info  nested: not supported on this host' <<<"$doc"
		began=$(date +%s)
		out=$("$RC" machine run --nested --image alpine -- ls -l /dev/kvm 2>&1)
		check "run --nested: recusado com o motivo" grep -q 'nested virtualization is not supported on this host' <<<"$out"
		check "a recusa vem na hora (< 10 s), não no prazo de prontidão" test "$(($(date +%s) - began))" -lt 10
		check "nenhuma máquina sobrou" test -z "$(rc_names | grep '^run-')"
	fi
	printf 'image = "alpine"\nnested = true\n' >"$work/Nested"
	check "o runner.code não liga nested" sh -c "'$RC' machine create --name '$p-n' -s '$work/Nested' 2>&1 | grep -q -- '--nested'"
fi

if want limitations; then
	step "limitations: eth0, ping, volume só de diretório, /workspace do host"
	"$RC" machine create --name "$p-net" --image alpine --net -- sleep 3600 >/dev/null 2>&1
	check "start --net" "$RC" machine start --name "$p-net"
	check "eth0 com endereço (sempre virtio-net; não há TSI)" sh -c "'$RC' machine exec --name '$p-net' -- ip -4 addr show eth0 | grep -q inet"
	check "ping funciona (ICMP pelo gateway)" sh -c "'$RC' machine exec --name '$p-net' -- ping -c1 -W3 1.1.1.1 >/dev/null"
	"$RC" machine delete --name "$p-net" --force >/dev/null
	echo arq >"$work/f.txt"
	check "volume de arquivo único recusado" sh -c "'$RC' machine create --name '$p-f' --image alpine -v '$work/f.txt:/f.txt' 2>&1 | grep -q 'existing directory'"
	mkdir -p "$work/ws" && echo do-host >"$work/ws/marca"
	check "-v DIR:/workspace vence o disco da máquina" sh -c "'$RC' machine run --image alpine -v '$work/ws:/workspace' -- cat /workspace/marca | grep -qx do-host"
fi

if want security-model; then
	step "security-model: pontes de socket nas duas direções; o que o rc não tem"
	python3 - "$work/h.sock" >/dev/null 2>&1 <<'PY' &
import socket, sys
s = socket.socket(socket.AF_UNIX); s.bind(sys.argv[1]); s.listen()
while True:
    c, _ = s.accept(); c.sendall(b"do-host:" + c.recv(64)); c.close()
PY
	srv=$!
	for _ in $(seq 1 20); do [ -S "$work/h.sock" ] && break; sleep 0.1; done
	"$RC" machine create --name "$p-sk" --stack python --expose-socket /run/app.sock \
		--mount-socket "$work/h.sock:/run/podman/h.sock" -- python3 -c '
import socket
s = socket.socket(socket.AF_UNIX); s.bind("/run/app.sock"); s.listen()
while True:
    c, _ = s.accept(); c.sendall(b"do-guest:" + c.recv(64)); c.close()' >/dev/null 2>&1
	check "start" "$RC" machine start --name "$p-sk"
	exposed="$("$RC" machine data-dir --name "$p-sk")/app.sock"
	got=""
	for _ in $(seq 1 40); do
		# Só a resposta encerra a espera: a ponte do rc aceita o connect antes de o servidor do guest
		# escutar e fecha sem dados, e o python sai 0 com resposta vazia (medido em 21/10/2026: o laço
		# parava na 1ª volta e o check reprovava com a ponte funcionando).
		got=$(python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.connect(sys.argv[1]); s.sendall(b"ping"); print(s.recv(64).decode())' "$exposed" 2>/dev/null)
		[ -n "$got" ] && break
		sleep 0.25
	done
	check "--expose-socket: o host fala com o servidor do guest" test "$got" = do-guest:ping
	check "--mount-socket: o guest fala com o servidor do host" test "$(x "$p-sk" python3 -c 'import socket; s=socket.socket(socket.AF_UNIX); s.connect("/run/podman/h.sock"); s.sendall(b"pong"); print(s.recv(64).decode())')" = do-host:pong
	"$RC" machine delete --name "$p-sk" --force >/dev/null
	# O servidor herdou a saída do script: vivo, quem lê pelo pipe nunca recebe o fim.
	kill $srv 2>/dev/null
	srv=""
	for f in --allow-system-mounts --use-host-proxy --trust-host-certs; do
		check "$f: não existe no rc (recusado, nada criado)" sh -c "! '$RC' machine create --name '$p-z' --image alpine $f 2>/dev/null"
	done
	check "nada criado pelas recusas" test "$(rc_state "$p-z")" = absent
fi

if want browser-display; then
	step "browser-display: --gpu --display, visualizador no navegador"
	dport=$((base + 300))
	"$RC" machine create --name "$p-dsp" --image alpine --gpu --display 640x480 --display-port "$dport" -- sleep 3600 >/dev/null 2>&1
	check "start com --gpu --display" "$RC" machine start --name "$p-dsp"
	check "o visualizador responde HTML em 127.0.0.1:$dport" sh -c "curl -s --max-time 5 'http://127.0.0.1:$dport/' | grep -qi '<html'"
	check "o guest tem /dev/dri/card0 (scanout KMS)" sh -c "'$RC' machine exec --name '$p-dsp' -- ls /dev/dri | grep -qx card0"
	"$RC" machine delete --name "$p-dsp" --force >/dev/null
fi

if want pack-layer-ownership; then
	step "pack-layer-ownership: dono de arquivo da imagem preservado na máquina e no pack"
	# postgres:16-alpine guarda /var/lib/postgresql como 70:70 (o usuário postgres); o
	# alpine, /etc/shadow como 0:42. Achatado no host sem root, os dois virariam do usuário.
	check "na máquina: /var/lib/postgresql é 70:70" sh -c "'$RC' machine run --image postgres:16-alpine -- stat -c %u:%g /var/lib/postgresql | grep -qx 70:70"
	check "na máquina: /etc/shadow é 0:42" sh -c "'$RC' machine run --image alpine -- stat -c %u:%g /etc/shadow | grep -qx 0:42"
	check "pack create --image postgres:16-alpine" "$RC" pack create --image postgres:16-alpine --output "$work/pg"
	check "no pack: o mesmo 70:70" sh -c "'$work/pg' run -- stat -c %u:%g /var/lib/postgresql | grep -qx 70:70"
fi

if want volumes; then
	step "volumes: bucket S3 (rclone serve s3 local) montado no guest"
	if command -v nix >/dev/null; then
		mkdir -p "$work/s3/dados" && echo do-bucket >"$work/s3/dados/marca.txt"
		s3port=$((base + 350))
		nix shell nixpkgs#rclone -c rclone serve s3 "$work/s3" --addr "127.0.0.1:$s3port" --auth-key rckey,rcsecret123 >/dev/null 2>&1 &
		srv=$!
		for _ in $(seq 1 240); do nc -z 127.0.0.1 "$s3port" 2>/dev/null && break; sleep 0.5; done
		# O guest alcança o 127.0.0.1 do host pelo gateway (100.96.0.1), e só com ele liberado.
		check "s3 -v s3://dados:/data com -e AWS_*" sh -c "'$RC' machine run --image alpine --allow-cidr 127.0.0.1 \
			-e AWS_ACCESS_KEY_ID=rckey -e AWS_SECRET_ACCESS_KEY=rcsecret123 -e AWS_ENDPOINT_URL=http://100.96.0.1:$s3port \
			-v s3://dados:/data -- cat /data/marca.txt | grep -qx do-bucket"
		kill $srv 2>/dev/null
		srv=""
	else
		printf '   skip  sem nix para o S3 local\n'
	fi
	step "volumes: -v HOST:GUEST:staged (cópia no disco, sync de volta)"
	mkdir -p "$work/repo" && echo original >"$work/repo/a.txt" && echo x >"$work/repo/old.txt"
	"$RC" machine create --name "$p-st" --image alpine -v "$work/repo:/work:staged" -- sleep 3600 >/dev/null 2>&1
	check "start semeia a cópia" sh -c "'$RC' machine start --name '$p-st' >/dev/null && '$RC' machine exec --name '$p-st' -- cat /work/a.txt | grep -qx original"
	check "a cópia está no ext4 da máquina, não no virtio-fs" sh -c "'$RC' machine exec --name '$p-st' -- stat -f -c %T /work | grep -qx ext2/ext3"
	x "$p-st" sh -c 'echo mudou > /work/a.txt; rm /work/old.txt; printf "#!/bin/sh\n" > /work/run.sh; chmod 755 /work/run.sh' >/dev/null
	check "o host não muda antes do sync" test "$(cat "$work/repo/a.txt")" = original
	check "rc machine sync" "$RC" machine sync --name "$p-st"
	check "depois do sync: a.txt mudou, old.txt sumiu" sh -c "[ \"\$(cat '$work/repo/a.txt')\" = mudou ] && [ ! -e '$work/repo/old.txt' ]"
	# Modo lido em Python: o stat do PATH pode ser o GNU (Nix), onde -f é outra coisa.
	check "o script novo chega executável ao host (755)" test "$(python3 -c 'import os,sys; print(oct(os.stat(sys.argv[1]).st_mode & 0o777)[2:])' "$work/repo/run.sh")" = 755
	x "$p-st" sh -c 'echo tarde > /work/late.txt' >/dev/null
	"$RC" machine stop --name "$p-st" >/dev/null
	check "o stop gracioso sincroniza" test "$(cat "$work/repo/late.txt" 2>/dev/null)" = tarde
	"$RC" machine delete --name "$p-st" --force >/dev/null
fi

finish
