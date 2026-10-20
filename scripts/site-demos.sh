#!/bin/bash
# Roda, numa VM de verdade, cada cena das demonstrações do site (runner.codes) e imprime comando + saída + tempo.
# O site copia estas saídas: nenhuma linha da página é inventada. Uso: task site:demos [-- cena...]
# Cenas: network ping branch credential pack staged api. Sem argumento, todas.
set -u
cd "$(dirname "$0")/.."
RC=./bin/rc
OUT=${SITE_DEMOS_DIR:-$(mktemp -d)}
PORT=${SITE_DEMOS_PORT:-18099}
mkdir -p "$OUT"
# Prefixo das máquinas desta captura: o cleanup apaga só elas, nunca as máquinas de quem roda.
P=site
fails=0

# Mostra o comando como o site vai exibir, roda com prazo e imprime a saída e o tempo em ms.
# 180 s: o primeiro init com pip pela rede é o passo mais lento medido (~40 s); folga para rede ruim.
run() {
  echo "\$ $*"
  local t0 t1 rc
  t0=$(python3 -c 'import time; print(int(time.time()*1000))')
  timeout 180s bash -c "$*" 2>&1
  rc=$?
  t1=$(python3 -c 'import time; print(int(time.time()*1000))')
  echo "# rc=$rc tempo=$((t1 - t0))ms"
  return $rc
}

# Exige que a saída contenha o texto esperado: a cena só vale para o site se provou o que diz.
expect() {
  if grep -qF -- "$2" <<<"$1"; then echo "# ok: $2"; else echo "# FALHOU: esperava '$2'"; fails=$((fails + 1)); fi
}

# Apaga as máquinas desta captura e o servidor da API, mesmo com Ctrl-C no meio.
cleanup() {
  [ -n "${srv_pid:-}" ] && kill "$srv_pid" 2>/dev/null
  for m in $($RC machine ls 2>/dev/null | awk -v p="^$P-" 'NR>1 && $1 ~ p {print $1}'); do
    $RC machine stop --name "$m" --timeout 0 >/dev/null 2>&1
    $RC machine delete --name "$m" >/dev/null 2>&1
  done
}
trap cleanup EXIT
cleanup

# Rede com filtro: só pypi.org sai; o resto nem resolve, e a recusa vai para o log da máquina.
scene_network() {
  run "$RC machine create --name $P-net --stack python --allow-host pypi.org"
  run "$RC machine start --name $P-net"
  o=$(run "$RC machine exec --name $P-net -- python3 -c 'import urllib.request as u; print(u.urlopen(\"https://pypi.org/simple/requests/\").status)'"); echo "$o"; expect "$o" "200"
  o=$(run "$RC machine exec --name $P-net -- python3 -c 'import urllib.request as u; u.urlopen(\"https://example.com\")' 2>&1 | tail -1"); echo "$o"; expect "$o" "URLError"
  o=$(run "tail -n 2 \"\$($RC machine data-dir --name $P-net)/egress-denials.log\" | cut -c 22-"); echo "$o"; expect "$o" "deny dns example.com"
}

# Ping: ICMP passa pela mesma política; fora da lista não volta e fica registrado.
scene_ping() {
  run "$RC machine create --name $P-png --stack python --net"
  run "$RC machine start --name $P-png"
  o=$(run "$RC machine exec --name $P-png -- ping -c1 -W3 1.1.1.1 | head -2"); echo "$o"; expect "$o" "bytes from 1.1.1.1"
  run "$RC machine create --name $P-pngd --stack python --allow-cidr 198.51.100.0/24"
  run "$RC machine start --name $P-pngd"
  o=$(run "$RC machine exec --name $P-pngd -- ping -c1 -W2 1.1.1.1 | tail -2"); echo "$o"; expect "$o" "100% packet loss"
  o=$(run "grep 'deny icmp' \"\$($RC machine data-dir --name $P-pngd)/egress-denials.log\" | tail -1 | cut -c 22-"); echo "$o"; expect "$o" "deny icmp 1.1.1.1"
}

# Branch: uma máquina aquecida (init com pip) vira três filhas copy-on-write, sem rodar o init de novo.
scene_branch() {
  run "$RC machine create --name $P-base --stack python --net --init 'pip install -q --user requests'"
  run "$RC machine start --name $P-base"
  run "$RC machine stop --name $P-base"
  o=$(run "for i in 1 2 3; do $RC machine branch --from $P-base --name $P-try-\$i; done"); echo "$o"; expect "$o" "rc=0"
  run "$RC machine start --name $P-try-2"
  o=$(run "$RC machine exec --name $P-try-2 -- python3 -c 'import requests; print(\"requests\", requests.__version__)'"); echo "$o"; expect "$o" "requests 2."
  o=$(run "$RC machine pause --name $P-try-2 && $RC machine exec --name $P-try-2 -- true; $RC machine resume --name $P-try-2"); echo "$o"; expect "$o" "paused"
}

# Credencial: o guest só vê o placeholder; o servidor recebe o valor real, trocado no gateway do host.
scene_credential() {
  run "$RC machine create --name $P-cred --stack python --credential echo=API_TOKEN@httpbin.org"
  run "API_TOKEN=tok-real-123 $RC machine start --name $P-cred"
  o=$(run "$RC machine exec --name $P-cred -- sh -c 'echo \$API_TOKEN'"); echo "$o"; expect "$o" "RC_PLACEHOLDER_ECHO_"
  o=$(run "$RC machine exec --name $P-cred -- python3 -c 'import os,json,urllib.request as u; r=u.Request(\"https://httpbin.org/headers\", headers={\"Authorization\": \"Bearer \"+os.environ[\"API_TOKEN\"], \"User-Agent\": \"curl/8\"}); print(json.load(u.urlopen(r))[\"headers\"][\"Authorization\"])'"); echo "$o"; expect "$o" "Bearer tok-real-123"
  o=$(run "grep -rl tok-real-123 --exclude='*.ext4' \"\$($RC machine data-dir --name $P-cred)\" | wc -l | tr -d ' '"); echo "$o"; expect "$o" "0"
}

# Pack: a stack vira um executável que roda em outro host sem pull nem build.
scene_pack() {
  run "$RC pack create --stack python --output $OUT/py312"
  o=$(run "ls -lh $OUT/py312 $OUT/py312.rcmachine | awk '{print \$5, \$9}' | sed 's|$OUT/||'"); echo "$o"
  o=$(run "$OUT/py312 run -- python3 -c 'import sys; print(sys.version.split()[0])'"); echo "$o"; expect "$o" "3."
}

# Staged: a cópia mora no disco da máquina; o host só muda no sync, e o script novo chega executável.
scene_staged() {
  mkdir -p "$OUT/repo" && echo original >"$OUT/repo/a.txt" && echo x >"$OUT/repo/old.txt"
  run "$RC machine create --name $P-stg --stack python -v $OUT/repo:/work:staged"
  run "$RC machine start --name $P-stg"
  run "$RC machine exec --name $P-stg -- sh -c 'echo mudou > /work/a.txt; rm /work/old.txt; printf \"#!/bin/sh\\n\" > /work/run.sh; chmod 755 /work/run.sh'"
  o=$(run "cat $OUT/repo/a.txt"); echo "$o"; expect "$o" "original"
  run "$RC machine sync --name $P-stg"
  o=$(run "cat $OUT/repo/a.txt; ls $OUT/repo; ls -l $OUT/repo/run.sh | awk '{print \$1, \$NF}' | sed 's|$OUT/||'"); echo "$o"; expect "$o" "-rwxr-xr-x"
}

# API: pool pré-bootado, uma requisição de execução e o streaming SSE uma linha por evento.
scene_api() {
  $RC serve --listen 127.0.0.1:$PORT --pool-size 1 >"$OUT/serve.log" 2>&1 &
  srv_pid=$!
  # Até 30 s para a API responder; depois mais 3 s para o pool ter a VM pronta (a medida é com pool cheio).
  for _ in $(seq 60); do curl -s -m 1 "http://127.0.0.1:$PORT/api/v1/languages" >/dev/null && break; sleep 0.5; done
  sleep 3
  o=$(run "curl -s -m 30 -X POST http://127.0.0.1:$PORT/api/v1/run -H 'content-type: application/json' -d '{\"language\":\"python\",\"stdin\":\"oi\",\"files\":[{\"name\":\"main.py\",\"content\":\"print(input().upper())\"}]}'"); echo "$o"; expect "$o" '"stdout":"OI\n"'
  o=$(run "curl -s -N -m 30 -X POST http://127.0.0.1:$PORT/api/v1/execute/stream -H 'content-type: application/json' -d '{\"language\":\"python\",\"code\":\"import time\\nprint(1)\\ntime.sleep(1)\\nprint(2)\"}'"); echo "$o"; expect "$o" "event: result"
  kill "$srv_pid"; srv_pid=
}

scenes=${*:-network ping branch credential pack staged api}
for s in $scenes; do
  echo "===== $s"
  "scene_$s"
done
echo "falhas: $fails"
exit "$fails"
