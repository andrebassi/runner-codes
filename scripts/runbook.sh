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

# cleanup roda até no Ctrl+C: máquina e serve do runbook não podem sobrar.
cleanup() {
  [ -n "$serve_pid" ] && kill "$serve_pid" 2>/dev/null
  $rc machine delete --name "$name" -f >/dev/null 2>&1
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

step "8. Logs da máquina"
expect "logs não vazios" sh -c "$rc machine logs --name '$name' --tail 10 | grep -q ."

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

step "11. Limpar"
expect "stop" $rc machine stop --name "$name"
expect "delete" $rc machine delete --name "$name"

printf '\nfalhas: %d\n' "$fails"
exit "$fails"
