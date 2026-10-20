#!/usr/bin/env bash
# Guia gpu-cuda: o rc NÃO tem CUDA. O guia prova a resposta "não", dada sem ligar VM e sem
# deixar margem para ler um "sim" — a armadilha central de CUDA em VM é a sonda passar contra
# um dispositivo emulado num host sem GPU NVIDIA.
#   - preflight (`rc doctor`): diz "cuda: not supported" e aponta o --gpu (Vulkan);
#   - --cuda na CLI, `cuda = true` no runner.code e "cuda" na API: recusados com o motivo;
#   - nada é criado em nenhuma das recusas.
source "$(dirname "$0")/../lib.sh"
work=$(mktemp -d /tmp/rccudaXXXX)
port=$((19950 + RANDOM % 40))
srv=""
trap '[ -n "$srv" ] && kill $srv 2>/dev/null; cleanup_prefix cudadoc-; rm -rf "$work"' EXIT

step "1. preflight (rc doctor, não liga VM)"
doc=$("$RC" doctor 2>&1)
check "cuda: not supported" grep -q '^info  cuda: not supported' <<<"$doc"
check "a mesma linha aponta o --gpu como Vulkan" grep -q 'cuda: .*--gpu is Vulkan' <<<"$doc"
check "nenhuma VM ligada pelo doctor" test "$(vmm_count cudadoc-)" = 0

step "2. as três vias de pedir CUDA são recusadas com o motivo"
out=$("$RC" machine create --name cudadoc-cli --image alpine --cuda 2>&1)
check "--cuda: recusado, dizendo que não há CUDA remoting" grep -q 'no CUDA remoting' <<<"$out"
check "--cuda: aponta o --gpu" grep -q -- '--gpu gives the guest Vulkan' <<<"$out"
check "--cuda: nada criado" test "$(rc_state cudadoc-cli)" = absent
check "--cuda fica fora do --help (não é recurso)" sh -c "! '$RC' machine create --help | grep -q -- --cuda"
printf 'image = "alpine"\ncuda = true\n' >"$work/runner.code"
out=$(cd "$work" && "$RC" machine create --name cudadoc-file -f runner.code 2>&1)
check "cuda = true no runner.code: recusado com o motivo" grep -q 'key "cuda" is not supported yet: CUDA' <<<"$out"
check "runner.code: nada criado" test "$(rc_state cudadoc-file)" = absent
"$RC" serve --listen "127.0.0.1:$port" --pool-size 0 >/dev/null 2>&1 &
srv=$!
for _ in $(seq 1 50); do curl -s --max-time 2 -o /dev/null "http://127.0.0.1:$port/health" && break; sleep 0.2; done
code=$(curl -s --max-time 30 -o "$work/api.json" -w '%{http_code}' -X POST "http://127.0.0.1:$port/api/v1/machines" \
	-H 'content-type: application/json' -d '{"name":"cudadoc-api","image":"alpine","cuda":true}')
check "\"cuda\" na API: 400" test "$code" = 400
check "API: a mensagem nomeia o campo" grep -q 'unknown field \\"cuda\\"' "$work/api.json"
check "API: nada criado" test "$(rc_state cudadoc-api)" = absent
finish
