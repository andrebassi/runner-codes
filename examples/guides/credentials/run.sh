#!/usr/bin/env bash
# Guia credentials: uma credencial que a máquina usa e nunca lê. A prova de contenção tem
# quatro frentes:
#   - o guest só tem o placeholder, e o valor não está em lugar nenhum dele (/proc/*/environ,
#     /run, /etc, /root, /tmp, /var, /home);
#   - o valor não está no registro da máquina no host;
#   - o host da binding é interceptado (certificado da CA da máquina) e outro host não é;
#   - placeholder na URL é recusado com 403.
# E o uso de verdade: o servidor de eco recebe o valor real no cabeçalho.
#
# O placeholder é RC_PLACEHOLDER_*; a CA se chama "runner.code credentials CA"; aqui o valor
# vem só do ambiente do start (sem --secret-file).
source "$(dirname "$0")/../lib.sh"
name=crdoc-$$
trap 'cleanup_prefix crdoc-' EXIT
export RCDOC_TOKEN="valor-rcdoc-$RANDOM$RANDOM-nao-e-segredo"
host=postman-echo.com
x() { "$RC" machine exec --name "$name" -- "$@" 2>/dev/null; }

step "create-credentialed"
check "create com --credential" "$RC" machine create --name "$name" --image alpine --credential "tok=RCDOC_TOKEN@$host"
check "start com o valor no ambiente" "$RC" machine start --name "$name"
check "curl e openssl no guest" x sh -c 'apk add -q curl openssl >/dev/null 2>&1; command -v curl && command -v openssl'

step "verify-containment"
seen=$(x printenv RCDOC_TOKEN)
printf '  guest: %s\n' "$seen"
check "o guest só tem o placeholder" grep -q '^RC_PLACEHOLDER_TOK_' <<<"$seen"
check "exec -i repassa o stdin (a busca abaixo depende disso)" test "$(printf 'probe\n' | "$RC" machine exec -i --name "$name" -- cat)" = probe
# Um processo vivo com o ambiente da máquina (herda o placeholder) e um arquivo com ele: é
# onde um valor vazado estaria, e é o que a contraprova abaixo precisa achar.
x sh -c 'printenv RCDOC_TOKEN > /root/probe-placeholder; (sleep 120 >/dev/null 2>&1 &)'

hits=$(printf '%s\n' "$RCDOC_TOKEN" | "$RC" machine exec -i --name "$name" -- sh -c 'grep -rlF -f /dev/stdin /proc/[0-9]*/environ /run /etc /root /tmp /var /home 2>/dev/null | grep -v "^/proc/$$/" | wc -l | tr -d " "' 2>/dev/null)
check "o valor não está em lugar nenhum do guest (achados: ${hits:-?})" test "${hits:-x}" = 0
# Contraprova da busca: plantado acima, o placeholder tem de aparecer no arquivo e no environ do
# processo. Busca que não acha nada nunca prova ausência (medido: sem o processo plantado, o
# glob de /proc só tinha processos sem o placeholder, e a busca dava 0 com ou sem vazamento).
phits=$(printf '%s\n' "$seen" | "$RC" machine exec -i --name "$name" -- sh -c 'grep -rlF -f /dev/stdin /proc/[0-9]*/environ /run /etc /root /tmp /var /home 2>/dev/null | grep -v "^/proc/$$/" | wc -l | tr -d " "' 2>/dev/null)
check "contraprova: a mesma busca acha o placeholder no arquivo e no processo (achados: ${phits:-?})" test "${phits:-0}" -ge 2
x sh -c 'rm -f /root/probe-placeholder; pkill sleep' >/dev/null
dir=$("$RC" machine data-dir --name "$name")
rec=$( { printf '%s\n' "$RCDOC_TOKEN" | grep -rlF --exclude='*.ext4' -f /dev/stdin "$dir" 2>/dev/null || true; } | wc -l | tr -d ' ')
check "o valor não está no registro da máquina no host" test "$rec" = 0
issuer=$(x sh -c "echo | openssl s_client -connect $host:443 -servername $host 2>/dev/null | openssl x509 -noout -issuer")
printf '  emissor em %s: %s\n' "$host" "$issuer"
check "o host da binding é interceptado (CA da máquina)" grep -q 'runner.code credentials CA' <<<"$issuer"
other=$(x sh -c "echo | openssl s_client -connect example.com:443 -servername example.com 2>/dev/null | openssl x509 -noout -issuer")
printf '  emissor em example.com: %s\n' "$other"
check "outro host passa intacto (certificado real)" sh -c "test -n '$other' && ! grep -q 'credentials CA' <<<'$other'"
refusal=$(x sh -c "curl -s -w ' [%{http_code}]' \"https://$host/get?probe=\$RCDOC_TOKEN\"")
printf '  recusa: %s\n' "$refusal"
check "placeholder na URL recusado com 403" grep -q '\[403\]' <<<"$refusal"

step "uso: o servidor recebe o valor real"
echo=$(x sh -c "curl -s -A rcdoc/1 -H \"Authorization: Bearer \$RCDOC_TOKEN\" https://$host/headers")
check "o eco mostra o valor real no Authorization" grep -qF "Bearer $RCDOC_TOKEN" <<<"$echo"
finish
