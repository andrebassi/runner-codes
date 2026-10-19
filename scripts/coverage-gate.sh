#!/usr/bin/env bash
# Gate de cobertura que REPROVA (rule 15): total >= COVERAGE_FLOOR (padrão 90) e domínio
# em 100%. Go mede cobertura de statement; não há cobertura de branch nativa, e isso fica
# declarado — os ramos de erro são cobertos por testes de tabela.
#
# Fora da conta, com motivo:
#   cmd/runner.code  wiring (main): só liga as peças, verificado pelo e2e
#   internal/guest      executor herdado (59 linguagens) que só roda com as toolchains
#                       dentro da VM; dívida declarada com catraca em GUEST_FLOOR
#   spike/              descartável, sai na story 4.1
set -euo pipefail
cd "$(dirname "$0")/.."
floor=${COVERAGE_FLOOR:-90}
guest_floor=${GUEST_FLOOR:-36}
out=$(mktemp)
trap 'rm -f "$out" "$out.guest"' EXIT

pkgs=$(go list ./internal/... | grep -v '/internal/guest$')
# Falha de teste aparece inteira: medido em 19/10/2026, o >/dev/null com set -e fazia o gate
# sair rc=1 sem uma linha, e um teste intermitente passou por "cobertura baixa".
if ! log=$(go test -count=1 -coverprofile="$out" $pkgs 2>&1); then echo "$log" | grep -vE '^(ok|\?)'; echo "🛑 testes falharam"; exit 1; fi
total=$(go tool cover -func="$out" | awk '/^total:/ {gsub("%","",$3); print $3}')
domain=$(go tool cover -func="$out" | awk '$1 ~ /internal\/domain\// {gsub("%","",$3); s+=$3; n++} END {if (n) printf "%.1f", s/n; else print 0}')

if ! log=$(go test -count=1 -coverprofile="$out.guest" ./internal/guest/ 2>&1); then echo "$log"; echo "🛑 testes do guest falharam"; exit 1; fi
guest=$(go tool cover -func="$out.guest" | awk '/^total:/ {gsub("%","",$3); print $3}')

echo "cobertura total (sem guest): ${total}%  piso ${floor}%"
echo "cobertura domain (média por função): ${domain}%  piso 100%"
echo "cobertura guest (dívida): ${guest}%  catraca ${guest_floor}%"
rc=0
awk -v t="$total" -v f="$floor" 'BEGIN { exit (t+0 >= f+0) ? 0 : 1 }' || { echo "🛑 total abaixo do piso"; rc=1; }
awk -v t="$domain" 'BEGIN { exit (t+0 >= 100) ? 0 : 1 }' || { echo "🛑 domain abaixo de 100%"; rc=1; }
awk -v t="$guest" -v f="$guest_floor" 'BEGIN { exit (t+0 >= f+0) ? 0 : 1 }' || { echo "🛑 guest caiu abaixo da catraca"; rc=1; }
exit $rc
