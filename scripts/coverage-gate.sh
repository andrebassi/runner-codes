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
guest_floor=${GUEST_FLOOR:-67}
out=$(mktemp)
trap 'rm -f "$out" "$out.guest"' EXIT

pkgs=$(go list ./internal/... | grep -v '/internal/guest$')
# Falha de teste aparece inteira: medido em 19/10/2026, o >/dev/null com set -e fazia o gate
# sair rc=1 sem uma linha, e um teste intermitente passou por "cobertura baixa".
if ! log=$(go test -count=1 -coverprofile="$out" $pkgs 2>&1); then echo "$log" | grep -vE '^(ok|\?)'; echo "🛑 tests failed"; exit 1; fi
total=$(go tool cover -func="$out" | awk '/^total:/ {gsub("%","",$3); print $3}')
domain=$(go tool cover -func="$out" | awk '$1 ~ /internal\/domain\// {gsub("%","",$3); s+=$3; n++} END {if (n) printf "%.1f", s/n; else print 0}')

if ! log=$(go test -count=1 -coverprofile="$out.guest" ./internal/guest/ 2>&1); then echo "$log"; echo "🛑 guest tests failed"; exit 1; fi
guest=$(go tool cover -func="$out.guest" | awk '/^total:/ {gsub("%","",$3); print $3}')

echo "total coverage (without guest): ${total}%  floor ${floor}%"
echo "domain coverage (per-function average): ${domain}%  floor 100%"
echo "guest coverage (debt): ${guest}%  ratchet ${guest_floor}%"
rc=0
awk -v t="$total" -v f="$floor" 'BEGIN { exit (t+0 >= f+0) ? 0 : 1 }' || { echo "🛑 total below the floor"; rc=1; }
awk -v t="$domain" 'BEGIN { exit (t+0 >= 100) ? 0 : 1 }' || { echo "🛑 domain below 100%"; rc=1; }
awk -v t="$guest" -v f="$guest_floor" 'BEGIN { exit (t+0 >= f+0) ? 0 : 1 }' || { echo "🛑 guest dropped below the ratchet"; rc=1; }
exit $rc
