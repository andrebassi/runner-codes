#!/usr/bin/env bash
# Publica a branch atual: push, PR e merge pela API do GitHub, sem o `gh` (saiu com o
# Homebrew; o Nix fica só com o que o projeto usa). Uma decifragem do pass só: o token
# vive numa variável deste processo e não vai para a linha de comando nem para o disco.
# Antes de tudo espera o agente do celular responder, porque a rede dele cai por segundos
# (medido em 19/10/2026: 66% de perda no Wi-Fi local com a tela apagada).
# Uso: scripts/ship-pr.sh "<título>" <arquivo-com-o-corpo>
set -uo pipefail
title=${1:?title}
body_file=${2:?body file}
repo=andrebassi/runner-codes
cd "$(dirname "$0")/.."
branch=$(git rev-parse --abbrev-ref HEAD)
[ "$branch" != "main" ] || { echo "refusing to ship main"; exit 1; }

# 20 tentativas a cada 3 s: cobre uma queda de rádio do celular sem esperar para sempre.
for i in $(seq 1 20); do
  # Captura antes do grep: com pipefail, `status | grep -q` dava falso justamente no sucesso
  # (o grep -q fecha o pipe ao achar, o status leva SIGPIPE e o pipe inteiro "falha").
  # Medido em 19/10/2026: 20 voltas de espera com a YubiKey e o celular respondendo.
  st=$(timeout 20s pass-route status 2>&1)
  grep -qE "agente do zfold: ✅|YubiKey: ✅" <<<"$st" && break
  echo "waiting for a signing/pass agent ($i/20)"
  sleep 3
done

git log -1 --format=%B | grep -iE "claude|anthropic|co-authored|generated with" && { echo "attribution in the last commit"; exit 1; }

TOKEN=$(pass show bassi/github/token) || { echo "pass failed"; exit 1; }
[ -n "$TOKEN" ] || { echo "empty token"; exit 1; }
export TOKEN
timeout 90s git -c credential.helper= -c 'credential.helper=!f() { echo username=oauth2; echo "password=$TOKEN"; }; f' \
  push -q -u origin "$branch" || { echo "push failed"; exit 1; }
echo "pushed $branch"

api() { timeout 30s curl -fsS -H "Authorization: Bearer $TOKEN" -H "Accept: application/vnd.github+json" "$@"; }
payload=$(python3 -c 'import json,sys; print(json.dumps({"title":sys.argv[1],"head":sys.argv[2],"base":"main","body":open(sys.argv[3]).read()}))' \
  "$title" "$branch" "$body_file")
pr=$(api -X POST "https://api.github.com/repos/$repo/pulls" -d "$payload" | python3 -c 'import json,sys; print(json.load(sys.stdin)["number"])') \
  || { echo "PR creation failed"; exit 1; }
echo "PR #$pr https://github.com/$repo/pull/$pr"
api -X PUT "https://api.github.com/repos/$repo/pulls/$pr/merge" -d '{"merge_method":"merge"}' \
  | python3 -c 'import json,sys; d=json.load(sys.stdin); print("merged", d.get("sha","")[:7]) if d.get("merged") else (print(d), sys.exit(1))'
unset TOKEN
