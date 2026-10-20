#!/bin/bash
# Prova de falha do token do front (rule 13): o token atenuado por scripts/playground-fly.sh (scoped_token) tem de
# listar as máquinas do app de visitantes (200) e ser RECUSADO no site e no próprio front. Nenhum token é impresso.
# Uso: scripts/playground-token-check.sh   (sai com o número de respostas fora do esperado)
set -u
# ~/.local/bin antes do perfil do Nix: lá está o roteador gpg2 do pass (zfold/YubiKey); o gpg2 do Nix na frente manda
# o pass ao agente local e o pedido de aprovação nunca chega ao celular (task check:path reprova a ordem invertida).
export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
pass_cmd() { PATH="$HOME/.local/bin:/usr/bin:/bin:$PATH" pass "$@"; }
FLY_API_TOKEN="$(pass_cmd show bassi/flyio/api-token)" || exit 1
export FLY_API_TOKEN
# A mesma função do setup, lida do script, para o teste não divergir dela.
eval "$(sed -n '/^scoped_token()/,/^}/p' "$(dirname "$0")/playground-fly.sh")"
T="$(scoped_token runner-codes-visitors)" || exit 1
bad=0
for pair in runner-codes-visitors:200 bassi-site:401 runner-codes-playground:401; do
  app=${pair%%:*} want=${pair##*:}
  code=$(timeout 30s curl -s -o /dev/null -w '%{http_code}' -H "Authorization: Bearer $T" "https://api.machines.dev/v1/apps/$app/machines")
  # Recusa pode vir 401 ou 403 conforme a API; o que importa é não ser 2xx.
  if [ "$want" = 200 ] && [ "$code" = 200 ]; then echo "✅ $app: $code"
  elif [ "$want" != 200 ] && [ "${code:0:1}" = 4 ]; then echo "✅ $app: $code (recusado)"
  else echo "🛑 $app: $code"; bad=$((bad + 1)); fi
done
exit "$bad"
