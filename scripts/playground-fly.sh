#!/bin/bash
# Operação do playground no Fly.io (região gru): o front runner-codes-playground (rc-playground + cloudflared) e o app
# runner-codes-visitors, onde o front cria uma Fly Machine por visitante (teto de 3). Os dois na rede privada própria
# claude-in-chrome. Segredos saem do pass e nunca são impressos.
#
# Uso: scripts/playground-fly.sh setup | deploy | status | logs | visitors | visitors:delete
set -euo pipefail
# ~/.local/bin antes do perfil do Nix: lá está o roteador gpg2 do pass (zfold/YubiKey); o gpg2 do Nix na frente manda
# o pass ao agente local e o pedido de aprovação nunca chega ao celular (task check:path reprova a ordem invertida).
export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FRONT_APP=runner-codes-playground
VISITOR_APP=runner-codes-visitors
NETWORK=claude-in-chrome
CONFIG="$ROOT/deploy/playground/fly.toml"

# pass pelo roteador ~/.local/bin/gpg2, com as ferramentas do macOS na frente.
pass_cmd() { PATH="$HOME/.local/bin:/usr/bin:/bin:$PATH" pass "$@"; }
FLY_API_TOKEN="$(pass_cmd show bassi/flyio/api-token)"
export FLY_API_TOKEN

# app_exists confere pelo nome (o JSON do flyctl muda a caixa do campo conforme o comando).
app_exists() { timeout 60s fly apps list --json | jq -e --arg a "$1" '.[] | select((.Name // .name) == $a)' >/dev/null; }

# scoped_token atenua o token do pass para um app só (leitura, escrita, criação, apagamento e controle nele).
scoped_token() {
  local id
  id="$(timeout 30s curl -sf https://api.fly.io/graphql -H "Authorization: Bearer $FLY_API_TOKEN" -H 'Content-Type: application/json' \
    -d "{\"query\":\"{ app(name: \\\"$1\\\") { internalNumericId } }\"}" | jq -r '.data.app.internalNumericId')"
  [ -n "$id" ] && [ "$id" != null ] || { echo "🛑 id numérico do app $1 não veio" >&2; return 1; }
  printf '[{"type":"Apps","body":{"apps":{"%s":"rwcdC"}}}]' "$id" | timeout 30s fly tokens attenuate -f /dev/stdin
}

# do_setup cria os dois apps na rede própria e grava os secrets do front. Idempotente.
do_setup() {
  for a in "$VISITOR_APP" "$FRONT_APP"; do
    if app_exists "$a"; then echo "app $a já existe"; else timeout 60s fly apps create "$a" --org personal --network "$NETWORK"; fi
  done
  local have
  have="$(timeout 60s fly secrets list -a "$FRONT_APP" --json | jq -r '.[].name // .[].Name' 2>/dev/null || true)"
  local args=()
  # RC_SECRET é gerado UMA vez: trocá-lo invalida o token das máquinas de visitante já criadas.
  if ! grep -qx RC_SECRET <<<"$have"; then args+=("RC_SECRET=$(openssl rand -hex 32)"); fi
  # Token de deploy com escopo SÓ no app de visitantes: o front não consegue mexer no próprio app nem nos outros.
  if ! grep -qx FLY_API_TOKEN <<<"$have"; then
    # O token do pass é de organização (caveat Organization, rwcdC) e não pode criar outro ("Not authorized to access
    # this createlimitedaccesstoken", medido em 20/10/2026). Ele é um macaroon: acrescentar um caveat Apps com só o
    # app de visitantes o ATENUA aqui mesmo, sem servidor, e o resultado não alcança o front nem o resto da org.
    args+=("FLY_API_TOKEN=$(scoped_token "$VISITOR_APP")")
  fi
  args+=("TUNNEL_TOKEN=$(pass_cmd show bassi/runner-codes/playground-tunnel-token | head -1)")
  timeout 120s fly secrets set -a "$FRONT_APP" --stage "${args[@]}" >/dev/null
  echo "✅ apps e secrets prontos (valores não impressos)"
}

# do_deploy publica o front com a imagem do visitante da última build.
do_deploy() {
  local ft vt
  ft="$(cat "$ROOT/.playground/front.tag" 2>/dev/null)" || { echo "🛑 rode antes: scripts/playground-image.sh" >&2; exit 1; }
  vt="$(cat "$ROOT/.playground/visitor.tag" 2>/dev/null)" || { echo "🛑 rode antes: scripts/playground-image.sh" >&2; exit 1; }
  timeout 600s fly deploy -c "$CONFIG" --image "registry.fly.io/$FRONT_APP:$ft" --ha=false --wait-timeout 300 \
    --env "RC_VISITOR_IMAGE=registry.fly.io/$VISITOR_APP:$vt"
  # fly deploy troca a imagem de uma máquina parada mas a deixa parada.
  for m in $(timeout 60s fly machine list -a "$FRONT_APP" --json | jq -r '.[] | select(.state == "stopped") | .id'); do
    timeout 60s fly machine start "$m" -a "$FRONT_APP"
  done
}

case "${1:-}" in
  setup) do_setup ;;
  deploy) do_deploy ;;
  status)
    timeout 60s fly status -a "$FRONT_APP"
    timeout 60s fly machine list -a "$VISITOR_APP"
    ;;
  logs) timeout 60s fly logs -a "$FRONT_APP" --no-tail ;;
  visitors) timeout 60s fly machine list -a "$VISITOR_APP" ;;
  # Apaga as máquinas de visitante (e os volumes): usado depois de uma imagem nova do visitante, para cada um nascer
  # com ela. Só o app de visitantes; o front fica.
  visitors:delete)
    for m in $(timeout 60s fly machine list -a "$VISITOR_APP" --json | jq -r '.[].id'); do timeout 60s fly machine destroy "$m" -a "$VISITOR_APP" --force; done
    for v in $(timeout 60s fly volumes list -a "$VISITOR_APP" --json | jq -r '.[].id'); do timeout 60s fly volumes destroy "$v" -a "$VISITOR_APP" --yes; done
    ;;
  *) echo "uso: $0 setup | deploy | status | logs | visitors | visitors:delete" >&2; exit 2 ;;
esac
