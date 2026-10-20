#!/bin/bash
# Prepara a Cloudflare para o playground no Fly (idempotente): o túnel claude-in-chrome gerenciado pela Cloudflare
# (config remota, o conector roda dentro da máquina do front no Fly), a ingress dele e os dois CNAMEs da zona
# runner.codes:
#   claude-in-chrome.runner.codes -> a página e os terminais
#   *.runner.codes                -> as portas dos visitantes (<porta>-<id>.runner.codes); o certificado universal
#                                    cobre um nível de curinga, e os registros que já existem (docs, www, apex) vencem
#                                    o curinga, então nada do que está no ar muda.
# O token do conector vai para o pass (bassi/runner-codes/playground-tunnel-token): só cifrar, sem aprovação. Nenhum
# segredo é impresso. Uso: scripts/playground-cloudflare.sh
set -euo pipefail
# ~/.local/bin antes do perfil do Nix: lá está o roteador gpg2 do pass (zfold/YubiKey); o gpg2 do Nix na frente manda
# o pass ao agente local e o pedido de aprovação nunca chega ao celular (task check:path reprova a ordem invertida).
export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
# pass pelo roteador ~/.local/bin/gpg2, com as ferramentas do macOS na frente.
pass_cmd() { PATH="$HOME/.local/bin:/usr/bin:/bin:$PATH" pass "$@"; }
CF_TOKEN=$(pass_cmd show bassi/cloudflare/token)
# Conta "Bassi | Global" e zona runner.codes (scripts/playground-cf-check.sh lista as duas).
ACC=e11c18594fd320087af6f18de03012b7
ZONE=8100ec8c965a04e15b3db7e92be4f2ac
NAME=claude-in-chrome
HOST=claude-in-chrome.runner.codes
api() { timeout 30s curl -sf -H "Authorization: Bearer $CF_TOKEN" -H "Content-Type: application/json" "$@"; }
CF=https://api.cloudflare.com/client/v4

# 1) o túnel: reaproveita o de mesmo nome se for de config remota; um de config local sem conexão (sobra de um
# `cloudflared tunnel create`) é apagado e recriado, porque a ingress dele não se muda pela API.
tun=$(api "$CF/accounts/$ACC/cfd_tunnel?name=$NAME&is_deleted=false" | python3 -c '
import json, sys
r = json.load(sys.stdin)["result"]
print(" ".join([r[0]["id"], r[0].get("config_src") or "local", str(len(r[0].get("connections") or []))]) if r else "")')
read -r TID SRC CONNS <<<"$tun" || true
if [ -n "${TID:-}" ] && [ "$SRC" != "cloudflare" ]; then
  [ "$CONNS" = "0" ] || { echo "🛑 túnel $NAME ($TID) é de config local e tem $CONNS conexões; não apago" >&2; exit 1; }
  api -X DELETE "$CF/accounts/$ACC/cfd_tunnel/$TID" >/dev/null
  echo "túnel local sem conexão apagado: $TID"
  TID=""
fi
if [ -z "${TID:-}" ]; then
  TID=$(api -X POST "$CF/accounts/$ACC/cfd_tunnel" -d "{\"name\":\"$NAME\",\"config_src\":\"cloudflare\"}" | python3 -c 'import json,sys; print(json.load(sys.stdin)["result"]["id"])')
  echo "túnel criado: $TID"
else
  echo "túnel existente: $TID ($SRC)"
fi

# 2) ingress: a página e o curinga das portas vão ao rc-playground no loopback da máquina do front; o resto, 404.
api -X PUT "$CF/accounts/$ACC/cfd_tunnel/$TID/configurations" -d "{\"config\":{\"ingress\":[
  {\"hostname\":\"$HOST\",\"service\":\"http://localhost:8080\"},
  {\"hostname\":\"*.runner.codes\",\"service\":\"http://localhost:8080\"},
  {\"service\":\"http_status:404\"}]}}" >/dev/null
echo "ingress gravada"

# 3) DNS: CNAME com proxy para <id>.cfargotunnel.com, criado ou corrigido.
for rec in "$HOST" "*.runner.codes"; do
  rid=$(api "$CF/zones/$ZONE/dns_records?type=CNAME&name=$rec" | python3 -c 'import json,sys; r=json.load(sys.stdin)["result"]; print(r[0]["id"] if r else "")')
  body="{\"type\":\"CNAME\",\"name\":\"$rec\",\"content\":\"$TID.cfargotunnel.com\",\"proxied\":true,\"comment\":\"playground runner.codes (Fly)\"}"
  if [ -n "$rid" ]; then api -X PUT "$CF/zones/$ZONE/dns_records/$rid" -d "$body" >/dev/null; else api -X POST "$CF/zones/$ZONE/dns_records" -d "$body" >/dev/null; fi
  echo "DNS $rec -> $TID.cfargotunnel.com"
done

# 4) token do conector para o pass (o deploy do front o põe nos fly secrets).
api "$CF/accounts/$ACC/cfd_tunnel/$TID/token" | python3 -c 'import json,sys; print(json.load(sys.stdin)["result"])' |
  pass_cmd insert -m -f bassi/runner-codes/playground-tunnel-token >/dev/null
echo "token do conector gravado em bassi/runner-codes/playground-tunnel-token"
