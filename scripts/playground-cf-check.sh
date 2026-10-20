#!/bin/bash
# Confere o que o playground precisa na Cloudflare: a zona runner.codes, o que o token do pass pode fazer
# (túnel e DNS) e os registros que já existem na zona. Não muda nada. O token nunca é impresso.
set -u
export PATH="$HOME/.nix-profile/bin:$PATH"
CF_TOKEN=$(pass show bassi/cloudflare/token) || { echo "pass falhou"; exit 1; }
api() { timeout 30s curl -s -H "Authorization: Bearer $CF_TOKEN" "https://api.cloudflare.com/client/v4$1"; }

echo "== token"
api /user/tokens/verify | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("success"), d.get("result",{}).get("status"))'
echo "== zona runner.codes"
api "/zones?name=runner.codes" | python3 -c '
import json, sys
d = json.load(sys.stdin)
for z in d.get("result", []):
    print(z["id"], z["name"], "conta:", z["account"]["id"], z["account"]["name"], "plano:", z["plan"]["name"])
print("ok:", d.get("success"), d.get("errors"))' | tee /tmp/rc-cf-zone.txt
ZONE=$(awk 'NR==1{print $1}' /tmp/rc-cf-zone.txt)
ACC=$(awk 'NR==1{print $4}' /tmp/rc-cf-zone.txt)
echo "== registros da zona"
api "/zones/$ZONE/dns_records?per_page=100" | python3 -c '
import json, sys
for r in json.load(sys.stdin).get("result", []):
    print(f"{r[\"type\"]:6} {r[\"name\"]:45} {r[\"content\"][:50]:50} proxied={r[\"proxied\"]}")'
echo "== túneis da conta (permissão de Tunnel)"
api "/accounts/$ACC/cfd_tunnel?is_deleted=false" | python3 -c '
import json, sys
d = json.load(sys.stdin)
print("ok:", d.get("success"), d.get("errors"))
for t in d.get("result", []) or []:
    print(t["id"], t["name"], t["status"])'
