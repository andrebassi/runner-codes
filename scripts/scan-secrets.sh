#!/bin/bash
# Scan de segredo antes do commit: por CONTEÚDO (diff cru das mudanças + arquivos novos não ignorados) e por NOME
# (.env, .key, .pem, token...). O diff vem de `rtk proxy git diff`: o `git diff` puro é reescrito pelo hook do rtk,
# que tira o `+` da coluna 0 e elide diff grande, e o scan reportaria limpo com segredo dentro.
# Antes de varrer, prova que a regex enxerga (canário montado em pedaços para não casar com este próprio arquivo).
# Uso: scripts/scan-secrets.sh [diretório do repo]   (sai 1 se achar)
set -u
# ~/.local/bin antes do perfil do Nix: lá está o roteador gpg2 do pass (zfold/YubiKey); o gpg2 do Nix na frente manda
# o pass ao agente local e o pedido de aprovação nunca chega ao celular (task check:path reprova a ordem invertida).
export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
cd "${1:-$(dirname "$0")/..}"
# Chaves AWS, GitLab, GitHub, chave privada PEM, Google, token do Fly (macaroon FlyV1/fm2) e token de túnel da
# Cloudflare (JSON base64 que começa com {"a":").
RE='AKIA[0-9A-Z]{16}|glpat-[A-Za-z0-9._-]{20}|ghp_[A-Za-z0-9]{36}|-----BEGIN [A-Z ]*PRIVATE KEY|AIza[0-9A-Za-z]{35}|FlyV1 fm[0-9]_|fm2_[A-Za-z0-9+/]{20}|eyJhIjoi[A-Za-z0-9]{20}'
canary="AK""IA0000000000CANARY"
grep -qE "$RE" <<<"+$canary" || { echo "🛑 regex de scan quebrada"; exit 1; }
hits=$({ rtk proxy git diff HEAD 2>/dev/null; git ls-files --others --exclude-standard -z | xargs -0 cat 2>/dev/null; } | grep -cE "$RE")
NAME_RE='(^|/)\.env($|\.)|\.(key|pem|p12|pfx|jks|keystore)$|(^|/)(credentials|secrets?|token|\.netrc|\.pgpass)($|\.)|_rsa$|_ed25519$|(^|/)id_[a-z0-9]+$'
names=$({ git diff HEAD --name-only; git ls-files --others --exclude-standard; } | grep -E "$NAME_RE" | sort -u)
echo "conteúdo: $hits achado(s)"
[ -z "$names" ] || { echo "nome suspeito:"; echo "$names"; }
[ "$hits" = 0 ] && [ -z "$names" ] && { echo "✅ limpo"; exit 0; }
echo "🛑 não commitar"
exit 1
