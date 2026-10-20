#!/bin/sh
# Roda DENTRO do builder x86_64 fixo do Fly (runner-codes-builder: imagem nixos/nix, /nix no volume), chamado por
# scripts/playground-image.sh: constrói uma imagem do flake deploy/playground (nix2container) e a publica no
# registry.fly.io. O token chega por arquivo (/token, 0600) e é apagado no fim: na linha de comando, o espaço de
# "FlyV1 …" quebraria o argumento.
#
# Uso: sh /rc-playground-remote.sh <atributo: visitor|front> <repositório> <tag>
set -eu
ATTR="$1"
REPO="$2"
TAG="$3"

AUTH=/tmp/rc-registry-auth.json
trap 'rm -f /token "$AUTH"' EXIT
export NIX_CONFIG="experimental-features = nix-command flakes"

# Fonte inteira a cada envio (o flake e os dois binários, ~16 MB): sem resto de envio anterior.
SRC=/data/rc-playground-src
rm -rf "$SRC"
mkdir -p "$SRC"
tar -xzf /rc-playground-src.tar.gz -C "$SRC"
cd "$SRC"
nix build "path:$SRC#packages.x86_64-linux.$ATTR" -o "/result-rc-$ATTR" --print-build-logs

# Credencial do registry em ARQUIVO: o copyTo do nix2container repassa os argumentos sem aspas.
umask 077
printf '{"auths":{"registry.fly.io":{"auth":"%s"}}}' "$(printf 'x:%s' "$(cat /token)" | base64 | tr -d '\n')" > "$AUTH"
# Push pelo nix2container: lê as camadas direto do store e só envia as que o registry ainda não tem.
nix run "path:$SRC#packages.x86_64-linux.$ATTR.copyTo" -- --dest-authfile "$AUTH" "docker://registry.fly.io/$REPO:$TAG"
