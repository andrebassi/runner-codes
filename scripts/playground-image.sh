#!/bin/bash
# Monta as duas imagens do playground (deploy/playground/flake.nix: visitor e front) no builder x86_64 do Fly e
# publica em registry.fly.io. O builder é FIXO e próprio (app runner-codes-builder, máquina nix-builder, /nix no
# volume nixcache), no padrão do site andrebassi: o store fica no volume, e cada build só baixa o que mudou. Fica
# desligado entre builds (trap). Os binários Go são compilados aqui no Mac (estáticos, CGO off).
# Próprio, e não o do site, porque o do site ficou preso num host sem capacidade em 20/10/2026 ("machine can't start
# at the moment", seis tentativas): o volume prende a máquina ao host, e um builder compartilhado parava os dois.
#
# Saída: .playground/visitor.tag e .playground/front.tag (gitignored), lidos por scripts/playground-fly.sh deploy.
# Uso: scripts/playground-image.sh [visitor|front|all]
set -euo pipefail
# ~/.local/bin antes do perfil do Nix: lá está o roteador gpg2 do pass (zfold/YubiKey); o gpg2 do Nix na frente manda
# o pass ao agente local e o pedido de aprovação nunca chega ao celular (task check:path reprova a ordem invertida).
export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WHICH="${1:-all}"
# Apps do Fly: o front e o das máquinas de visitante (cada imagem vai para o registry do app que a roda).
FRONT_APP=runner-codes-playground
VISITOR_APP=runner-codes-visitors
BUILDER=runner-codes-builder
NAME=nix-builder
# iad e não gru: em 20/10/2026 o gru recusou máquina de 4 vCPUs com volume ("insufficient resources", também com
# volume novo). O builder pode ficar em qualquer região: o registry.fly.io é global.
REGION=iad
# Imagem oficial do Nix com versão fixa (nunca :latest: o build tem de ser repetível).
NIX_IMAGE=nixos/nix:2.28.3
CACHE_VOL=nixcache
CACHE_GB=20 # Node, Python, Claude Code e as últimas imagens cabem com folga

# pass pelo roteador ~/.local/bin/gpg2, com as ferramentas do macOS na frente.
pass_cmd() { PATH="$HOME/.local/bin:/usr/bin:/bin:$PATH" pass "$@"; }
FLY_API_TOKEN="$(pass_cmd show bassi/flyio/api-token)"
export FLY_API_TOKEN

cd "$ROOT"
TAG="nix-$(git rev-parse --short HEAD)"
# Árvore suja ganha o horário: a mesma tag republicada com outro conteúdo faria a máquina achar a imagem velha.
[ -z "$(git status --porcelain)" ] || TAG="$TAG-dirty-$(date +%s)"

echo "[1/4] compilando rc-visitor e rc-playground (linux/amd64, estáticos)"
for b in rc-visitor rc-playground; do
  CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -trimpath -ldflags="-s -w" -o "deploy/playground/bin/$b" "./cmd/$b"
done

builder_id() { timeout 60s fly machine list -a "$BUILDER" --json | jq -r --arg n "$NAME" '.[] | select(.name == $n) | .id' | head -1; }
# cleanup DESLIGA o builder (não destrói): o store no volume é o cache do próximo build. Armado antes da partida.
cleanup() {
  local id
  id="$(builder_id 2>/dev/null || true)"
  if [ -n "$id" ]; then timeout 60s fly machine stop "$id" -a "$BUILDER" >/dev/null 2>&1 || true; fi
}
trap cleanup EXIT
echo "[2/4] ligando o builder (custo só enquanto liga; desliga no fim)"
if ! timeout 60s fly apps list --json | jq -e --arg a "$BUILDER" '.[] | select((.Name // .name) == $a)' >/dev/null; then
  timeout 60s fly apps create "$BUILDER" --org personal >/dev/null
fi
if ! timeout 60s fly volumes list -a "$BUILDER" --json | jq -e --arg n "$CACHE_VOL" '.[] | select((.Name // .name) == $n)' >/dev/null; then
  timeout 60s fly volumes create "$CACHE_VOL" -a "$BUILDER" --region "$REGION" --size "$CACHE_GB" --yes >/dev/null
  echo "volume $CACHE_VOL criado (${CACHE_GB} GB)"
fi
# BOOT semeia o volume uma vez (marcador .seeded) e faz bind-mount dele sobre o /nix. A imagem nixos/nix não traz
# `mount`: o util-linux vem do cache.nixos.org pelo caminho exato da revisão fixada no flake.
MOUNT_PKG="$(nix eval --raw --inputs-from path:deploy/playground nixpkgs#legacyPackages.x86_64-linux.util-linux.bin)"
BOOT="set -e; if [ ! -e /data/store/.seeded ]; then rm -rf /data/store; mkdir -p /data/store; cp -a /nix/. /data/store/; touch /data/store/.seeded; fi; nix-store -r $MOUNT_PKG >/dev/null; $MOUNT_PKG/bin/mount --bind /data/store /nix; exec sleep infinity"
MACHINE="$(builder_id)"
if [ -z "$MACHINE" ]; then
  # O comando vai depois de `--`: sem ele o flyctl lê o `-c` do `sh -c` como a flag --config dele.
  timeout 300s fly machine run "$NIX_IMAGE" -a "$BUILDER" -r "$REGION" --name "$NAME" \
    --vm-cpu-kind shared --vm-cpus 4 --vm-memory 8192 --restart no --autostop=off --detach \
    --volume "$CACHE_VOL:/data" -- sh -c "$BOOT" >/dev/null
  MACHINE="$(builder_id)"
  [ -n "$MACHINE" ] || { echo "🛑 máquina $NAME não apareceu na listagem" >&2; exit 1; }
  echo "builder criado: $MACHINE (a primeira partida semeia o store no volume)"
else
  # Partida que falha tem de parar o script: o "|| true" daqui escondia "machine can't start" e o envio caía 5 vezes.
  state="$(timeout 60s fly machine list -a "$BUILDER" --json | jq -r --arg id "$MACHINE" '.[] | select(.id == $id) | .state')"
  [ "$state" = started ] || timeout 120s fly machine start "$MACHINE" -a "$BUILDER" >/dev/null || { echo "🛑 o builder $MACHINE não ligou" >&2; exit 1; }
  echo "builder ligado: $MACHINE"
fi
# Pronto = SSH responde E o /nix já é o do volume; na primeira partida a cópia do store leva alguns minutos.
for _ in $(seq 1 150); do
  if timeout 30s fly ssh console -a "$BUILDER" --machine "$MACHINE" -C "test -e /nix/.seeded" >/dev/null 2>&1; then break; fi
  sleep 2
done

# O SSH do Fly cai de vez em quando: todo passo remoto é repetido, 5 vezes com 10 s de intervalo.
SSH_TRIES=5
SSH_GAP=10
# put envia um arquivo ao builder, apagando o destino antes de cada tentativa.
put() {
  local i
  for i in $(seq 1 "$SSH_TRIES"); do
    if timeout 60s fly ssh console -a "$BUILDER" --machine "$MACHINE" -C "rm -f $2" >/dev/null 2>&1 &&
      timeout 300s fly ssh sftp put "$1" "$2" -a "$BUILDER" --machine "$MACHINE" >"$LOGDIR/put.log" 2>&1; then return 0; fi
    # O motivo da queda vai para o log, não para /dev/null: sem ele, cinco quedas seguidas não diziam nada.
    tail -2 "$LOGDIR/put.log" >&2
    echo "↻ envio de $2 caiu ($i/$SSH_TRIES)" >&2
    sleep "$SSH_GAP"
  done
  return 1
}
# published diz se a tag já está no registry (a prova de que a imagem saiu, não o rc do SSH). OCI e Docker v2.
published() {
  [ "$(timeout 30s curl -s -o /dev/null -w '%{http_code}' --max-time 30 -u "x:$FLY_API_TOKEN" \
    -H 'Accept: application/vnd.oci.image.manifest.v1+json, application/vnd.docker.distribution.manifest.v2+json' \
    "https://registry.fly.io/v2/$1/manifests/$TAG")" = 200 ]
}

echo "[3/4] enviando o flake e os binários"
LOGDIR="$(mktemp -d)"
STAGE="$(mktemp -d)"
tar -czf "$STAGE/src.tar.gz" -C deploy/playground flake.nix flake.lock bin
ls -lh "$STAGE/src.tar.gz" | awk '{print "pacote:", $5}'
put "$STAGE/src.tar.gz" /rc-playground-src.tar.gz
put "$ROOT/scripts/playground-image-remote.sh" /rc-playground-remote.sh
rm -rf "$STAGE"

# build_push constrói um atributo e o publica no app; o token vai por arquivo (0600, apagado pelo script remoto).
build_push() {
  local attr="$1" app="$2" tok i
  for i in $(seq 1 "$SSH_TRIES"); do
    tok="$(mktemp)"
    chmod 600 "$tok"
    printf '%s' "$FLY_API_TOKEN" >"$tok"
    if put "$tok" /token; then
      rm -f "$tok"
      # 40 min: a primeira imagem do visitante baixa Node, Python e o Claude Code; as seguintes saem do cache.
      if timeout 2400s fly ssh console -a "$BUILDER" --machine "$MACHINE" -C "sh /rc-playground-remote.sh $attr $app $TAG" || published "$app"; then
        published "$app" || { echo "🛑 o build terminou mas registry.fly.io/$app:$TAG não existe" >&2; return 1; }
        echo "✅ registry.fly.io/$app:$TAG"
        mkdir -p "$ROOT/.playground"
        echo "$TAG" >"$ROOT/.playground/$attr.tag"
        return 0
      fi
    fi
    rm -f "$tok"
    echo "↻ build/push de $attr caiu ($i/$SSH_TRIES)" >&2
    sleep "$SSH_GAP"
  done
  echo "🛑 build/push de $attr falhou $SSH_TRIES vezes" >&2
  return 1
}

echo "[4/4] construindo e publicando ($WHICH)"
case "$WHICH" in
  visitor) build_push visitor "$VISITOR_APP" ;;
  front) build_push front "$FRONT_APP" ;;
  all) build_push visitor "$VISITOR_APP" && build_push front "$FRONT_APP" ;;
  *) echo "uso: $0 [visitor|front|all]" >&2; exit 2 ;;
esac
