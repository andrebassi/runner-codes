#!/usr/bin/env bash
# Gate multi-alvo: cada alvo declarado tem de compilar, e o que está fora (Windows)
# tem de FALHAR com o motivo explícito. Alvo novo sem decisão reprova.
set -uo pipefail
cd "$(dirname "$0")/.."
supported="darwin/arm64 linux/amd64 linux/arm64"
refused="windows/amd64"
rc=0
for t in $supported; do
  if GOOS=${t%/*} GOARCH=${t#*/} CGO_ENABLED=0 go build -o /dev/null ./cmd/runner.code 2>/tmp/targets-gate.err; then
    echo "ok     $t compila"
  else
    echo "🛑     $t deveria compilar:"; cat /tmp/targets-gate.err; rc=1
  fi
done
for t in $refused; do
  if GOOS=${t%/*} GOARCH=${t#*/} CGO_ENABLED=0 go build -o /dev/null ./cmd/runner.code 2>/tmp/targets-gate.err; then
    echo "🛑     $t compilou, mas está fora do escopo (libkrun sem WHP)"; rc=1
  elif grep -q runnerCodesDoesNotSupportWindows /tmp/targets-gate.err; then
    echo "ok     $t recusado com o motivo declarado"
  else
    echo "🛑     $t falhou sem o motivo declarado:"; cat /tmp/targets-gate.err; rc=1
  fi
done
# Todo arquivo com build tag de SO tem de mirar um alvo com decisão.
for f in $(grep -rl '^//go:build' --include='*.go' internal cmd); do
  tag=$(sed -n 's#^//go:build ##p' "$f" | head -1)
  case "$tag" in
    darwin|linux|windows|'!linux'|e2e) ;;
    *) echo "🛑     $f usa build tag sem decisão de alvo: $tag"; rc=1 ;;
  esac
done
exit $rc
