#!/usr/bin/env bash
# Compila o host (SO atual) e o guest (Linux, mesma arquitetura) em bin/, e no macOS
# assina o host com o entitlement do hypervisor, sem o qual krun_start_enter falha.
set -euo pipefail
cd "$(dirname "$0")/.."
version=${VERSION:-$(git describe --tags --always --dirty 2>/dev/null || echo dev)}
arch=$(go env GOARCH)
mkdir -p bin
CGO_ENABLED=0 go build -ldflags "-s -w -X main.Version=${version}" -o bin/runner.code ./cmd/runner.code
# O guest roda dentro da VM: sempre Linux, estático, da mesma arquitetura do host
# (a libkrun não emula outra arquitetura). É outro main: o host importa purego, que
# força link dinâmico no Linux.
CGO_ENABLED=0 GOOS=linux GOARCH="$arch" go build -ldflags "-s -w -X main.Version=${version}" \
  -o "bin/runner.code-guest-linux-${arch}" ./cmd/runner-guest
# Prova de que saiu estático: binário dinâmico não roda na imagem musl (medido: "not found").
if file "bin/runner.code-guest-linux-${arch}" | grep -q 'dynamically linked'; then
  echo "guest saiu dinâmico; não roda na VM" >&2
  exit 1
fi
if [ "$(uname -s)" = "Darwin" ]; then
  codesign --force --sign - --entitlements build/hv.entitlements bin/runner.code
fi
ls -la bin/runner.code "bin/runner.code-guest-linux-${arch}"
