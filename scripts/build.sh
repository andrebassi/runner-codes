#!/usr/bin/env bash
# Compila o host (SO atual) e o guest (Linux, mesma arquitetura) em bin/, e no macOS
# assina o host com o entitlement do hypervisor, sem o qual krun_start_enter falha.
set -euo pipefail
cd "$(dirname "$0")/.."
version=${VERSION:-$(git describe --tags --always --dirty 2>/dev/null || echo dev)}
arch=$(go env GOARCH)
mkdir -p bin
CGO_ENABLED=0 go build -ldflags "-s -w -X main.Version=${version}" -o bin/rc ./cmd/rc
# O guest roda dentro da VM: sempre Linux, estático, da mesma arquitetura do host
# (a libkrun não emula outra arquitetura). É outro main: o host importa purego, que
# força link dinâmico no Linux.
CGO_ENABLED=0 GOOS=linux GOARCH="$arch" go build -ldflags "-s -w -X main.Version=${version}" \
  -o "bin/rc-guest-linux-${arch}" ./cmd/runner-guest
# Prova de que saiu estático: binário dinâmico não roda na imagem musl (medido: "not found").
if file "bin/rc-guest-linux-${arch}" | grep -q 'dynamically linked'; then
  echo "guest binary is dynamically linked; it will not run in the VM" >&2
  exit 1
fi
# O runtime de Kubernetes (containerd-shim-rc-v2) só existe para nó Linux: sai sempre linux/<arch>.
CGO_ENABLED=0 GOOS=linux GOARCH="$arch" go build -ldflags "-s -w -X main.Version=${version}" \
  -o "bin/containerd-shim-rc-v2-linux-${arch}" ./cmd/containerd-shim-rc-v2
if [ "$(uname -s)" = "Darwin" ]; then
  codesign --force --sign - --entitlements build/hv.entitlements bin/rc
fi
ls -la bin/rc "bin/rc-guest-linux-${arch}"
