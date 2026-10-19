#!/usr/bin/env bash
# Instala libkrun + libkrunfw. macOS: tap slp/krun (bottles arm64; 1.19.6 / 5.6.2 em
# 19/10/2026). Linux: nixpkgs (libkrun 1.19.5 / libkrunfw 5.5.0), sem Docker (regra local).
set -euo pipefail
case "$(uname -s)" in
  Darwin)
    # O brew recusa fórmula de tap não confiado ("Refusing to load formula ... untrusted tap").
    brew tap slp/krun
    brew trust slp/krun
    HOMEBREW_NO_AUTO_UPDATE=1 brew install slp/krun/libkrunfw slp/krun/libkrun
    ls -la /opt/homebrew/lib/libkrun.dylib /opt/homebrew/lib/libkrunfw.dylib
    ;;
  Linux)
    nix profile install nixpkgs#libkrun nixpkgs#libkrunfw
    echo "aponte RUNNER_KRUN_LIB para \$HOME/.nix-profile/lib/libkrun.so.1 se o loader não achar"
    ;;
  *)
    echo "SO sem libkrun upstream: $(uname -s)" >&2
    exit 1
    ;;
esac
