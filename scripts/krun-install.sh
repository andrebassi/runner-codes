#!/usr/bin/env bash
# Instala libkrun + libkrunfw pelo Nix (sem Homebrew), nos dois sistemas: o flake do repo
# expõe `.#krun`, um lib/ com as duas, e o `rc` procura em ~/.nix-profile/lib por padrão.
#   - macOS arm64: derivações próprias em nix/ (libkrun 1.19.6, libkrunfw 5.6.2); o nixpkgs
#     só empacota a libkrun para Linux. A 1ª vez compila a libkrun (Rust, ~10 min);
#   - Linux: nixpkgs com virtio-blk ligado.
# Reinstalar sobre uma versão anterior troca a entrada do perfil em vez de duplicar.
set -euo pipefail
cd "$(dirname "$0")/.."
case "$(uname -s)" in
  Darwin | Linux) ;;
  *)
    echo "OS without upstream libkrun: $(uname -s)" >&2
    exit 1
    ;;
esac
# `nix profile` identifica a entrada pelo nome do atributo; remover antes deixa a operação
# idempotente (o `add` recusa um segundo pacote com os mesmos arquivos).
nix profile remove krun >/dev/null 2>&1 || true
nix profile add .#krun
ls -la "$HOME"/.nix-profile/lib/libkrun* "$HOME"/.nix-profile/lib/libkrunfw*
