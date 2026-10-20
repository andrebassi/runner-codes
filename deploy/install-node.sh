#!/bin/bash
# Parte comum da instalação do rc como runtime de Kubernetes, em qualquer distribuição (k3s, microk8s):
# põe o rc, a libkrun, a imagem da VM do pod e o shim no lugar, e grava /etc/rc/shim.json. Quem liga ao
# containerd é o instalador de cada distribuição (deploy/k3s, deploy/microk8s), que chama este.
#
# Roda como root num nó Linux com /dev/kvm, a partir do repositório com `task build` feito e a libkrun
# instalada (task krun:install). Reexecutar é seguro: só sobrescreve os arquivos do rc.
set -euo pipefail
cd "$(dirname "$0")/.."
[ "$(id -u)" = 0 ] || { echo "rode como root" >&2; exit 1; }
[ -c /dev/kvm ] || { echo "nó sem /dev/kvm: a libkrun precisa de KVM" >&2; exit 1; }
arch=$(uname -m); case "$arch" in x86_64) arch=amd64 ;; aarch64) arch=arm64 ;; esac
PREFIX=${RC_PREFIX:-/opt/rc}
# A libkrun do perfil Nix (task krun:install) de quem compilou; LIB_SRC troca.
LIB_SRC=${LIB_SRC:-$HOME/.nix-profile/lib}
for f in bin/rc "bin/rc-guest-linux-$arch" "bin/containerd-shim-rc-v2-linux-$arch" "$LIB_SRC/libkrun.so.1" "$LIB_SRC/libkrunfw.so.5"; do
  [ -e "$f" ] || { echo "falta $f (task build / task krun:install)" >&2; exit 1; }
done
install -d -m 755 "$PREFIX/bin" "$PREFIX/lib" /etc/rc
install -m 755 bin/rc "$PREFIX/bin/rc"
# O guest vai ao lado do rc: o build da imagem o procura ali para injetar na VM.
install -m 755 "bin/rc-guest-linux-$arch" "$PREFIX/bin/rc-guest-linux-$arch"
# /usr/local/bin está no PATH do containerd do k3s e do microk8s: é por ele que o runtime_type
# io.containerd.rc.v2 acha o binário containerd-shim-rc-v2.
install -m 755 "bin/containerd-shim-rc-v2-linux-$arch" /usr/local/bin/containerd-shim-rc-v2
# cp -L: no perfil Nix são links para o /nix/store; a cópia leva o arquivo, e as dependências dele
# continuam achadas pelo RPATH no store.
cp -fL "$LIB_SRC"/libkrun.so* "$LIB_SRC"/libkrunfw.so* "$PREFIX/lib/"
# A VM do pod usa a imagem Alpine do catálogo (stack awk): sh, mount e o guest do rc, e nada mais.
out=$("$PREFIX/bin/rc" image build --stack awk 2>&1) || { echo "$out" >&2; exit 1; }
img=$(echo "$out" | tail -1 | awk '{print $2}')
[ -f "$img" ] || { echo "imagem da VM do pod não saiu: $img" >&2; exit 1; }
install -m 644 "$img" "$PREFIX/pod.ext4"
cat >/etc/rc/shim.json <<JSON
{"rc": "$PREFIX/bin/rc", "lib_dir": "$PREFIX/lib", "lib_path": "$PREFIX/lib/libkrun.so.1",
 "image": "$PREFIX/pod.ext4", "state_dir": "/var/lib/rc-shim"}
JSON
install -d -m 700 /var/lib/rc-shim
echo "rc instalado em $PREFIX; shim em /usr/local/bin/containerd-shim-rc-v2"
