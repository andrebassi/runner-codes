# libkrunfw 5.6.2 (kernel 6.12.109) para Linux: o kernel do convidado das microVMs.
#
# Por que não a do nixpkgs: o 26.05 traz a 5.3.0, cujo kernel sai com "# CONFIG_VIRTIO_INPUT is not
# set" (config-libkrunfw_x86_64 da tag). Os dispositivos de teclado e mouse aparecem no barramento
# virtio do convidado (id 0x0012, medido), mas sem o driver não existe /dev/input, o Xorg não acha
# entrada nenhuma e o desktop fica com a tela viva e o mouse parado no meio. A 5.6.2 — a mesma do
# runner.codes no Mac — liga CONFIG_VIRTIO_INPUT=y.
#
# Compilado do fonte nas duas arquiteturas Linux (o "prebuilt" da release é só para o Mac): uma vez.
# `variant` repassa o que a libkrun pede (ela chama libkrunfw.override { variant = ...; }).
{ stdenv, libkrunfw, fetchFromGitHub, fetchurl, variant ? null }:
let
  # O arquivo de config do kernel é por arquitetura (config-libkrunfw_x86_64 / _aarch64).
  arch = if stdenv.hostPlatform.isx86_64 then "x86_64" else "aarch64";
in
(libkrunfw.override { inherit variant; }).overrideAttrs (old: rec {
  version = "5.6.2";
  src = fetchFromGitHub {
    owner = "containers";
    repo = "libkrunfw";
    tag = "v${version}";
    hash = "sha256-HklZgZPjXe+eAGzRulEwRR1eo83tGlZBTRooCv0/ADU=";
  };
  # KERNEL_VERSION do Makefile da 5.6.2.
  kernelSrc = fetchurl {
    url = "mirror://kernel/linux/kernel/v6.x/linux-6.12.109.tar.xz";
    hash = "sha256-VITlUqM04VAZ9K66ieW1jwRlHPL04k4E3p8VLxw44/o=";
  };
  # O driver que faltava: derruba o build se a config da tag não o ligar.
  preBuild = (old.preBuild or "") + ''
    grep -q '^CONFIG_VIRTIO_INPUT=y' config-libkrunfw_${arch}
  '';
})
