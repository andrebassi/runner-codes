# libkrunfw para macOS arm64: o kernel Linux do guest embutido numa dylib.
# O nixpkgs só empacota a libkrunfw para Linux. Compilar o kernel no Mac exige toolchain
# LLVM cruzada; o upstream publica o kernel já gerado como C (`kernel.c`) no tarball
# "prebuilt", e o `make` só compila esse C numa dylib — o mesmo que a fórmula slp/krun fazia.
{
  lib,
  stdenv,
  fetchurl,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "libkrunfw";
  version = "5.6.2";

  src = fetchurl {
    url = "https://github.com/containers/libkrunfw/releases/download/v${finalAttrs.version}/libkrunfw-prebuilt-aarch64.tgz";
    # Mesmo sha256 que a fórmula slp/krun conferia para a 5.6.2.
    hash = "sha256:adef6739cc3bda2a1b57f04c08ccbfdf206a6c831d76b32de1beddc0e7984e86";
  };

  makeFlags = [ "PREFIX=${placeholder "out"}" ];

  meta = {
    description = "Kernel Linux do guest da libkrun, empacotado como dylib (macOS arm64)";
    homepage = "https://github.com/containers/libkrunfw";
    license = with lib.licenses; [
      gpl2Only
      lgpl21Only
    ];
    platforms = [ "aarch64-darwin" ];
  };
})
