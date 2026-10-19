# libkrun para macOS arm64 (Hypervisor.framework). O nixpkgs só empacota a libkrun para Linux.
#
# Três desvios do Makefile upstream, todos para caber no sandbox do Nix (sem rede, sem /usr/bin):
#   1. o `init` do guest (C, Linux estático) é compilado com `zig cc -target aarch64-linux-musl`
#      e entregue por KRUN_INIT_BINARY_PATH. O Makefile, no Mac, baixaria um sysroot do Debian
#      (curl dentro do build) e usaria /usr/bin/clang + lld do sistema;
#   2. SYSROOT_LINUX aponta para um caminho qualquer só para desligar esse download;
#   3. a libkrun abre a libkrunfw por nome (`dlopen("libkrunfw.5.dylib")`), o que obrigava
#      DYLD_LIBRARY_PATH em todo processo `vmm`. O nome vira o caminho absoluto no store.
{
  lib,
  stdenv,
  fetchurl,
  rustPlatform,
  cargo,
  rustc,
  zig,
  libkrunfw,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "libkrun";
  version = "1.19.6";

  src = fetchurl {
    url = "https://github.com/containers/libkrun/archive/refs/tags/v${finalAttrs.version}.tar.gz";
    # Mesmo sha256 que a fórmula slp/krun conferia para a 1.19.6.
    hash = "sha256:7025d72208172dc06f791ad5af7ccaafeabdac9869f74ccb45fb6d4f6e5991cd";
  };

  cargoDeps = rustPlatform.fetchCargoVendor {
    inherit (finalAttrs) src;
    hash = "sha256-SPlozqdmX0khawoFjZrqYjQ5qDY4tSVa7gpehYHUTz8=";
  };

  nativeBuildInputs = [
    rustPlatform.cargoSetupHook
    cargo
    rustc
  ];

  postPatch = ''
    substituteInPlace src/libkrun/src/lib.rs \
      --replace-fail 'const KRUNFW_NAME: &str = "libkrunfw.5.dylib";' \
                     'const KRUNFW_NAME: &str = "${libkrunfw}/lib/libkrunfw.5.dylib";'
    # Sem VARIANT, o Makefile faz `mv libkrun.dylib libkrun.dylib`: o mv do BSD aceita, o do
    # coreutils (o do stdenv) recusa com "are the same file".
    sed -i '/mv target\/release\/libkrun.dylib target\/release\/$(KRUN_BASE_$(OS))/d' Makefile
  '';

  preBuild = ''
    # O zig grava cache no HOME; no sandbox o HOME não existe.
    export ZIG_GLOBAL_CACHE_DIR=$TMPDIR/zig-cache ZIG_LOCAL_CACHE_DIR=$TMPDIR/zig-cache
    # zig fora do nativeBuildInputs: o setup hook dele troca o buildPhase por `zig build`.
    ${lib.getExe zig} cc -target aarch64-linux-musl -O2 -static -Wall \
      -o $TMPDIR/krun-init src/init_blob/init/init.c src/init_blob/init/dhcp.c
    export KRUN_INIT_BINARY_PATH=$TMPDIR/krun-init
  '';

  # BLK=1 liga virtio-blk: sem ele não existe krun_add_disk2, e o root read-only das VMs de job
  # depende disso. Rede (NET), GPU e som ficam de fora: o runner não usa.
  makeFlags = [
    "PREFIX=${placeholder "out"}"
    "BLK=1"
    "SYSROOT_LINUX=/nonexistent"
  ];

  meta = {
    description = "VMM em biblioteca (Hypervisor.framework), sem rede/GPU, com virtio-blk";
    homepage = "https://github.com/containers/libkrun";
    license = lib.licenses.asl20;
    platforms = [ "aarch64-darwin" ];
  };
})
