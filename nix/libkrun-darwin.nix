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
  pkg-config,
  libkrunfw,
  virglrenderer,
  libepoxy,
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
    pkg-config
    # O crate de display gera bindings em C com bindgen quando GPU=1.
    rustPlatform.bindgenHook
  ];

  # GPU=1: o rutabaga_gfx acha o virglrenderer (com Venus) e o libepoxy por pkg-config.
  buildInputs = [
    virglrenderer
    libepoxy
  ];

  postPatch = ''
    substituteInPlace src/libkrun/src/lib.rs \
      --replace-fail 'const KRUNFW_NAME: &str = "libkrunfw.5.dylib";' \
                     'const KRUNFW_NAME: &str = "${libkrunfw}/lib/libkrunfw.5.dylib";'
    # Sem VARIANT, o Makefile faz `mv libkrun.dylib libkrun.dylib`: o mv do BSD aceita, o do
    # coreutils (o do stdenv) recusa com "are the same file".
    sed -i '/mv target\/release\/libkrun.dylib target\/release\/$(KRUN_BASE_$(OS))/d' Makefile
    # KRUN_GPU_BACKEND=2d escolhe o componente Rutabaga2D (framebuffer por CPU) no lugar do
    # virglrenderer. Com o virglrenderer só com Venus (sem virgl, o caso do macOS), o recurso
    # 2D do guest falha (TRANSFER_TO_HOST_2D -> ERR_UNSPEC, SET_SCANOUT -> INVALID_RESOURCE_ID,
    # medido) e a tela nunca chega ao backend de display. O rc liga a variável só na VM com
    # display, que assim perde o Venus.
    # Duas linhas iguais existem (o builder normal e o de fallback); o que distingue é a linha
    # seguinte (virgl_flags), por isso o N do sed. O grep derruba o build se a troca não
    # acontecer exatamente uma vez — patch que passa calado é pior que build quebrado.
    sed -i '/rutabaga_gfx::RutabagaComponentType::VirglRenderer,$/{N;s/rutabaga_gfx::RutabagaComponentType::VirglRenderer,\(\n *virgl_flags,\)/if env::var("KRUN_GPU_BACKEND").as_deref() == Ok("2d") { rutabaga_gfx::RutabagaComponentType::Rutabaga2D } else { rutabaga_gfx::RutabagaComponentType::VirglRenderer },\1/}' \
      src/devices/src/virtio/gpu/virtio_gpu.rs
    test "$(grep -c 'KRUN_GPU_BACKEND' src/devices/src/virtio/gpu/virtio_gpu.rs)" = 1
    # read_2d_resource fazia unwrap() no erro do transfer_read: um quadro pego no meio de uma troca
    # de modo (xrandr) pede mais bytes do que a memória que o guest associou ao recurso ("an iovec is
    # outside of guest memory's range"), o unwrap derruba a thread 'gpu worker', ninguém mais atende
    # a fila de controle e o Xorg do guest fica preso para sempre (medido: 30 trocas de tamanho
    # reproduzem). Com o erro devolvido, o flush_resource já loga e responde ERR_UNSPEC ao guest: o
    # quadro se perde e a tela segue. O grep derruba o build se o trecho mudar de forma.
    # sed com N (junta a linha do unwrap): string multilinha do Nix perde a indentação, e um
    # substituteInPlace com o trecho literal não casava.
    sed -i '/\.map_err(|e| format!("{e}"))$/{N;s/\.map_err(|e| format!("{e}"))\n *\.unwrap();/.map_err(|e| { log::error!("read_2d_resource {}: {e}", resource.id); ErrUnspec })?;/}' \
      src/devices/src/virtio/gpu/virtio_gpu.rs
    test "$(grep -c 'read_2d_resource {}: {e}' src/devices/src/virtio/gpu/virtio_gpu.rs)" = 1
    # Rutabaga2D: transfer_read e transfer_write tiram o info_2d (e as iovecs) do recurso com take()
    # e só devolvem no caminho feliz; um erro no transfer_2d sai pelo `?` com o recurso sem info_2d,
    # e TODO quadro seguinte daquele recurso falha com "invalid 2D info" (medido: depois de uma troca
    # de modo, a tela ficava parada para sempre, com o guest saudável). O patch devolve o que foi
    # tirado antes de propagar o erro; sed -z trata o arquivo inteiro como uma linha.
    sed -z -i \
      -e 's/        transfer_2d(\n/        let result = transfer_2d(\n/g' \
      -e 's/        )?;\n\n        resource.info_2d = Some(info_2d);\n        resource.backing_iovecs = Some(iovecs);\n        Ok(())/        );\n\n        resource.info_2d = Some(info_2d);\n        resource.backing_iovecs = Some(iovecs);\n        result/' \
      -e 's/        )?;\n\n        resource.info_2d = Some(info_2d);\n        Ok(())/        );\n\n        resource.info_2d = Some(info_2d);\n        result/' \
      -e 's/        let iovecs = resource\n            .backing_iovecs\n            .take()\n            .ok_or(RutabagaError::InvalidIovec)?;/        let iovecs = match resource.backing_iovecs.take() {\n            Some(v) => v,\n            None => {\n                resource.info_2d = Some(info_2d);\n                return Err(RutabagaError::InvalidIovec);\n            }\n        };/' \
      src/rutabaga_gfx/src/rutabaga_2d.rs
    test "$(grep -c 'let result = transfer_2d(' src/rutabaga_gfx/src/rutabaga_2d.rs)" = 2
    test "$(grep -c '^        result$' src/rutabaga_gfx/src/rutabaga_2d.rs)" = 2
    test "$(grep -c 'return Err(RutabagaError::InvalidIovec)' src/rutabaga_gfx/src/rutabaga_2d.rs)" = 1
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
  # depende disso. GPU=1 liga o virtio-gpu (Venus), usado só por VM que pede GPU. NET=1 liga a
  # virtio-net (krun_add_net_unixstream), que as máquinas com rede usam com o gateway do
  # runner (internal/adapters/netgw); VM sem rede não ganha a placa. Som fica de fora.
  makeFlags = [
    "PREFIX=${placeholder "out"}"
    "BLK=1"
    "GPU=1"
    "NET=1"
    # INPUT=1: virtio-input (teclado e mouse do display); sem ele krun_add_input_device é stub.
    "INPUT=1"
    "SYSROOT_LINUX=/nonexistent"
  ];

  meta = {
    description = "VMM em biblioteca (Hypervisor.framework), com virtio-blk, virtio-gpu (Venus) e virtio-net";
    homepage = "https://github.com/containers/libkrun";
    license = lib.licenses.asl20;
    platforms = [ "aarch64-darwin" ];
  };
})
