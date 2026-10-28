# libkrun 1.19.6 para Linux, com o que o desktop dos agentes precisa: virtio-gpu (a tela),
# virtio-input (teclado e mouse), virtio-blk (disco) e virtio-net (rede).
#
# Por que não a do nixpkgs: o 26.05 traz a 1.17.4, sem krun_set_display_backend (a API de tela que o
# rc usa) e com GPU e INPUT desligados. É a mesma versão e os mesmos remendos de tela 2D do
# runner.codes no Mac (nix/libkrun-darwin.nix), lá medidos:
#   - KRUN_GPU_BACKEND=2d escolhe o Rutabaga2D (framebuffer por CPU). Host sem GPU (VM de nuvem), e
#     o caminho do virglrenderer (Venus) não atende recurso 2D: sem isso a tela nunca chega;
#   - read_2d_resource sem unwrap: um quadro no meio de uma troca de tamanho derrubava a thread da
#     GPU e travava o Xorg do convidado para sempre;
#   - Rutabaga2D devolve o info_2d no erro: sem isso, depois de uma troca de modo a tela parava.
# Cada sed tem um `test` que derruba o build se a troca não acontecer — remendo que passa calado é
# pior que build quebrado.
# A libkrunfw (o kernel do convidado) vem de fora: a do nixpkgs não tem virtio-input (ver libkrunfw.nix).
{ libkrun, libkrunfw, fetchurl, rustPlatform, virglrenderer, libepoxy }:
(libkrun.override {
  inherit libkrunfw;
  withBlk = true;
  withNet = true;
  withGpu = true;
  withInput = true;
}).overrideAttrs (old: rec {
  version = "1.19.6";
  src = fetchurl {
    url = "https://github.com/containers/libkrun/archive/refs/tags/v${version}.tar.gz";
    # Mesmos hashes do build do Mac (o tarball e o vendor do cargo não dependem do sistema).
    hash = "sha256:7025d72208172dc06f791ad5af7ccaafeabdac9869f74ccb45fb6d4f6e5991cd";
  };
  cargoDeps = rustPlatform.fetchCargoVendor {
    inherit src;
    hash = "sha256-SPlozqdmX0khawoFjZrqYjQ5qDY4tSVa7gpehYHUTz8=";
  };
  buildInputs = (old.buildInputs or [ ]) ++ [ virglrenderer libepoxy ];
  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ rustPlatform.bindgenHook ];
  postPatch = (old.postPatch or "") + ''
    sed -i '/rutabaga_gfx::RutabagaComponentType::VirglRenderer,$/{N;s/rutabaga_gfx::RutabagaComponentType::VirglRenderer,\(\n *virgl_flags,\)/if env::var("KRUN_GPU_BACKEND").as_deref() == Ok("2d") { rutabaga_gfx::RutabagaComponentType::Rutabaga2D } else { rutabaga_gfx::RutabagaComponentType::VirglRenderer },\1/}' \
      src/devices/src/virtio/gpu/virtio_gpu.rs
    test "$(grep -c 'KRUN_GPU_BACKEND' src/devices/src/virtio/gpu/virtio_gpu.rs)" = 1

    sed -i '/\.map_err(|e| format!("{e}"))$/{N;s/\.map_err(|e| format!("{e}"))\n *\.unwrap();/.map_err(|e| { log::error!("read_2d_resource {}: {e}", resource.id); ErrUnspec })?;/}' \
      src/devices/src/virtio/gpu/virtio_gpu.rs
    test "$(grep -c 'read_2d_resource {}: {e}' src/devices/src/virtio/gpu/virtio_gpu.rs)" = 1

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
})
