# virglrenderer com Venus (Vulkan-over-virtio) para macOS arm64, consumido pela libkrun com GPU=1.
#
# O virglrenderer do nixpkgs sai com `-Dvenus=false` no macOS, e é justamente o Venus que dá
# Vulkan ao guest. Usa o fork do autor da libkrun (`slp/virglrenderer`, tag 0.10.4e-krunkit), o
# mesmo da fórmula `slp/krun` que o krunkit usam: ele traz os ajustes para o Venus
# rodar sobre o MoltenVK (Vulkan → Metal).
{
  lib,
  stdenv,
  fetchurl,
  meson,
  ninja,
  pkg-config,
  python3,
  libepoxy,
  moltenvk,
  vulkan-headers,
  vulkan-loader,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "virglrenderer-krun";
  version = "0.10.4e";

  src = fetchurl {
    url = "https://gitlab.freedesktop.org/slp/virglrenderer/-/archive/0.10.4e-krunkit/virglrenderer-0.10.4e-krunkit.tar.gz";
    # Mesmo sha256 que a fórmula slp/krun conferia (medido de novo em 19/10/2026).
    hash = "sha256:09d000623fbdb966cb604eb48c962a0815e8142383e6066d6494809335b76dbb";
  };

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    python3
  ];

  buildInputs = [
    libepoxy
    moltenvk
    vulkan-headers
    vulkan-loader
  ];

  # Sem render server: no macOS o virglrenderer roda dentro do próprio processo `vmm`
  # (a fórmula do slp faz igual); o render server separado é coisa do Linux.
  # O drm é `feature: auto`, e o hook de meson do Nix liga toda feature auto
  # (`-Dauto_features=enabled`): sem desligar, o configure exige libdrm, que só existe no Linux.
  mesonFlags = [
    "-Dvenus=true"
    "-Drender-server=false"
    "-Ddrm=disabled"
  ];

  meta = {
    description = "virglrenderer com Venus (Vulkan) sobre MoltenVK, para a libkrun no macOS";
    homepage = "https://gitlab.freedesktop.org/slp/virglrenderer";
    license = lib.licenses.mit;
    platforms = [ "aarch64-darwin" ];
  };
})
