# Dependências nativas do runner.codes por Nix (sem Homebrew, sem Docker).
#
# `nix profile install .#krun` põe libkrun + libkrunfw em ~/.nix-profile/lib, que é onde o
# `rc` procura por padrão (cmd/rc/main.go, libPath).
#   - macOS arm64: derivações próprias em nix/ (o nixpkgs só tem Linux);
#   - Linux: derivações próprias em nix/*-linux.nix sobre as do nixpkgs — libkrun 1.19.6 com BLK, NET,
#     GPU e INPUT (a API de tela) e libkrunfw 5.6.2 (kernel com virtio-input) — e a lib movida de
#     lib64/ para lib/.
{
  description = "runner.codes: libkrun + libkrunfw";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { nixpkgs, ... }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      packages = forAll (
        pkgs:
        let
          # No Linux, a do nixpkgs (5.3.0) sai sem CONFIG_VIRTIO_INPUT: tela viva e teclado/mouse mortos
          # no desktop (medido numa VM do GCP em 24/10/2026). A 5.6.2 é a mesma do Mac.
          libkrunfw =
            if pkgs.stdenv.hostPlatform.isDarwin then
              pkgs.callPackage ./nix/libkrunfw-darwin.nix { }
            else
              pkgs.callPackage ./nix/libkrunfw-linux.nix { };
          virglrenderer =
            if pkgs.stdenv.hostPlatform.isDarwin then pkgs.callPackage ./nix/virglrenderer-darwin.nix { } else pkgs.virglrenderer;
          libkrun =
            if pkgs.stdenv.hostPlatform.isDarwin then
              pkgs.callPackage ./nix/libkrun-darwin.nix { inherit libkrunfw virglrenderer; }
            else
              # A do nixpkgs (1.17.4) não tem krun_set_display_backend nem GPU/INPUT ligados: a 1.19.6
              # com os mesmos remendos de tela 2D do Mac.
              pkgs.callPackage ./nix/libkrun-linux.nix { inherit libkrunfw; };
        in
        {
          inherit libkrun libkrunfw virglrenderer;
          # Um diretório lib/ só com as duas: é o que vai para o perfil.
          krun = pkgs.runCommand "krun-${libkrun.version}" { } ''
            mkdir -p $out/lib
            for d in ${libkrun}/lib ${libkrun}/lib64 ${libkrunfw}/lib ${libkrunfw}/lib64; do
              # -f: na libkrun do nixpkgs, lib/ e lib64/ trazem os mesmos arquivos (medido em 20/10/2026 no
              # primeiro build em Linux), e o segundo ln falhava com "File exists".
              [ -d "$d" ] && ln -sf "$d"/libkrun* $out/lib/
            done
            true
          '';
          default = libkrun;
        }
      );

      # `nix develop` dá o ambiente de desenvolvimento sem nada instalado no perfil global:
      # Go no mínimo do go.mod (1.26), task, coreutils (o `timeout` dos scripts) e o Node
      # do Docusaurus. A libkrun continua vindo do `.#krun`, que é o que o `rc` carrega.
      devShells = forAll (pkgs: {
        default = pkgs.mkShell {
          packages = [
            pkgs.go_1_26
            pkgs.go-task
            pkgs.coreutils
            pkgs.nodejs_24
          ];
          # O host carrega a libkrun por purego e o guest é estático: nada usa cgo. Com o
          # compilador C do mkShell no PATH, o Go ligaria cgo e o `GOOS=linux go vet`
          # quebraria compilando runtime/cgo contra o SDK do macOS.
          env.CGO_ENABLED = "0";
        };
      });
    };
}
