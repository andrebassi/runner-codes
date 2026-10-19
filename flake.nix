# Dependências nativas do runner.codes por Nix (sem Homebrew, sem Docker).
#
# `nix profile install .#krun` põe libkrun + libkrunfw em ~/.nix-profile/lib, que é onde o
# `rc` procura por padrão (cmd/rc/main.go, libPath).
#   - macOS arm64: derivações próprias em nix/ (o nixpkgs só tem Linux);
#   - Linux: as do nixpkgs, com virtio-blk ligado (o padrão do nixpkgs vem sem BLK, e sem ele
#     não existe krun_add_disk2) e a lib movida de lib64/ para lib/.
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
          libkrunfw =
            if pkgs.stdenv.hostPlatform.isDarwin then pkgs.callPackage ./nix/libkrunfw-darwin.nix { } else pkgs.libkrunfw;
          libkrun =
            if pkgs.stdenv.hostPlatform.isDarwin then
              pkgs.callPackage ./nix/libkrun-darwin.nix { inherit libkrunfw; }
            else
              pkgs.libkrun.override { withBlk = true; };
        in
        {
          inherit libkrun libkrunfw;
          # Um diretório lib/ só com as duas: é o que vai para o perfil.
          krun = pkgs.runCommand "krun-${libkrun.version}" { } ''
            mkdir -p $out/lib
            for d in ${libkrun}/lib ${libkrun}/lib64 ${libkrunfw}/lib ${libkrunfw}/lib64; do
              [ -d "$d" ] && ln -s "$d"/libkrun* $out/lib/
            done
            true
          '';
          default = libkrun;
        }
      );
    };
}
