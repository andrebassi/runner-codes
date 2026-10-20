{
  # Imagens do playground público no Fly (claude-in-chrome.runner.codes), montadas pelo Nix com nix2container no
  # builder x86_64 do Fly (scripts/playground-image.sh), no mesmo padrão do site andrebassi:
  #   nix build .#visitor  -> a máquina de cada visitante: rc-visitor + Claude Code + git/python/node
  #   nix build .#front    -> o front: rc-playground + cloudflared
  # Os dois binários Go chegam prontos em ./bin (estáticos, CGO desligado, compilados no Mac por
  # scripts/playground-image.sh): compilar Go aqui pediria vendorHash a cada mudança de go.sum.
  description = "runner.codes playground images (Fly Machines)";

  inputs = {
    # A mesma revisão do flake do site (scripts/playground-image.sh confere): o store do builder já tem o fechamento
    # dela, então só baixa o que é novo (Claude Code, Node).
    nixpkgs.url = "github:NixOS/nixpkgs/55bfb4d7437bd4e09ab7cba58a43bd68eb310044";
    # Por URL de tarball e não github: a forma github: passa pela API sem autenticação e cai no limite de taxa.
    nix2container.url = "https://github.com/nlewo/nix2container/archive/master.tar.gz";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, nix2container, ... }:
    let
      system = "x86_64-linux";
      # unfree só para o Claude Code (licença proprietária da Anthropic; o visitante faz login com a conta dele).
      pkgs = import nixpkgs { inherit system; config.allowUnfreePredicate = p: (nixpkgs.lib.getName p) == "claude-code"; };
      n2c = nix2container.packages.${system}.nix2container;

      # bin/<nome> copiado para o store como pacote (o fonte só tem os dois binários já compilados).
      prebuilt = name: pkgs.runCommand name { } ''
        install -Dm755 ${./bin + "/${name}"} $out/bin/${name}
      '';
      rcVisitor = prebuilt "rc-visitor";
      rcPlayground = prebuilt "rc-playground";

      # root e o visitante (uid 1000), com shell de login: é o bash da aba shell do terminal web.
      nss = pkgs.dockerTools.fakeNss.override {
        extraPasswdLines = [ "visitor:x:1000:1000:visitor:/data/home:/bin/bash" ];
        extraGroupLines = [ "visitor:x:1000:" ];
      };
      # /usr/bin/env: o shebang de quase todo script npm e python que o visitante instalar.
      usrEnv = pkgs.runCommand "usr-bin-env" { } ''
        mkdir -p $out/usr/bin
        ln -s ${pkgs.coreutils}/bin/env $out/usr/bin/env
      '';

      # O que o visitante tem no PATH (/bin): o Claude Code e o mínimo para programar e instalar o resto.
      tools = with pkgs; [
        bashInteractive coreutils findutils gnugrep gnused gawk gnutar gzip xz which less procps
        git curl cacert openssh ripgrep jq nano
        nodejs_22 python3 claude-code
        # iptables é do rc-visitor (root), não do visitante: ele não tem permissão para mexer nas regras.
        iptables
      ];
      visitorRoot = pkgs.buildEnv { name = "visitor-root"; paths = [ nss usrEnv rcVisitor ] ++ tools; pathsToLink = [ "/bin" "/etc" "/usr" "/share" ]; };

      # O front sobe o túnel e o rc-playground; se qualquer um sair, o outro é derrubado e a máquina reinicia
      # (restart always no fly.toml), em vez de ficar meio no ar.
      frontEntry = pkgs.writeShellScriptBin "front-entrypoint" ''
        set -u
        ${pkgs.cloudflared}/bin/cloudflared tunnel --no-autoupdate run --token "$TUNNEL_TOKEN" &
        tunnel=$!
        ${rcPlayground}/bin/rc-playground &
        front=$!
        trap 'kill $tunnel $front 2>/dev/null' TERM INT
        wait -n $tunnel $front
        kill $tunnel $front 2>/dev/null
        exit 1
      '';
      frontRoot = pkgs.buildEnv { name = "front-root"; paths = [ (pkgs.dockerTools.fakeNss) pkgs.bashInteractive pkgs.coreutils ]; pathsToLink = [ "/bin" "/etc" ]; };
      sslEnv = "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    in
    {
      packages.${system} = {
        visitor = n2c.buildImage {
          name = "rc-visitor";
          tag = "nix";
          copyToRoot = [ visitorRoot ];
          # A camada grande (Node, Python, Claude Code) muda pouco; o rc-visitor muda a cada deploy e vai sozinho.
          layers = [
            (n2c.buildLayer { deps = tools ++ [ nss usrEnv ]; })
            (n2c.buildLayer { deps = [ rcVisitor ]; })
          ];
          config = {
            Entrypoint = [ "${rcVisitor}/bin/rc-visitor" ];
            Env = [
              "PATH=/data/home/.npm-global/bin:/data/home/.local/bin:/bin"
              sslEnv
              "NODE_EXTRA_CA_CERTS=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              "GIT_SSL_CAINFO=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              # npm -g e pip --user vão para o volume (o store do Nix é só leitura).
              "NPM_CONFIG_PREFIX=/data/home/.npm-global"
              "RC_DATA=/data"
              "RC_UID=1000"
              "LANG=C.UTF-8"
            ];
          };
        };
        front = n2c.buildImage {
          name = "rc-playground";
          tag = "nix";
          copyToRoot = [ frontRoot ];
          layers = [
            (n2c.buildLayer { deps = [ pkgs.cloudflared pkgs.cacert pkgs.bashInteractive ]; })
            (n2c.buildLayer { deps = [ rcPlayground ]; })
          ];
          config = {
            Entrypoint = [ "${frontEntry}/bin/front-entrypoint" ];
            Env = [ sslEnv ];
          };
        };
      };
    };
}
