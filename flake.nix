{
  description = "tnix: a gradual type system and tooling stack for Nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        haskellPackages = pkgs.haskellPackages;
        tnixCoreSource = pkgs.runCommand "tnix-core-source" { } ''
          mkdir -p "$out"
          cp -R ${./packages/tnix-core}/. "$out"/
          chmod -R u+w "$out"
          cp -R ${./registry} "$out/registry"
        '';
        tnixHaskellPackages =
          haskellPackages.extend
            (
              hfinal: _:
                {
                  "tnix-core" = hfinal.callCabal2nix "tnix-core" tnixCoreSource { };
                  "tnix-cli" = hfinal.callCabal2nix "tnix-cli" ./packages/tnix-cli { };
                  "tnix-lsp" = hfinal.callCabal2nix "tnix-lsp" ./packages/tnix-lsp { };
                }
            );
        tnixCli = tnixHaskellPackages."tnix-cli";
        tnixLsp = tnixHaskellPackages."tnix-lsp";

        vp = pkgs.writeShellScriptBin "vp" ''
          if [ -x "$PWD/node_modules/.bin/vp" ]; then
            exec "$PWD/node_modules/.bin/vp" "$@"
          fi

          exec ${pkgs.pnpm}/bin/pnpm dlx vite-plus "$@"
        '';
      in
      {
        formatter = pkgs.nixfmt-rfc-style;

        packages = {
          default = tnixCli;
          tnix = tnixCli;
          tnix-lsp = tnixLsp;
          tnix-core = tnixHaskellPackages."tnix-core";
        };

        apps = {
          default = self.apps.${system}.tnix;
          tnix = {
            type = "app";
            program = "${tnixCli}/bin/tnix";
          };
          tnix-lsp = {
            type = "app";
            program = "${tnixLsp}/bin/tnix-lsp";
          };
        };

        devShells.default = pkgs.mkShell {
          packages = [
            haskellPackages.ghc
            pkgs.cabal-install
            pkgs.haskell-language-server
            pkgs.fourmolu
            pkgs.neovim
            pkgs.pkg-config
            pkgs.zlib
            pkgs.nodejs_24
            pkgs.pnpm
            pkgs.rustc
            pkgs.cargo
            pkgs.rust-analyzer
            pkgs.git
            vp
          ];

          shellHook = ''
            export LANG=C.UTF-8
            export LC_ALL=C.UTF-8
            echo "tnix dev shell ready"
            echo "  - Haskell: cabal / ghc / hls"
            echo "  - Tasks: pnpm / vp"
            echo "  - Editors: Node.js / Rust"
          '';
        };
      });
}
