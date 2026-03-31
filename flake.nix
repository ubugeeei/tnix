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

        vp = pkgs.writeShellScriptBin "vp" ''
          if [ -x "$PWD/node_modules/.bin/vp" ]; then
            exec "$PWD/node_modules/.bin/vp" "$@"
          fi

          exec ${pkgs.pnpm}/bin/pnpm dlx vite-plus "$@"
        '';
      in
      {
        formatter = pkgs.nixfmt-rfc-style;

        devShells.default = pkgs.mkShell {
          packages = [
            haskellPackages.ghc
            pkgs.cabal-install
            pkgs.haskell-language-server
            pkgs.fourmolu
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
