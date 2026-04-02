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
        haskellLib = pkgs.haskell.lib;
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
        tnixCore = tnixHaskellPackages."tnix-core";
        tnixCli = tnixHaskellPackages."tnix-cli";
        tnixLsp = tnixHaskellPackages."tnix-lsp";
        tnixToolchain = pkgs.runCommand "tnix-toolchain" { } ''
          mkdir -p "$out/bin"
          ln -s ${tnixCli}/bin/tnix "$out/bin/tnix"
          ln -s ${tnixLsp}/bin/tnix-lsp "$out/bin/tnix-lsp"
        '';
        tnixCoreChecked = haskellLib.doCheck tnixCore;
        tnixCliChecked = haskellLib.doCheck tnixCli;
        tnixLspChecked = haskellLib.doCheck tnixLsp;

        versionMetadataCheck = pkgs.runCommand "tnix-version-metadata-check" { nativeBuildInputs = [ pkgs.nodejs_24 ]; } ''
          export HOME="$TMPDIR"
          workspace="$TMPDIR/version-check"
          mkdir -p "$workspace/scripts" "$workspace/editors/vscode" "$workspace/packages/tnix-core" "$workspace/packages/tnix-cli" "$workspace/packages/tnix-lsp"
          cp ${./scripts/check-version-sync.ts} "$workspace/scripts/check-version-sync.ts"
          cp ${./package.json} "$workspace/package.json"
          cp ${./CHANGELOG.md} "$workspace/CHANGELOG.md"
          cp ${./editors/vscode/package.json} "$workspace/editors/vscode/package.json"
          cp ${./packages/tnix-core/tnix-core.cabal} "$workspace/packages/tnix-core/tnix-core.cabal"
          cp ${./packages/tnix-cli/tnix-cli.cabal} "$workspace/packages/tnix-cli/tnix-cli.cabal"
          cp ${./packages/tnix-lsp/tnix-lsp.cabal} "$workspace/packages/tnix-lsp/tnix-lsp.cabal"
          cd "$workspace"
          node --experimental-strip-types ./scripts/check-version-sync.ts
          touch "$out"
        '';

        cliSmokeCheck = pkgs.runCommand "tnix-cli-smoke-check" { nativeBuildInputs = [ tnixCli tnixLsp ]; } ''
          export HOME="$TMPDIR"
          tnix --version >/dev/null
          tnix-lsp --version >/dev/null
          touch "$out"
        '';

        repoFixturesCheck = pkgs.runCommand "tnix-repo-fixtures-check" { nativeBuildInputs = [ tnixCli ]; } ''
          export HOME="$TMPDIR"
          cd ${self}
          tnix check ./dogfood/flake-surface.tnix >/dev/null
          tnix check-project ./examples --format json >/dev/null
          touch "$out"
        '';

        vp = pkgs.writeShellScriptBin "vp" ''
          case "''${1-}" in
            ide|cli)
              command="$1"
              shift
              repo_root="$PWD"
              if command -v git >/dev/null 2>&1; then
                maybe_root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
                if [ -n "$maybe_root" ] && [ -f "$maybe_root/flake.nix" ]; then
                  repo_root="$maybe_root"
                fi
              fi
              exec ${pkgs.nodejs_24}/bin/node --experimental-strip-types "$repo_root/scripts/install-$command.ts" "$@"
              ;;
          esac

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
          tnix-core = tnixCore;
          tnix-toolchain = tnixToolchain;
        };

        checks = {
          tnix-core-tests = tnixCoreChecked;
          tnix-cli-tests = tnixCliChecked;
          tnix-lsp-tests = tnixLspChecked;
          version-metadata = versionMetadataCheck;
          cli-smoke = cliSmokeCheck;
          repo-fixtures = repoFixturesCheck;
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
