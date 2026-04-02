import { defineConfig } from "vite-plus";

export default defineConfig({
  run: {
    tasks: {
      build: {
        command: "vp run build:haskell && pnpm --filter tnix build && vp run build:zed",
      },
      check: {
        command: "vp run check:versions && vp run check:haskell && vp run test:haskell && vp run check:dogfood && vp run check:examples && pnpm --filter tnix check && pnpm --filter tnix test && vp run check:zed && vp run test:zed && vp run check:neovim",
      },
      fmt: {
        command: "vp run fmt:haskell && pnpm --filter tnix fmt",
      },
      ide: {
        command: "node --experimental-strip-types ./scripts/install-ide.ts",
        cache: false,
      },
      cli: {
        command: "node --experimental-strip-types ./scripts/install-cli.ts",
        cache: false,
      },
      "build:haskell": {
        command: "cabal build all",
      },
      "check:haskell": {
        command: "cabal build all",
      },
      "test:haskell": {
        command: "cabal test all",
      },
      "fmt:haskell": {
        command:
          "if rg --files -g '*.hs' >/dev/null 2>&1; then fourmolu -m inplace $(rg --files -g '*.hs'); else echo 'no haskell sources'; fi",
        cache: false,
      },
      "check:versions": {
        command: "node --experimental-strip-types ./scripts/check-version-sync.ts",
        cache: false,
      },
      "check:dogfood": {
        command: "cabal run tnix -- check ./dogfood/flake-surface.tnix",
        cache: false,
      },
      "check:examples": {
        command: "cabal run tnix -- check-project ./examples --format json",
        cache: false,
      },
      "build:zed": {
        command: "cargo build --manifest-path editors/zed/Cargo.toml",
        cache: false,
      },
      "check:zed": {
        command: "cargo check --manifest-path editors/zed/Cargo.toml",
        cache: false,
      },
      "test:zed": {
        command: "cargo test --manifest-path editors/zed/Cargo.toml",
        cache: false,
      },
      "check:neovim": {
        command: "nvim --headless -u NONE -c \"lua vim.opt.runtimepath:append(vim.fn.getcwd() .. '/editors/neovim')\" -l editors/neovim/test/config_spec.lua",
        cache: false,
      }
    }
  }
});
