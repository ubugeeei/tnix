import { defineConfig } from "vite-plus";

export default defineConfig({
  run: {
    tasks: {
      build: {
        command: "vp run build:haskell && pnpm --filter @tnix/vscode build && vp run build:zed",
      },
      check: {
        command: "vp run check:haskell && vp run test:haskell && pnpm --filter @tnix/vscode check && pnpm --filter @tnix/vscode test && vp run check:zed && vp run check:neovim",
      },
      fmt: {
        command: "vp run fmt:haskell && pnpm --filter @tnix/vscode fmt",
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
      "build:zed": {
        command: "cargo build --manifest-path editors/zed/Cargo.toml",
        cache: false,
      },
      "check:zed": {
        command: "cargo check --manifest-path editors/zed/Cargo.toml",
        cache: false,
      },
      "check:neovim": {
        command: "nvim --headless -u NONE -c \"lua vim.opt.runtimepath:append(vim.fn.getcwd() .. '/editors/neovim')\" -l editors/neovim/test/config_spec.lua",
        cache: false,
      }
    }
  }
});
