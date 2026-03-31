# Architecture

## Monorepo Strategy

Haskell is the core implementation language. Editor integrations stay thin and are written only in the languages required by each host editor.

### Haskell packages

- `tnix-core`
  - AST
  - parser
  - type checker
  - declaration emitter
  - `.tnix -> .nix` compiler
- `tnix-cli`
  - `compile`
  - `check`
  - `emit`
- `tnix-lsp`
  - diagnostics
  - hover
  - go to definition
  - declaration lookup

### Editor packages

- `editors/vscode`
  - TypeScript
  - managed by `pnpm`
  - built and checked with `vp`
- `editors/zed`
  - a thin Rust launcher
- `editors/neovim`
  - a thin Lua setup helper

## Development Environment

- environment provisioning with `nix`
- TypeScript package management with `pnpm`
- task runner with Vite+ via `vp`

`nix develop` is expected to provide:

- `ghc`
- `cabal`
- `haskell-language-server`
- `node`
- `pnpm`
- `vp`
- `rustc`
- `cargo`

## Compiler Pipeline

1. parse
   - parse `.tnix` and `.d.tnix` into ASTs
2. collect declarations
   - type aliases
   - ambient declarations
   - workspace declaration sidecars
3. type check
   - inference
   - subtype and consistency checks
   - diagnostics
4. emit
   - generate `.nix`
   - generate `.d.tnix`

## LSP Responsibilities

- parse and type-check the current `.tnix` buffer
- index workspace `.d.tnix` files
- resolve declarations for `import` targets
- publish diagnostics
- return inferred types and declaration origins in hover

## Editor Integration Strategy

### VS Code

- TextDocumentSync
- diagnostics push
- hover
- semantic tokens in a later phase

### Zed

- a minimal extension whose main job is starting `tnix-lsp`
- treat `.tnix` as a Nix-like language

### neovim

- a thin helper around `vim.lsp.start`
- no custom UI layer

## Test Strategy

The most important tests live in `tnix-core`.

- parser golden tests
- subtype and consistency tests
- inference tests
- conditional and `infer` tests
- declaration emitter tests
- compile erasure tests
- regression fixtures

The LSP and editor integrations stay thin and mainly verify startup, protocol wiring, and small regression cases.

## Quality Strategy

- prefer explainable errors
- keep generated `.nix` readable
- stabilize `.d.tnix` as an API contract format
- keep implementation files below 250 lines with clearly separated responsibilities
