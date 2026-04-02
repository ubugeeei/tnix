# tnix

`tnix` is a gradual type system and tooling stack for Nix. It compiles `.tnix` to `.nix`, provides static checking, and emits `.d.tnix` declaration files. It has no runtime and is intentionally limited to complementing existing Nix semantics rather than replacing them.

## Goals

- Add types to `.nix` without breaking its syntax or culture
- Allow ambient declarations for existing `.nix` files
- Adopt the parts of the TypeScript strategy that worked
  - incremental adoption
  - semantic preservation
  - strong editor tooling
- Be expressive enough for type-level programming
  - parametric polymorphism
  - higher-kinded types
  - conditional types
  - `infer`-style type decomposition
- Use structural subtyping and `dynamic`, `unknown`, and `any` as the core gradual typing tools

## File Kinds

- `.tnix`
  - source files with type annotations
  - compiled to `.nix`
- `.d.tnix`
  - declaration-only ambient files
  - used to type existing `.nix` modules and external code
- `.nix`
  - runtime artifact
  - `tnix` does not change its behavior

## Example

```tnix
type Option a = { _tag :: "some"; value :: a; } | { _tag :: "none"; };

declare "./legacy/default.nix" {
  mkPkg :: { name :: String; version :: String; } -> Derivation;
};

let
  map :: forall f a b. Functor f => (a -> b) -> f a -> f b;
  map = f: xs: builtins.map f xs;
in
  import ./legacy/default.nix {
    name = "hello";
    version = "1.0.0";
  }
```

After compilation, type information is erased and only ordinary Nix code remains.

## Design Docs

- [Getting Started](./docs/getting-started.md)
- [Language Reference](./docs/language-reference.md)
- [Language Design](./docs/language-design.md)
- [Type System](./docs/type-system.md)
- [Architecture](./docs/architecture.md)
- [Roadmap](./docs/roadmap.md)

## Current Status

`tnix` is now in its first integrated toolchain release.

- Haskell monorepo for parser, checker, compiler, emitter, CLI, and LSP
- Nix-based development environment
- `pnpm`-managed editor tooling
- VS Code, Zed, and Neovim integrations
- gradual typing with ambient declarations, HKT support, indexed `Vec` / `Matrix` / `Tensor`, and heterogeneous `Tuple`
- numeric singleton/primitive support via `Float`, `Number`, `Nat`, `Range`, and `Unit`
- explicit `expr as Type` casts for widening, narrowing, and gradual-boundary assertions
- TypeScript-style checker directives via `# @tnix-ignore` and `# @tnix-expected`
- project bootstrapping via `tnix init`, `tnix scaffold`, and `tnix.config.tnix`
- shipped declaration files for `builtins`, `flake.nix`, and `tnix.config.tnix`
- bundled declaration packs under `registry/` for workspace files and popular Nix ecosystem surfaces

## Installation

Install the CLI and language server from the published flake:

```bash
nix profile install github:ubugeeei/tnix#tnix
nix profile install github:ubugeeei/tnix#tnix-lsp
```

Or download the prebuilt archives attached to each GitHub release and place
`tnix` / `tnix-lsp` somewhere on your `PATH`.

Quick verification:

```bash
tnix --version
tnix-lsp --version
tnix check ./examples/main.tnix
```

For local development, enter the reproducible shell first:

```bash
nix develop
vp run check
```

## Bundled Registry Packs

The repository includes reusable `.d.tnix` packs under `registry/` so projects
can vendor common declarations instead of rewriting ambient files from scratch.

Workspace-oriented packs live under `registry/workspace/`:

- `registry/workspace/builtins.d.tnix`
- `registry/workspace/flake.d.tnix`
- `registry/workspace/tnix.config.d.tnix`

Ecosystem alias packs live under `registry/ecosystem/`:

- `registry/ecosystem/nixpkgs-lib.d.tnix`
- `registry/ecosystem/nixpkgs-pkgs.d.tnix`
- `registry/ecosystem/flake-ecosystem.d.tnix`
- `registry/ecosystem/community-flakes.d.tnix`

Typical usage is to copy the pack you want into your declaration directory and
reuse its aliases from local `declare` blocks:

```tnix
declare "./flake-utils.nix" { default :: NixFlakeUtilsFlake; };
declare "./devenv.nix" { default :: DevenvFlake; };
declare "./treefmt-nix.nix" { default :: TreefmtNixFlake; };
```

The workspace packs assume they live under `registry/workspace/` so their
relative `declare` targets resolve back to the project root.

See [CHANGELOG.md](./CHANGELOG.md) for the release history.

## Distribution

The primary distribution channel is GitHub Releases. Tagged releases publish
prebuilt `tnix` and `tnix-lsp` archives for supported platforms together with
checksums, plus a packaged VS Code `.vsix` extension. When marketplace tokens
are configured, the same tag also publishes the extension to VS Code
Marketplace and Open VSX.

The flake also exports installable packages and runnable apps:

```bash
nix build github:ubugeeei/tnix#tnix
nix run github:ubugeeei/tnix#tnix -- check ./main.tnix
nix run github:ubugeeei/tnix#tnix-lsp
```

See [RELEASING.md](./RELEASING.md) for the release flow.
