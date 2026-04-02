# Changelog

## v0.3.0 - 2026-04-02

- Add project-aware CLI workflows for `check-project`, `build`, and `emit-project`, together with stronger config decoding and absolute root handling.
- Expand the LSP surface with richer editor features, cached analyzed documents, and better workspace-scale behavior.
- Ship a curated `examples/` showcase project and wire it into repeatable verification so sample code stays healthy in CI.
- Support `declarationPacks` in `tnix.config.tnix`, including direct consumption of upstream ecosystem packs and rebased `registry/workspace` packs without copying.
- Harden release and CI behavior with version sync validation, packaged binary smoke tests, and fixture lookup fixes for package builds.
- Extend core regression coverage with compile/emit golden tests and broader registry/config support tests.

## v0.2.0 - 2026-04-01

- Ship the first end-to-end `tnix` toolchain release with a Haskell core, CLI, LSP, and editor integrations for VS Code, Zed, and Neovim.
- Compile `.tnix` to `.nix`, type-check source files, and emit `.d.tnix` declaration files without adding any runtime layer.
- Add gradual typing primitives centered on `dynamic`, structural subtyping, ambient declarations for existing `.nix`, and declaration loading from workspace support files.
- Add type-level expressiveness including `forall`, conditional types, `infer`, higher-kinded aliases, and indexed container types for `Vec`, `Matrix`, `Tensor`, and `Tuple`.
- Infer exact container and sequence shapes from ordinary Nix list literals, including heterogeneous tuple inference and tensor widening back to structural list views when shapes diverge.
- Expand the regression suite across parser, checker, subtyping, compile/emit, CLI, LSP, VS Code, Zed, and Neovim validation.
