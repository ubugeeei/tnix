# Roadmap

## Phase 0: Spec First

- document the language design
- lock down the core type-system concepts
- define monorepo package responsibilities

## Phase 1: Core

- parser
- AST
- pretty printer
- type representation
- subtype and consistency
- basic inference

Deliverables:

- `.tnix -> .nix`
- `tnix check`
- `tnix emit`

## Phase 2: Ambient + Workspace

- `.d.tnix` parser
- workspace declaration discovery
- `import` declaration resolution
- declaration emitter stabilization

## Phase 3: Type Puzzle Features

- conditional types
- `infer`
- higher-kinded type application
- improved solver diagnostics

## Phase 4: Tooling

- Haskell LSP server
- VS Code extension
- Zed extension
- neovim helper

## Phase 5: Hardening

- larger fixture corpus
- golden tests
- regression suite
- performance tuning
- incremental cache

## Shipping Criteria

- erased `.nix` preserves source semantics
- existing `.nix` files can be typed with `.d.tnix` alone
- hover and diagnostics are practically useful
- the main type-puzzle examples are expressible
