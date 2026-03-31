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
- Use structural subtyping and `dynamic` as the core gradual typing tools

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

- [Language Design](/Users/nishimura/Code/github.com/ubugeeei/tnix/docs/language-design.md)
- [Type System](/Users/nishimura/Code/github.com/ubugeeei/tnix/docs/type-system.md)
- [Architecture](/Users/nishimura/Code/github.com/ubugeeei/tnix/docs/architecture.md)
- [Roadmap](/Users/nishimura/Code/github.com/ubugeeei/tnix/docs/roadmap.md)

## Current Status

The repository is still in the specification phase. The plan is to lock down the language design first, then fix the development environment with Nix, and finally implement the Haskell monorepo and `pnpm`-managed editor integrations on top of that spec.
