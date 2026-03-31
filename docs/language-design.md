# Language Design

## Core Positioning

`tnix` is not a new runtime language inspired by Nix. It is a type layer on top of Nix. Types exist only for static analysis, API contracts, and editor support. They do not affect evaluation and they must not change the meaning of the generated `.nix`.

## Main Design Principles

1. Nix first
   - Anyone who can read `.nix` should be able to read `.tnix` with minimal retraining.
2. Gradual first
   - Users must be able to start without types and adopt them incrementally.
3. Erasure first
   - Generated `.nix` contains no type artifacts.
4. Tooling first
   - The language is designed together with its checker, emitter, LSP, and editor integrations.

## File Model

### `.tnix`

Implementation files. They preserve the main Nix surface syntax such as expressions, `let`, lambdas, attribute sets, lists, `if`, paths, and `import`, while adding type annotations and type aliases.

### `.d.tnix`

Declaration-only files. They contain no value implementations and describe the external type surface of existing `.nix` code. They play the same role as `.d.ts` files in TypeScript.

### `.nix`

The runtime output. In the `.tnix -> .nix` transform, type annotations, type aliases, and declarations are erased while value-level structure is preserved.

## Added Syntax

### Type annotations

```tnix
name :: String;
f :: Int -> Int;
(x :: String): x
```

### Type aliases

```tnix
type Result e a = { ok :: true; value :: a; } | { ok :: false; error :: e; };
```

### Ambient declarations

```tnix
declare "./pkgs/default.nix" {
  mkPkg :: { name :: String; } -> Derivation;
};
```

This declares the public type surface of the value returned by `import ./pkgs/default.nix`.

## Syntax That Should Stay Familiar

- `let ... in`
- `x: body`
- `{ a = 1; }`
- `[ a b c ]`
- `if cond then a else b`
- `import ./file.nix`

The priority is to keep the mental model close to the Nix syntax tree users already know.

## Import Model

`tnix` does not redefine `import`. `import ./foo.nix` remains ordinary Nix code. Only the type checker looks for `./foo.d.tnix` or `declare "./foo.nix"` entries to recover a static type for the imported value.

## Declaration Merging

- Multiple `declare` blocks for the same path may be merged structurally.
- Conflicting declarations produce diagnostics.
- Local declarations take precedence over broader workspace declarations.

## Type Erasure

The compiler erases:

- `::` annotations
- `type` declarations
- `declare` declarations

The compiler preserves:

- value-level lambdas
- value-level `let`
- attribute sets
- lists
- paths
- `import`

## Explicit Non-Goals

- introducing a custom runtime
- replacing the Nix evaluator
- redesigning the full Nix module system
- drifting into a separate language that only resembles Nix

## Intended Style

The goal is to combine Haskell-like type expressiveness with ordinary Nix value syntax.

```tnix
type MapLike f = forall a b. (a -> b) -> f a -> f b;

mapNames :: forall f. MapLike f -> f { name :: String; } -> f String;
mapNames = fmap: xs: fmap (x: x.name) xs;
```

## Future Extensions

- kind annotations
- row-polymorphic attribute set extensions
- module-aware declaration emit
- incremental build graph support
