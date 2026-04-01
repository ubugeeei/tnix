# Getting Started

## What `tnix` Is

`tnix` is a static type layer for Nix.

- `.tnix` keeps ordinary Nix value syntax
- types are used for checking, hover, and declaration emit
- generated `.nix` erases all type syntax

If you already know Nix, the goal is that `tnix` feels like "Nix plus a type surface", not a different runtime language.

## First Commands

From the repository root:

```bash
nix develop
```

Typical CLI entry points:

```bash
tnix init .
tnix scaffold .
tnix check ./examples/main.tnix
tnix compile ./examples/main.tnix -o ./dist/main.nix
tnix emit ./examples/main.tnix -o ./dist/main.d.tnix
```

If you are using the published flake directly:

```bash
nix run github:ubugeeei/tnix#tnix -- check ./main.tnix
nix run github:ubugeeei/tnix#tnix -- compile ./main.tnix -o ./main.nix
nix run github:ubugeeei/tnix#tnix -- emit ./main.tnix -o ./main.d.tnix
```

## Scaffolding A Project

`tnix init` creates a starter project in the target directory:

- `tnix.config.tnix`
- `src/main.tnix`
- `types/builtins.d.tnix`

The generated config is ordinary tnix syntax:

```tnix
{
  name = "demo";
  sourceDir = ./src;
  entry = ./src/main.tnix;
  declarationDir = ./types;
  builtins = true;
}
```

You can later re-run:

```bash
tnix scaffold .
```

to materialize any missing scaffold files without overwriting existing ones.

## Your First `.tnix` File

```tnix
let
  greeting :: String;
  greeting = "hello";
in greeting
```

Checking this file validates the annotation and infers the root type.

Compiling it produces ordinary Nix:

```nix
let
  greeting = "hello";
in greeting
```

## Records And Field Access

`tnix` uses structural typing for attribute sets.

```tnix
let
  pkg :: { name :: String; version :: String; };
  pkg = { name = "tnix"; version = "0.1.0"; };
in pkg.name
```

The checker understands the field projection and infers the root type as `String`.

## Ambient Typing For Existing `.nix`

You can type legacy `.nix` modules without rewriting them.

```tnix
declare "./legacy/default.nix" {
  default :: { name :: String; version :: String; };
};

import ./legacy/default.nix
```

This is the main bridge for incremental adoption:

- keep the runtime implementation in `.nix`
- describe its public API in `.d.tnix` or inline `declare`
- use that API from typed `.tnix`

## Lists, Vectors, And Matrices

Plain Nix list syntax can infer more precise indexed shapes.

```tnix
[1 2]
# => Vec 2 (1 | 2)

[[1 2] [3 4]]
# => Matrix 2 2 (1 | 2 | 3 | 4)

[[1] [2 3]]
# => List (Vec (1 | 2) (1 | 2 | 3))
```

You can also write shape annotations directly:

```tnix
let
  xs :: Vec 3 Int;
  xs = [1 2 3];
in xs
```

Bounded lengths are supported too:

```tnix
let
  xs :: Vec (Range 2 4 Nat) Int;
  xs = [1 2 3];
in xs
```

## Numeric Validation

`tnix` includes a small refinement surface for numeric values.

```tnix
let
  ratio :: Range 0.0 1.0 Float;
  ratio = 0.5;
in ratio
```

This passes, while:

```tnix
let
  ratio :: Range 0.0 1.0 Float;
  ratio = 1.5;
in ratio
```

is rejected.

## Units

Units are phantom wrappers that survive checking but disappear at runtime.

```tnix
let
  timeout :: Unit "ms" (Range 0 5000 Nat);
  timeout = 2500;
in timeout
```

Different labels stay distinct:

```tnix
let
  timeoutMs :: Unit "ms" Nat;
  timeoutMs = 1;
  timeoutS :: Unit "s" Nat;
  timeoutS = timeoutMs;
in timeoutS
```

The assignment to `timeoutS` is rejected.

## Diagnostic Directives

`tnix` supports TypeScript-style line comments for intentional checker failures.

Ignore the next line's checker error:

```tnix
let
  # @tnix-ignore
  value = missing;
in value
```

Expect the next line to fail and report an error if it does not:

```tnix
let
  # @tnix-expected
  value :: Int;
  value = "oops";
in value
```

Today these directives are aimed at root expressions and `let` items.

## Declaration Emit

`tnix emit` turns a `.tnix` file into a `.d.tnix` API surface.

Source:

```tnix
type User = { name :: String; };

{
  make = name: { inherit name; };
}
```

Emitted declaration:

```tnix
type User = { name :: String; };

declare "./current-file.nix" {
  default :: { make :: String -> User; };
};
```

## Suggested Learning Path

1. Start with plain annotations on `let` bindings and function parameters.
2. Add ambient declarations for existing `.nix` imports.
3. Use `emit` to stabilize public APIs between files.
4. Add `tnix.config.tnix` and `tnix scaffold` once the project layout is settling.
5. Reach for `Vec` / `Matrix` / `Tensor`, `Range`, and `Unit` when the shape or numeric contract actually matters.

## Next Docs

- [Language Reference](./language-reference.md)
- [Type System](./type-system.md)
- [Language Design](./language-design.md)
- [Architecture](./architecture.md)
