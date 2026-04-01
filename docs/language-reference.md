# Language Reference

## Overview

`tnix` keeps Nix value syntax and adds type-only syntax for checking and
tooling.

The runtime model is simple:

- parse `.tnix`
- type-check it
- erase type syntax
- emit ordinary `.nix`

## Files

- `.tnix`: implementation files with value-level Nix syntax plus types
- `.d.tnix`: declaration-only files that describe existing `.nix` surfaces
- `.nix`: erased runtime output

## Expressions

### Literals

```tnix
"hello"
1
1.5
true
false
null
../path.nix
```

### Variables

```tnix
value
builtins
```

### Lambdas

```tnix
x: x
(x :: Int): x
```

### Application

```tnix
f x
builtins.map (x: x.name) xs
```

### `let`

```tnix
let
  value = 1;
in value
```

With signatures:

```tnix
let
  value :: Int;
  value = 1;
in value
```

`let` items are also where the current diagnostic directives are most useful:

```tnix
let
  # @tnix-expected
  value :: Int;
  value = "oops";
in value
```

### Attribute Sets

```tnix
{ name = "tnix"; version = "0.1.0"; }
{ inherit value; nested = { enabled = true; }; }
```

### Lists

```tnix
[1 2 3]
[[1 2] [3 4]]
```

### Conditionals

```tnix
if cond then yes else no
```

### Import

```tnix
import ./lib.nix
(import ./lib.nix).value
```

## Type Annotations

```tnix
name :: String;
f :: Int -> Int;
(x :: String): x
```

## Diagnostic Directives

`tnix` recognizes two line-comment directives modeled after TypeScript.

### `# @tnix-ignore`

Suppress the next root-expression or `let`-item checker failure.

```tnix
let
  # @tnix-ignore
  value = missing;
in value
```

### `# @tnix-expected`

Suppress the next failure, but raise an error if that line does not fail.

```tnix
# @tnix-expected
missing
```

This is useful for regression tests and documentation examples where a failure
is the expected outcome.

## Type Forms

### Primitive Constructors

```tnix
String
Int
Float
Number
Nat
Bool
Path
Null
any
dynamic
unknown
```

### Literal Singleton Types

```tnix
"tnix"
1
1.5
true
false
```

### Function Types

```tnix
Int -> Int
String -> { name :: String; }
Int %1 -> Int
```

`%1 ->` is the linear arrow.

### Record Types

```tnix
{ name :: String; version :: String; }
```

### Union Types

```tnix
String | Int
{ ok :: true; value :: a; } | { ok :: false; error :: e; }
```

### Parametric Polymorphism

```tnix
forall a. a -> a
forall f a. f a -> f a
```

### Type Aliases

```tnix
type Box a = { value :: a; };
type Pair = Tuple [Int String];
```

### Higher-Kinded Application

```tnix
type Apply f a = f a;
type Id f = f;
Apply (Id List) Int
```

### Conditional Types

```tnix
type Element t = t extends List (infer a) ? a : t;
type ReturnOf f = f extends (_ -> infer r) ? r : dynamic;
```

### Tuple Types

`Tuple` is the heterogeneous fixed-length sequence form.

```tnix
Tuple [Int String]
Tuple [1 "x" true]
```

### Indexed Containers

```tnix
Vec 3 Int
Matrix 2 4 Float
Tensor [2 3 4] Number
Vec (2 | 3 | Range 4 8 Nat) Int
Tensor [2 (Range 1 2 Nat) 1] Int
```

```tnix
[1 2]
# => Vec 2 (1 | 2)

[[1 2] [3 4]]
# => Matrix 2 2 (1 | 2 | 3 | 4)

[[1] [2 3]]
# => List (Vec (1 | 2) (1 | 2 | 3))
```

### Numeric Validators

```tnix
Nat
Range 0 10 Int
Range 0 5000 Nat
Range 0.0 1.0 Float
```

```tnix
3 <: Range 0 10 Nat
0.5 <: Range 0.0 1.0 Float
11 </: Range 0 10 Nat
```

### Units

```tnix
Unit "ms" Nat
Unit "ms" (Range 0 5000 Nat)
Unit "MiB" Int
```

```tnix
1 <: Unit "ms" Nat
Unit "ms" Nat </: Unit "s" Nat
```

## Declarations

Ambient declarations attach types to existing runtime files.

```tnix
declare "./legacy/default.nix" {
  default :: { value :: Int; };
  mkPkg :: { name :: String; } -> Derivation;
};
```

### Bundled Registry Packs

The repository also ships curated `.d.tnix` packs under `registry/`. These
packs are alias-only and are meant to be reused from local `declare` blocks.

Current packs include:

- `NixpkgsLib`, `NixpkgsPkgs`, and related aliases for `nixpkgs`
- `NixFlakeUtilsFlake`, `HomeManagerFlake`, `NixDarwinFlake`, `FlakePartsLib`
- `DevenvFlake`, `TreefmtNixFlake`, `PreCommitHooksFlake`, `CraneFlake`
- `DeployRsFlake`, `NixvimFlake`, `SopsNixFlake`, `AgenixFlake`, `DiskoFlake`, `ColmenaFlake`

Example:

```tnix
declare "./nixpkgs.nix" { default :: NixpkgsImport; };
declare "./home-manager.nix" { default :: HomeManagerFlake; };
declare "./treefmt-nix.nix" { default :: TreefmtNixFlake; };
```

The checker resolves those aliases exactly like aliases written in your own
`.d.tnix` files.

## Inference Notes

### Structural Records

```tnix
{ name = "a"; version = "1"; } :: { name :: String; }
```

### Gradual Compatibility

```tnix
any
unknown
dynamic
```

The three gradual escape hatches have different roles:

- `any` is assignable to and from every type.
- `unknown` is a top type. Every value can be viewed as `unknown`, but `unknown` does not flow back into concrete types without an annotation or narrowing.
- `dynamic` keeps the existing tnix gradual-consistency behavior. It is consistent with every type, but not a concrete subtype of every type.

### List Shape Inference

```tnix
[]
# => Vec 0 dynamic

[1 "x"]
# => Tuple [1 "x"]
```

## Erasure

The following syntax is erased during `.tnix -> .nix` compilation:

- `::` annotations
- `type` aliases
- `declare` blocks

The following value-level syntax remains:

- lambdas
- `let`
- attrsets
- lists
- paths
- `if`
- `import`

## Common Patterns

### Typing A Legacy Import

```tnix
declare "./lib.nix" {
  default :: { value :: Int; };
};

(import ./lib.nix).value
```

### Stable Public API With `emit`

```tnix
type User = { name :: String; };

{
  make = name: { inherit name; };
}
```

### Bounded Sequence Contracts

```tnix
let
  xs :: Vec (Range 2 4 Nat) Int;
  xs = [1 2 3];
in xs
```

### Unit-Safe Numeric Contracts

```tnix
let
  timeout :: Unit "ms" (Range 0 5000 Nat);
  timeout = 2500;
in timeout
```

## Related Docs

- [Getting Started](./getting-started.md)
- [Type System](./type-system.md)
- [Language Design](./language-design.md)
