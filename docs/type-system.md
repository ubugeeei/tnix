# Type System

## Direction

The `tnix` type system is a blend of Haskell and TypeScript. Its inference style is primarily Haskell-like, while its adoption model and tooling philosophy are TypeScript-like. The top priority is not maximal strictness. It is incremental adoption in existing Nix codebases.

## Core Features

### 1. `dynamic`

Untyped code and external boundaries are represented as `dynamic`. It is close to `unknown`, but not a full top type. It receives special treatment in consistency checking rather than replacing the subtype lattice.

### 2. Structural subtyping

Attribute sets are compared structurally rather than nominally.

```tnix
{ name :: String; version :: String; } <: { name :: String; }
```

Function types are contravariant in arguments and covariant in results.

### 3. Union

Union types are included to support partial adoption.

```tnix
String | Int
```

### 4. Parametric polymorphism

```tnix
id :: forall a. a -> a;
```

### 5. Higher-kinded types

Type constructor application is treated as a first-class operation in the type language.

```tnix
type Functor f = {
  map :: forall a b. (a -> b) -> f a -> f b;
};
```

### 6. Conditional types

The language includes a TypeScript-style `extends ? :` form.

```tnix
type Element t = t extends List (infer a) ? a : t;
```

### 7. `infer`

`infer` introduces pattern variables inside the right-hand side of conditional types.

```tnix
type ReturnOf f = f extends (_ -> infer r) ? r : dynamic;
```

### 8. Indexed dependent-ish containers

Lists can be preserved more precisely as indexed containers:

```tnix
Vec 3 Int
Matrix 2 4 Float
Tensor [2 3 4] Number
Vec (2 | 3 | Range 4 8 Nat) Int
```

Shape indices are type-level values, so the checker can express exact lengths,
bounded lengths, and unions of admissible lengths without introducing runtime
evidence.

Examples:

```tnix
[1 2]
# => Vec 2 (1 | 2)

[[1 2] [3 4]]
# => Matrix 2 2 (1 | 2 | 3 | 4)

[[1] [2 3]]
# => List (Vec (1 | 2) (1 | 2 | 3))

let xs :: Vec (Range 2 4 Nat) Int;
    xs = [1 2 3];
in xs
# => accepted

let xs :: Vec (2 | Range 4 8 Nat) Int;
    xs = [1 2 3];
in xs
# => rejected
```

### 9. Numeric validation

`tnix` supports a small numeric refinement surface:

```tnix
Nat
Range 0 100 Int
Range 0.0 1.0 Float
```

Numeric literals subtype these validators when they satisfy the corresponding
constraint. This also feeds back into indexed containers, so `Vec (Range 2 4
Nat) Int` can be checked directly against list literals of matching length.

Examples:

```tnix
let ratio :: Range 0.0 1.0 Float;
    ratio = 0.5;
in ratio
# => accepted

let ratio :: Range 0.0 1.0 Float;
    ratio = 1.5;
in ratio
# => rejected

let xs :: Vec (Range 0 0 Nat) Int;
    xs = [];
in xs
# => accepted
```

### 10. Units

Units are modeled as lightweight phantom wrappers over validated values:

```tnix
Unit "ms" (Range 0 5000 Nat)
Unit "MiB" Int
```

They are erased before runtime, but the checker keeps them distinct so values
with different units do not subtype each other accidentally.

Examples:

```tnix
let timeout :: Unit "ms" (Range 0 5000 Nat);
    timeout = 2500;
in timeout
# => accepted

let timeoutMs :: Unit "ms" Nat;
    timeoutMs = 1;
    timeoutS :: Unit "s" Nat;
    timeoutS = timeoutMs;
in timeoutS
# => rejected
```

## Consistency and Partial Adoption

`tnix` checks both subtyping and consistency.

- subtype
  - the strict static relation
- consistent
  - a gradual compatibility relation that accounts for `dynamic`

Examples:

- `String` and `dynamic` are consistent
- `String` and `Int` are not consistent
- `Vec 2 Int` and `List Int` can still interact structurally
- `Unit "ms" Nat` and `Unit "s" Nat` are neither subtypes nor consistent by label alone

## Inference Strategy

### Basic rules

- Use annotations on lambda parameters when present.
- Use `let` annotations as checking boundaries.
- Infer unannotated regions with meta variables.
- Generalize at `let` bindings.

### `import`

`import ./foo.nix` is typed from workspace declarations:

- `./foo.d.tnix`
- `declare "./foo.nix" { ... }`

If neither exists, the checker falls back to `dynamic`.

Examples:

```tnix
declare "./lib.nix" { default :: { value :: Int; }; };
import ./lib.nix
# => { value :: Int; }

import ./missing.nix
# => dynamic
```

## Attribute sets

Attribute sets are modeled as `Record` types. Users should be able to annotate only the public surface they care about while still permitting additional fields.

```tnix
{ name = "a"; version = "1"; } :: { name :: String; }
```

## Type-class-like behavior

The first version does not implement full Haskell-style type class resolution. Instead, it models capabilities as dictionary records and leaves room for future constraint sugar.

```tnix
type Eq a = { eq :: a -> a -> Bool; };
```

## Error Strategy

- Prefer explainable errors over maximal solver cleverness.
- Return messages that help users make fixes instead of exposing only large type equations.
- Surface `dynamic` fallbacks explicitly in hover and diagnostics.

## `.d.tnix` emitter

The emitter extracts only the public type surface from `.tnix`.

- If the root expression is an attribute set, its fields become the exported API.
- Any other root value is emitted as a `default`-style export.
- Required type aliases are emitted alongside it.

Examples:

```tnix
type User = { name :: String; };
{ make = name: { inherit name; }; }
```

Generated declaration:

```tnix
type User = { name :: String; };

declare "./current-file.nix" {
  default :: { make :: String -> User; };
};
```

## Future Work

- row variables
- stronger bidirectional checking
- exhaustiveness hints
- richer declaration merging
- faster and more incremental solving
