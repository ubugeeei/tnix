# Examples

`./examples` is a small tnix showcase project with runnable, checkable samples.

Quick commands:

```bash
tnix check ./examples/main.tnix
tnix check-project ./examples
tnix check ./examples/polymorphism/hkt-map.tnix
tnix check ./examples/interop/inline-declare.tnix
```

Catalog:

- `main.tnix`: a compact "hello tnix" entry point using `builtins.map`
- `basics/`: literals, lets, lambdas, records, lists, and conditionals
- `gradual/`: `any`, `unknown`, `dynamic`, casts, and diagnostic directives
- `polymorphism/`: `forall`, unions, higher-kinded aliases, tuples, and linear arrows
- `indexed/`: `Vec`, `Matrix`, `Tensor`, `Range`, and `Unit`
- `interop/`: inline declarations, ambient `.d.tnix` files, and untyped imports
- `legacy/`: plain `.nix` files used by the interop examples
- `support/`: workspace declarations for `builtins`, legacy modules, and `tnix.config.tnix`

The top-level `tnix.config.tnix` is wired so `tnix check-project ./examples`
walks the sample set as one project.
