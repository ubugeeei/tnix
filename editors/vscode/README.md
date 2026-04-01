# tnix for VS Code

`tnix` adds editor support for `.tnix` and `.d.tnix` files on top of the
`tnix-lsp` language server.

## Features

- diagnostics for parser and type errors
- hover with inferred and declared types
- completion for top-level bindings, `builtins`, and record fields
- go to definition / declaration for local bindings and ambient builtins

## Requirements

The extension launches `tnix-lsp` from your machine. Install it through a
GitHub release artifact or via the Nix flake:

```bash
nix profile install github:ubugeeei/tnix#tnix-lsp
```

The packaged `.vsix` is attached to each GitHub release. Tagged releases can
also publish directly to VS Code Marketplace and Open VSX when the repository
tokens are configured.

## Settings

- `tnix.server.path`
  Path to the `tnix-lsp` executable.
- `tnix.server.args`
  Extra command-line arguments passed to the server.
- `tnix.server.cwd`
  Optional working directory used to start the server.
