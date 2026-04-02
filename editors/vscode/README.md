# tnix for VS Code

`tnix` adds editor support for `.nix`, `.tnix`, and `.d.tnix` files on top of the
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

Verify the server is reachable before opening VS Code:

```bash
tnix-lsp --version
```

The packaged `.vsix` is attached to each GitHub release. Tagged releases can
also publish directly to VS Code Marketplace and Open VSX when the repository
tokens are configured.

## Settings

- `tnix.server.path`
  Path to the `tnix-lsp` executable. Leave this blank to auto-detect common
  Nix profile locations before falling back to `tnix-lsp` on `PATH`.
- `tnix.server.args`
  Extra command-line arguments passed to the server.
- `tnix.server.cwd`
  Optional working directory used to start the server.

## Troubleshooting

If the extension activates but features stay unavailable:

- confirm `tnix-lsp --version` works in the same shell environment VS Code inherits
- leave `tnix.server.path` blank unless you need to override auto-detection
- set `tnix.server.path` explicitly when the binary is outside both the default `PATH` and common Nix profile locations
- set `tnix.server.cwd` to the workspace root when you need `tnix-lsp` to discover ambient declarations from a specific project
