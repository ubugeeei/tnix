# Releasing

`tnix` is currently published primarily through GitHub Releases.

## What ships today

- `tnix` CLI binary
- `tnix-lsp` language server binary
- SHA-256 checksum files for each archive
- VS Code `.vsix` extension package
- Nix flake packages and apps exposed as `#tnix` and `#tnix-lsp`

Artifacts are built for:

- Linux x64
- macOS arm64

## Release flow

1. Make sure `main` is green in CI.
2. Update versioned files as needed.
3. Create and push a semver tag such as `v0.2.1`.
4. Wait for the `Release` GitHub Actions workflow to finish.
5. Verify the generated release notes and uploaded assets on GitHub.

## Commands

```bash
git checkout main
git pull --ff-only origin main
git tag v0.2.1
git push origin v0.2.1
```

## Notes

- CI now primes the Cabal package index explicitly so clean runners can resolve Haskell dependencies reliably.
- Editor marketplace publishing is intentionally separate from the binary release flow. VS Code, Zed, and Neovim can be added as follow-up distribution lanes without changing the core GitHub Release process.
