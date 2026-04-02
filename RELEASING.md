# Releasing

`tnix` is currently published primarily through GitHub Releases.

## What ships today

- `tnix` CLI binary
- `tnix-lsp` language server binary
- SHA-256 checksum files for each archive
- VS Code `.vsix` extension package
- Optional VS Code Marketplace publish when `VSCE_PAT` is configured
- Optional Open VSX publish when `OVSX_PAT` is configured
- Nix flake packages and apps exposed as `#tnix` and `#tnix-lsp`

Artifacts are built for:

- Linux x64
- macOS arm64

## Release flow

1. Make sure `main` is green in CI.
2. Run `nix flake check --accept-flake-config` locally to verify the published
   flake outputs, package tests, and dogfood/example fixtures.
3. Update versioned files as needed.
4. Create and push a semver tag such as `v0.2.1`.
5. Wait for the `Release` GitHub Actions workflow to finish.
6. Verify the generated release notes and uploaded assets on GitHub.
7. If `VSCE_PAT` and/or `OVSX_PAT` are configured, confirm the new extension
   version is visible on the corresponding marketplace.

## Commands

```bash
git checkout main
git pull --ff-only origin main
git tag v0.2.1
git push origin v0.2.1
```

## Notes

- `nix flake check` is the canonical release-grade validation entrypoint for
  the flake itself. It now covers version metadata sync, packaged binary smoke
  tests, Haskell package test suites, and dogfood/example fixture checks.
- CI now primes the Cabal package index explicitly so clean runners can resolve Haskell dependencies reliably.
- The release workflow always creates a GitHub Release. Marketplace publishing is layered on top and only runs when the corresponding repository secrets are present.
- Configure `VSCE_PAT` with a Visual Studio Marketplace publisher token and `OVSX_PAT` with an Open VSX token to enable automatic extension publishing on tag pushes.
