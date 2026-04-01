#!/usr/bin/env bash
set -euo pipefail

version="${1:?expected version tag like v0.2.0}"
target="${2:?expected target label}"
archive_name="${3:?expected archive filename}"
sha_name="${4:?expected sha filename}"

stage_dir="$(mktemp -d)"
release_dir="$stage_dir/tnix-${version#v}-${target}"

cleanup() {
  rm -rf "$stage_dir"
}

trap cleanup EXIT

mkdir -p "$release_dir/bin"

cabal build exe:tnix exe:tnix-lsp
cp "$(cabal list-bin exe:tnix)" "$release_dir/bin/tnix"
cp "$(cabal list-bin exe:tnix-lsp)" "$release_dir/bin/tnix-lsp"
cp README.md CHANGELOG.md LICENSE "$release_dir/"

tar -C "$stage_dir" -czf "$archive_name" "$(basename "$release_dir")"
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum "$archive_name" >"$sha_name"
else
  shasum -a 256 "$archive_name" >"$sha_name"
fi
