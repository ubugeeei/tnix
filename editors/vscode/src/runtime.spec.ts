import assert from "node:assert/strict";
import test from "node:test";
import { normalizeServerPath, resolveRuntimeConfig } from "./runtime.js";

test("normalizeServerPath falls back to tnix-lsp for missing or blank values", () => {
  assert.equal(normalizeServerPath(undefined), "tnix-lsp");
  assert.equal(normalizeServerPath(""), "tnix-lsp");
  assert.equal(normalizeServerPath("   "), "tnix-lsp");
});

test("normalizeServerPath preserves explicit executable paths", () => {
  assert.equal(normalizeServerPath("/nix/store/bin/tnix-lsp"), "/nix/store/bin/tnix-lsp");
  assert.equal(normalizeServerPath(" tnix-lsp-dev "), "tnix-lsp-dev");
});

test("resolveRuntimeConfig returns the expected selector and watcher glob", () => {
  assert.deepEqual(resolveRuntimeConfig(), {
    command: "tnix-lsp",
    args: [],
    documentSelector: [{ language: "tnix" }],
    watchPattern: "**/*.tnix",
  });
});
