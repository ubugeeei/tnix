import assert from "node:assert/strict";
import test from "node:test";
import { normalizeServerPath, resolveRuntimeConfig, resolveWorkspaceCwd } from "./runtime.js";

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
    cwd: undefined,
    documentSelector: [{ language: "tnix" }],
    watchPattern: "**/*.tnix",
  });
});

test("resolveWorkspaceCwd chooses the first non-empty workspace path", () => {
  assert.equal(resolveWorkspaceCwd(undefined), undefined);
  assert.equal(resolveWorkspaceCwd(["", "  ", "/workspace/two"]), "/workspace/two");
  assert.equal(resolveWorkspaceCwd(["/workspace/one", "/workspace/two"]), "/workspace/one");
});
