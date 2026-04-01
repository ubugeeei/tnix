import assert from "node:assert/strict";
import test from "node:test";
import {
  normalizeServerArgs,
  normalizeServerPath,
  resolveRuntimeConfig,
  resolveServerCwd,
  resolveWorkspaceCwd,
} from "./runtime.js";

test("normalizeServerPath falls back to tnix-lsp for missing or blank values", () => {
  assert.equal(normalizeServerPath(undefined), "tnix-lsp");
  assert.equal(normalizeServerPath(""), "tnix-lsp");
  assert.equal(normalizeServerPath("   "), "tnix-lsp");
});

test("normalizeServerPath preserves explicit executable paths", () => {
  assert.equal(normalizeServerPath("/nix/store/bin/tnix-lsp"), "/nix/store/bin/tnix-lsp");
  assert.equal(normalizeServerPath(" tnix-lsp-dev "), "tnix-lsp-dev");
});

test("normalizeServerArgs trims and drops blank argv entries", () => {
  assert.deepEqual(normalizeServerArgs(undefined), []);
  assert.deepEqual(normalizeServerArgs(["", " --stdio ", "  "]), ["--stdio"]);
  assert.deepEqual(normalizeServerArgs(["--log-file", "/tmp/tnix.log"]), ["--log-file", "/tmp/tnix.log"]);
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

test("resolveRuntimeConfig honors args and cwd overrides", () => {
  assert.deepEqual(resolveRuntimeConfig("tnix-lsp-dev", [" --stdio ", ""], "/tmp/tnix", ["/workspace"]), {
    command: "tnix-lsp-dev",
    args: ["--stdio"],
    cwd: "/tmp/tnix",
    documentSelector: [{ language: "tnix" }],
    watchPattern: "**/*.tnix",
  });
});

test("resolveWorkspaceCwd chooses the first non-empty workspace path", () => {
  assert.equal(resolveWorkspaceCwd(undefined), undefined);
  assert.equal(resolveWorkspaceCwd(["", "  ", "/workspace/two"]), "/workspace/two");
  assert.equal(resolveWorkspaceCwd(["/workspace/one", "/workspace/two"]), "/workspace/one");
});

test("resolveServerCwd prefers explicit cwd over workspace discovery", () => {
  assert.equal(resolveServerCwd(undefined, ["/workspace"]), "/workspace");
  assert.equal(resolveServerCwd("  /custom/root  ", ["/workspace"]), "/custom/root");
  assert.equal(resolveServerCwd("   ", ["/workspace"]), "/workspace");
});
