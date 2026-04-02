import { findExecutable, printUsage, run, rootDir, tryRun } from "./utils.ts";

if (process.argv.includes("--help") || process.argv.includes("-h")) {
  printUsage([
    "Usage: vp cli",
    "",
    "Builds and installs the local tnix CLI toolchain into the active Nix profile",
    "so `tnix` and `tnix-lsp` are available on PATH.",
  ]);
  process.exit(0);
}

process.chdir(rootDir);

console.log("Installing tnix CLI binaries into the active Nix profile...");

tryRun("nix", ["profile", "remove", "tnix"]);
tryRun("nix", ["profile", "remove", "tnix-lsp"]);
tryRun("nix", ["profile", "remove", "tnix-toolchain"]);
run("nix", ["profile", "add", "--accept-flake-config", ".#tnix-toolchain"]);

const tnixPath = findExecutable("tnix");
const tnixLspPath = findExecutable("tnix-lsp");

console.log();
console.log("Installed binaries:");

if (!tnixPath || !tnixLspPath) {
  console.error("Expected `tnix` and `tnix-lsp` to be available on PATH after installation.");
  process.exit(1);
}

console.log(tnixPath);
run("tnix", ["--version"]);
console.log(tnixLspPath);
run("tnix-lsp", ["--version"]);
