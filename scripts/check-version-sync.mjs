import { readFile } from "node:fs/promises";

const rootPackagePath = new URL("../package.json", import.meta.url);
const vscodePackagePath = new URL("../editors/vscode/package.json", import.meta.url);
const changelogPath = new URL("../CHANGELOG.md", import.meta.url);
const cabalPaths = [
  new URL("../packages/tnix-core/tnix-core.cabal", import.meta.url),
  new URL("../packages/tnix-cli/tnix-cli.cabal", import.meta.url),
  new URL("../packages/tnix-lsp/tnix-lsp.cabal", import.meta.url),
];

const errors = [];

const rootPackage = JSON.parse(await readFile(rootPackagePath, "utf8"));
const vscodePackage = JSON.parse(await readFile(vscodePackagePath, "utf8"));
const changelog = await readFile(changelogPath, "utf8");
const cabalFiles = await Promise.all(cabalPaths.map((path) => readFile(path, "utf8")));

const workspaceVersion = rootPackage.version;
const cabalVersion = `${workspaceVersion}.0`;
const changelogMatch = changelog.match(/^## v([0-9]+\.[0-9]+\.[0-9]+) - /m);

if (vscodePackage.version !== workspaceVersion) {
  errors.push(
    `editors/vscode/package.json version ${vscodePackage.version} does not match root package.json version ${workspaceVersion}.`,
  );
}

for (const [index, content] of cabalFiles.entries()) {
  const match = content.match(/^version:\s+([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)\s*$/m);
  const path = cabalPaths[index].pathname;

  if (!match) {
    errors.push(`${path} is missing a parseable version field.`);
    continue;
  }

  if (match[1] !== cabalVersion) {
    errors.push(`${path} version ${match[1]} does not match expected ${cabalVersion}.`);
  }
}

if (!changelogMatch) {
  errors.push("CHANGELOG.md is missing a top-level release heading like `## v0.2.0 - 2026-04-01`.");
} else if (changelogMatch[1] !== workspaceVersion) {
  errors.push(`CHANGELOG.md top release ${changelogMatch[1]} does not match package.json version ${workspaceVersion}.`);
}

if (errors.length > 0) {
  console.error("Version metadata is out of sync:");
  for (const error of errors) {
    console.error(`- ${error}`);
  }
  process.exit(1);
}

console.log(
  `Version metadata OK: workspace=${workspaceVersion}, vscode=${vscodePackage.version}, cabal=${cabalVersion}, changelog=${changelogMatch[1]}`,
);
