import { copyFileSync, createReadStream, mkdirSync, rmSync } from "node:fs";
import { createHash } from "node:crypto";
import { writeFile } from "node:fs/promises";
import { basename, join } from "node:path";
import { tmpdir } from "node:os";
import { capture, printUsage, run, rootDir } from "./utils.ts";

async function sha256File(path: string): Promise<string> {
  const hash = createHash("sha256");

  await new Promise<void>((resolvePromise, reject) => {
    const stream = createReadStream(path);
    stream.on("data", (chunk) => hash.update(chunk));
    stream.on("end", () => resolvePromise());
    stream.on("error", reject);
  });

  return hash.digest("hex");
}

const [version, target, archiveName, shaName] = process.argv.slice(2);
const wantsHelp = process.argv.includes("--help") || process.argv.includes("-h");

if (wantsHelp || !version || !target || !archiveName || !shaName) {
  printUsage([
    "Usage: node --experimental-strip-types ./scripts/package-release.ts <version> <target> <archive> <sha>",
    "",
    "Example:",
    "  node --experimental-strip-types ./scripts/package-release.ts v0.2.0 linux-x64 tnix-v0.2.0-linux-x64.tar.gz tnix-v0.2.0-linux-x64.sha256",
  ]);
  process.exit(wantsHelp ? 0 : 1);
}

process.chdir(rootDir);

const stageDir = join(tmpdir(), `tnix-release-${process.pid}-${Date.now()}`);
const releaseDir = join(stageDir, `tnix-${version.replace(/^v/, "")}-${target}`);

mkdirSync(join(releaseDir, "bin"), { recursive: true });

try {
  run("cabal", ["build", "exe:tnix", "exe:tnix-lsp"]);

  const tnixBin = capture("cabal", ["list-bin", "exe:tnix"]);
  const tnixLspBin = capture("cabal", ["list-bin", "exe:tnix-lsp"]);

  copyFileSync(tnixBin, join(releaseDir, "bin/tnix"));
  copyFileSync(tnixLspBin, join(releaseDir, "bin/tnix-lsp"));
  copyFileSync(join(rootDir, "README.md"), join(releaseDir, "README.md"));
  copyFileSync(join(rootDir, "CHANGELOG.md"), join(releaseDir, "CHANGELOG.md"));
  copyFileSync(join(rootDir, "LICENSE"), join(releaseDir, "LICENSE"));

  run("tar", ["-C", stageDir, "-czf", archiveName, basename(releaseDir)]);

  const digest = await sha256File(join(rootDir, archiveName));
  const checksumLine = `${digest}  ${archiveName}\n`;
  await writeFile(join(rootDir, shaName), checksumLine);
} finally {
  rmSync(stageDir, { recursive: true, force: true });
}
