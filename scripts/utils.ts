import { accessSync, constants, existsSync } from "node:fs";
import { delimiter, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync, type SpawnSyncOptions } from "node:child_process";

export const rootDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");

export function run(command: string, args: string[], options: SpawnSyncOptions = {}): void {
  const result = spawnSync(command, args, {
    stdio: "inherit",
    cwd: rootDir,
    ...options,
  });

  if (result.error) {
    throw result.error;
  }

  if (result.status !== 0) {
    process.exit(result.status ?? 1);
  }
}

export function capture(command: string, args: string[], options: SpawnSyncOptions = {}): string {
  const result = spawnSync(command, args, {
    stdio: ["inherit", "pipe", "inherit"],
    encoding: "utf8",
    cwd: rootDir,
    ...options,
  });

  if (result.error) {
    throw result.error;
  }

  if (result.status !== 0) {
    process.exit(result.status ?? 1);
  }

  return result.stdout.trim();
}

export function tryRun(command: string, args: string[], options: SpawnSyncOptions = {}): boolean {
  const result = spawnSync(command, args, {
    stdio: "ignore",
    cwd: rootDir,
    ...options,
  });

  return result.status === 0;
}

export function findExecutable(name: string): string | undefined {
  const pathValue = process.env.PATH ?? "";
  for (const entry of pathValue.split(delimiter)) {
    if (!entry) {
      continue;
    }

    const candidate = join(entry, name);
    if (!existsSync(candidate)) {
      continue;
    }

    try {
      accessSync(candidate, constants.X_OK);
      return candidate;
    } catch {
      continue;
    }
  }

  return undefined;
}

export function printUsage(lines: string[]): void {
  console.log(lines.join("\n"));
}
