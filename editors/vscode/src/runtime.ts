import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

export type RuntimeConfig = {
  command: string;
  args: string[];
  cwd?: string;
  documentSelector: { language: string }[];
  watchPattern: string;
};

export function defaultServerPathCandidates(homePath: string = homedir()): string[] {
  return [
    join(homePath, ".nix-profile", "bin", "tnix-lsp"),
    join(homePath, ".local", "state", "nix", "profiles", "profile", "bin", "tnix-lsp"),
    join(homePath, ".local", "state", "nix", "profiles", "home-manager", "home-path", "bin", "tnix-lsp"),
    "/run/current-system/sw/bin/tnix-lsp",
  ];
}

export function resolveInstalledServerPath(
  homePath: string = homedir(),
  exists: (path: string) => boolean = existsSync,
): string | undefined {
  return defaultServerPathCandidates(homePath).find((candidate) => exists(candidate));
}

export function resolveDefaultServerPath(
  homePath: string = homedir(),
  exists: (path: string) => boolean = existsSync,
): string {
  return resolveInstalledServerPath(homePath, exists) ?? "tnix-lsp";
}

/**
 * Normalize the configured server path into a safe executable command.
 *
 * Blank or whitespace-only values fall back to the bundled default so the
 * extension can recover from partially edited settings.
 */
export function normalizeServerPath(
  serverPath?: string,
  resolveDefault: () => string = () => resolveDefaultServerPath(),
): string {
  const trimmed = serverPath?.trim();
  return trimmed && trimmed.length > 0 ? trimmed : resolveDefault();
}

/**
 * Normalize the configured server arguments into a compact argv list.
 *
 * VS Code stores arrays verbatim, so we defensively trim and drop blank items
 * to tolerate partially edited workspace settings.
 */
export function normalizeServerArgs(serverArgs?: readonly string[]): string[] {
  return (serverArgs ?? []).map((arg) => arg.trim()).filter((arg) => arg.length > 0);
}

/**
 * Pick the first available workspace path as the language-server working
 * directory.
 */
export function resolveWorkspaceCwd(workspacePaths?: readonly string[]): string | undefined {
  return workspacePaths?.find((path) => path.trim().length > 0);
}

/**
 * Resolve the effective working directory for the language server.
 *
 * An explicit setting wins, otherwise we fall back to the first workspace
 * folder so `tnix-lsp` can discover ambient declarations from the project root.
 */
export function resolveServerCwd(
  configuredCwd?: string,
  workspacePaths?: readonly string[],
): string | undefined {
  const trimmed = configuredCwd?.trim();
  return trimmed && trimmed.length > 0 ? trimmed : resolveWorkspaceCwd(workspacePaths);
}

/**
 * Build the runtime configuration shared by activation and tests.
 */
export function resolveRuntimeConfig(
  serverPath?: string,
  serverArgs?: readonly string[],
  configuredCwd?: string,
  workspacePaths?: readonly string[],
): RuntimeConfig {
  return {
    command: normalizeServerPath(serverPath),
    args: normalizeServerArgs(serverArgs),
    cwd: resolveServerCwd(configuredCwd, workspacePaths),
    documentSelector: [{ language: "tnix" }, { language: "nix" }],
    watchPattern: "**/*.{nix,tnix}",
  };
}
