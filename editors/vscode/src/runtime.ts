export type RuntimeConfig = {
  command: string;
  args: string[];
  cwd?: string;
  documentSelector: { language: string }[];
  watchPattern: string;
};

/**
 * Normalize the configured server path into a safe executable command.
 *
 * Blank or whitespace-only values fall back to the bundled default so the
 * extension can recover from partially edited settings.
 */
export function normalizeServerPath(serverPath?: string): string {
  const trimmed = serverPath?.trim();
  return trimmed && trimmed.length > 0 ? trimmed : "tnix-lsp";
}

/**
 * Pick the first available workspace path as the language-server working
 * directory.
 */
export function resolveWorkspaceCwd(workspacePaths?: readonly string[]): string | undefined {
  return workspacePaths?.find((path) => path.trim().length > 0);
}

/**
 * Build the runtime configuration shared by activation and tests.
 */
export function resolveRuntimeConfig(serverPath?: string, workspacePaths?: readonly string[]): RuntimeConfig {
  return {
    command: normalizeServerPath(serverPath),
    args: [],
    cwd: resolveWorkspaceCwd(workspacePaths),
    documentSelector: [{ language: "tnix" }],
    watchPattern: "**/*.tnix",
  };
}
