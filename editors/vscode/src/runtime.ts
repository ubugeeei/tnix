export type RuntimeConfig = {
  command: string;
  args: string[];
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
 * Build the runtime configuration shared by activation and tests.
 */
export function resolveRuntimeConfig(serverPath?: string): RuntimeConfig {
  return {
    command: normalizeServerPath(serverPath),
    args: [],
    documentSelector: [{ language: "tnix" }],
    watchPattern: "**/*.tnix",
  };
}
