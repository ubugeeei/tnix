import * as vscode from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node.js";
import { resolveRuntimeConfig } from "./runtime.js";

let client: LanguageClient | undefined;

/**
 * Boot the tnix language client inside VS Code.
 *
 * The extension keeps configuration deliberately small: users may override the
 * `tnix-lsp` binary path, while everything else is derived from the current
 * workspace. The runtime work itself is delegated to the Haskell LSP server.
 */
export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const config = vscode.workspace.getConfiguration("tnix");
  const runtime = resolveRuntimeConfig(
    config.get<string>("server.path"),
    vscode.workspace.workspaceFolders?.map((folder) => folder.uri.fsPath),
  );
  const executable: Executable = {
    command: runtime.command,
    args: runtime.args,
    options: { cwd: runtime.cwd, env: process.env },
  };
  const serverOptions: ServerOptions = {
    run: executable,
    debug: executable,
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: runtime.documentSelector,
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher(runtime.watchPattern),
    },
  };

  client = new LanguageClient("tnix", "tnix", serverOptions, clientOptions);
  await client.start();
  context.subscriptions.push(client);
}

/**
 * Stop the language client when the extension is deactivated.
 *
 * Keeping shutdown explicit avoids orphaned child processes when VS Code unloads
 * the extension during reloads or window closure.
 */
export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
    client = undefined;
  }
}
