import * as vscode from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node.js";

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const config = vscode.workspace.getConfiguration("tnix");
  const command = config.get<string>("server.path", "tnix-lsp");
  const executable: Executable = {
    command,
    args: [],
    options: { env: process.env },
  };
  const serverOptions: ServerOptions = {
    run: executable,
    debug: executable,
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: "tnix" }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.{tnix,d.tnix}"),
    },
  };

  client = new LanguageClient("tnix", "tnix", serverOptions, clientOptions);
  await client.start();
  context.subscriptions.push(client);
}

export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
    client = undefined;
  }
}
