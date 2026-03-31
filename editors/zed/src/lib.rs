use zed_extension_api::{register_extension, settings::LspSettings, Command, Extension, LanguageServerId, Result, Worktree};

/// Zed extension entry point for tnix.
///
/// The extension does not implement language logic itself. Its only job is to
/// locate the `tnix-lsp` binary, pass through workspace shell environment
/// variables, and respect any user override configured in Zed settings.
struct Tnix;

impl Extension for Tnix {
    fn new() -> Self {
        Self {}
    }

    /// Resolve the command used to launch the tnix language server.
    ///
    /// A per-worktree configured binary wins. When no override is present, the
    /// extension falls back to looking up `tnix-lsp` on the worktree PATH so it
    /// works naturally inside the Nix dev shell.
    fn language_server_command(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> Result<Command> {
        let env = worktree.shell_env();

        if let Ok(settings) = LspSettings::for_worktree(language_server_id.as_ref(), worktree) {
            if let Some(binary) = settings.binary {
                if let Some(path) = binary.path {
                    return Ok(Command { command: path, args: binary.arguments.unwrap_or_default(), env });
                }
            }
        }

        let path = worktree.which("tnix-lsp").ok_or_else(|| "tnix-lsp must be installed and available in $PATH.".to_string())?;
        Ok(Command { command: path, args: vec![], env })
    }
}

register_extension!(Tnix);
