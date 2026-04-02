use std::env;
use std::path::Path;
use zed_extension_api::{register_extension, settings::LspSettings, Command, Extension, LanguageServerId, Result, Worktree};

/// Zed extension entry point for tnix.
///
/// The extension does not implement language logic itself. Its only job is to
/// locate the `tnix-lsp` binary, pass through workspace shell environment
/// variables, and respect any user override configured in Zed settings.
struct Tnix;

fn normalize_binary_path(path: Option<String>) -> Option<String> {
    path.and_then(|value| {
        let trimmed = value.trim().to_string();
        if trimmed.is_empty() {
            None
        } else {
            Some(trimmed)
        }
    })
}

fn normalize_binary_arguments(arguments: Option<Vec<String>>) -> Vec<String> {
    arguments
        .unwrap_or_default()
        .into_iter()
        .filter_map(|value| {
            let trimmed = value.trim().to_string();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed)
            }
        })
        .collect()
}

fn build_command(command: String, args: Vec<String>, env: Vec<(String, String)>) -> Command {
    Command { command, args, env }
}

fn default_binary_candidates(home: Option<&str>) -> Vec<String> {
    let mut candidates = Vec::new();

    if let Some(home) = home {
        candidates.push(format!("{home}/.nix-profile/bin/tnix-lsp"));
        candidates.push(format!("{home}/.local/state/nix/profiles/profile/bin/tnix-lsp"));
        candidates.push(format!("{home}/.local/state/nix/profiles/home-manager/home-path/bin/tnix-lsp"));
    }

    candidates.push("/run/current-system/sw/bin/tnix-lsp".to_string());
    candidates
}

fn resolve_default_binary_with<F>(home: Option<&str>, exists: F) -> Option<String>
where
    F: Fn(&str) -> bool,
{
    default_binary_candidates(home)
        .into_iter()
        .find(|candidate| exists(candidate))
}

fn resolve_default_binary() -> Option<String> {
    let home = env::var("HOME").ok();
    resolve_default_binary_with(home.as_deref(), |candidate| Path::new(candidate).exists())
}

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
                if let Some(path) = normalize_binary_path(binary.path) {
                    return Ok(build_command(path, normalize_binary_arguments(binary.arguments), env));
                }
            }
        }

        let path = worktree
            .which("tnix-lsp")
            .or_else(|| resolve_default_binary())
            .ok_or_else(|| "tnix-lsp must be installed and available in $PATH.".to_string())?;
        Ok(build_command(path, vec![], env))
    }
}

register_extension!(Tnix);

#[cfg(test)]
mod tests {
    use super::{build_command, default_binary_candidates, normalize_binary_arguments, normalize_binary_path, resolve_default_binary_with};

    #[test]
    fn normalize_binary_path_drops_blank_values() {
        assert_eq!(normalize_binary_path(None), None);
        assert_eq!(normalize_binary_path(Some(String::new())), None);
        assert_eq!(normalize_binary_path(Some("   ".into())), None);
    }

    #[test]
    fn normalize_binary_path_trims_explicit_values() {
        assert_eq!(normalize_binary_path(Some(" tnix-lsp ".into())), Some("tnix-lsp".into()));
        assert_eq!(normalize_binary_path(Some("/nix/store/bin/tnix-lsp".into())), Some("/nix/store/bin/tnix-lsp".into()));
    }

    #[test]
    fn normalize_binary_arguments_compacts_argv() {
        assert_eq!(normalize_binary_arguments(None), Vec::<String>::new());
        assert_eq!(
            normalize_binary_arguments(Some(vec!["".into(), " --stdio ".into(), "  ".into()])),
            vec!["--stdio".to_string()]
        );
        assert_eq!(
            normalize_binary_arguments(Some(vec!["--log-file".into(), "/tmp/tnix.log".into()])),
            vec!["--log-file".to_string(), "/tmp/tnix.log".to_string()]
        );
    }

    #[test]
    fn build_command_preserves_command_args_and_env() {
        let command = build_command(
            "tnix-lsp".into(),
            vec!["--stdio".into()],
            vec![("TNIX_ENV".into(), "1".into())],
        );

        assert_eq!(command.command, "tnix-lsp");
        assert_eq!(command.args, vec!["--stdio".to_string()]);
        assert_eq!(command.env, vec![("TNIX_ENV".to_string(), "1".to_string())]);
    }

    #[test]
    fn default_binary_candidates_prioritize_common_nix_profiles() {
        assert_eq!(
            default_binary_candidates(Some("/home/alice")),
            vec![
                "/home/alice/.nix-profile/bin/tnix-lsp".to_string(),
                "/home/alice/.local/state/nix/profiles/profile/bin/tnix-lsp".to_string(),
                "/home/alice/.local/state/nix/profiles/home-manager/home-path/bin/tnix-lsp".to_string(),
                "/run/current-system/sw/bin/tnix-lsp".to_string(),
            ]
        );
    }

    #[test]
    fn resolve_default_binary_with_returns_first_existing_candidate() {
        let resolved = resolve_default_binary_with(Some("/home/alice"), |candidate| {
            candidate == "/home/alice/.local/state/nix/profiles/profile/bin/tnix-lsp"
        });

        assert_eq!(resolved, Some("/home/alice/.local/state/nix/profiles/profile/bin/tnix-lsp".to_string()));
        assert_eq!(resolve_default_binary_with(Some("/home/alice"), |_| false), None);
    }
}
