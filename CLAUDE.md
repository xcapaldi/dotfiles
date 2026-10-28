# CLAUDE.md

This repository holds my dotfiles and system setup scripts for the [chezmoi](https://www.chezmoi.io/), the dotfile manager. 
It is not a  live configuration.
Editing files here does nothing until `chezmoi apply` is run.
When working in this repo, assume nothing has taken effect on the live system until `chezmoi apply` has been run.
`.chezmoiignore` controls which source files are skipped per-platform.

If necessary, see the [command overview](https://www.chezmoi.io/user-guide/command-overview/) for the full CLI surface.

## How it works

Chezmoi maintains a *source state* (this repo, at `~/.local/share/chezmoi`) and applies it to a *target state* (the user's home directory).
The mapping is driven by file name prefixes and suffixes:

- `dot_foo` → `~/.foo` (e.g. `dot_zshrc` → `~/.zshrc`, `dot_config/` → `~/.config/`)
- `private_foo` → applied with `0600` permissions
- `*.tmpl` → rendered as a Go text/template before being written
- `run_once_*` → executed once per machine
- `run_onchange_*` → executed when the file's contents change

The full naming reference is in the [source state attributes](https://www.chezmoi.io/reference/source-state-attributes/) docs.

If a file is deleted here, we must manually delete it from the target.

## Tool installation

Tools are installed by scripts in `.chezmoiscripts/`, which chezmoi runs during `apply`:

- `run_onchange_before_install-packages-brew.sh.tmpl` — runs `brew bundle`
  with an OS-conditional package list (macOS gets the full set; WSL Ubuntu
  gets a leaner one). Re-runs whenever the script's rendered contents change,
  so adding/removing a `brew "..."` line triggers reinstallation on next apply.
- `run_onchange_before_install-packages.sh.tmpl` — installs `gh` extensions
  and OS-specific extras (apt packages + snaps on WSL, sdkman on macOS).
- `run_once_before_install-brew-wsl.sh.tmpl` — bootstraps Homebrew on WSL.

Note that when removing a package they must be uninstalled manually as the scripts only handle install.

Templated externals (git repos, release archives) are pulled in via `.chezmoiexternal.toml.tmpl` — e.g. the Vim LSP plugin.

## Targets

- macOS work machine (primary, full package set)
- WSL2 Ubuntu under Windows 11
- Bare-metal Ubuntu hacktop
