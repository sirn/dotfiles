# Dotfiles Agent Instructions

This is a Nix/Home Manager dotfiles repository.

**Read `README.md` before doing any work.**

## Development

### Formatting

Format all files with treefmt. Note that `*.sops.*` and files in `secrets/` are excluded from formatting.

```shell
nix run path:.#treefmt
```

### Build Conventions

- **REQUIRED:** Always use `path:.` instead of `.#` when running Nix commands (e.g., `nix build`, `nix run`). This ensures untracked files are recognized in dirty workspaces.
- Home Manager profiles are named after hostnames (defined in `flake.nix` outputs).

### Verification

- When making changes to Nix files, always verify NixOS and Home Manager options against official documentation.

### Testing

Check which OS you're running on before applying changes:

```shell
uname -a
# or check for NixOS
[ -f /etc/NIXOS ] && echo "NixOS" || echo "Not NixOS"
```

**If running on NixOS:** Do NOT run `home-manager switch` directly. Home Manager is managed by the NixOS system configuration. Use `nix build` only to test:

```shell
nix build --no-link path:.#homeConfigurations.<hostname>.activationPackage
```

**If NOT running on NixOS (macOS or other Linux):** Home Manager is the proper way to apply changes:

```shell
home-manager switch --flake path:.#<hostname>
```

## Commit Messages

Keep commit messages concise. Use the following format for the scope prefix:

| Files Changed                          | Format                               | Example                       |
| -------------------------------------- | ------------------------------------ | ----------------------------- |
| `config/<dir1>/<dir2>`                 | Omit `config/`, use `<dir1>/<dir2>`  | `programs/bash: add alias`    |
| `modules/<dir1>/<dir2>`                | Omit `modules/`, use `<dir1>/<dir2>` | `programs/bash: add alias`    |
| `pkgs/<dir1>/<dir2>`                   | Omit `<dir1>`, use `pkgs/<dir2>`     | `pkgs/gemini-cli-bin: update` |
| Top-level (`flake.nix`, `README.md`)   | Use as-is                            | `README.md: update install`   |
| Other directories (`etc`, `var`, etc.) | Use as-is                            | `etc/emacs: fix path`         |

Multiple files:

- Same directory: `programs/{bash,zsh}: ...`
- Different directories: `{etc/emacs,programs/bash}: ...`

If too many files, use common ancestor or wildcard:

- `programs/*: ...`
- `{etc,programs}/*: ...`
- `*: ...`
