# Dotfiles Agent Instructions

This is a Nix/Home Manager dotfiles repository.

**Read `README.md` before doing any work.**

## Project Structure

- `profiles/`: Machine profiles containing both NixOS and Home Manager configuration.
  - Each profile is a single file: `profiles/<hostname>.nix`
  - Structure: `{ nixos = { ... }; home = { ... }; }`
  - Darwin-only profiles omit the `nixos` attribute.
- `home-manager/`: Home Manager configuration and modules.
  - `config/`: Shared configuration files (common.nix, programs/, services/, etc.).
  - `modules/`: Home Manager module definitions.
  - `lib/`: Helper functions and utilities.
- `nixos/`: NixOS configuration and modules.
  - `config/`: Shared configuration files (common.nix, system/, services/, etc.).
  - `modules/`: NixOS module definitions.
  - `lib/`: Helper functions and utilities (e.g., `mk-microvm.nix`).
- `pkgs/`: Custom package definitions and overlays.
- `secrets/`: Secrets managed with sops-nix.

## Machine Profiles

The following hostnames are defined in `profiles/`:

- `phoebe` (NixOS)
- `polaris` (NixOS)
- `system76` (NixOS)
- `terra` (NixOS)
- `theia` (macOS - `aarch64-darwin`, Home Manager only)
- `ws` (NixOS)

## Development

### Formatting

Format all files with treefmt. Note that `*.sops.*` and files in `secrets/` are excluded from formatting.

```shell
nix run path:.#treefmt
```

### Build Conventions

- **REQUIRED:** Always use `path:.` instead of `.#` when running Nix commands (e.g., `nix build`, `nix run`). This ensures untracked files are recognized in dirty workspaces.
- Profiles are defined in `profiles/<hostname>.nix` and referenced by `flake.nix`.

### Verification

- When making changes to Nix files, always verify NixOS and Home Manager options against official documentation.
- Validate profile syntax: `nix-instantiate --parse profiles/<hostname>.nix`

### Testing

Check which OS you're running on before applying changes:

```shell
uname -a
# or check for NixOS
[ -f /etc/NIXOS ] && echo "NixOS" || echo "Not NixOS"
```

**If running on NixOS:** Do NOT run `home-manager switch` directly. Home Manager is managed by the NixOS system configuration. Use `nix build` only to test:

```shell
nix build --no-link path:.#nixosConfigurations.<hostname>.config.system.build.toplevel
```

**If NOT running on NixOS (macOS or other Linux):** Home Manager is the proper way to apply changes:

```shell
home-manager switch --flake path:.#<hostname>
```

## Commit Messages

Keep commit messages concise. Use the following format for the scope prefix:

| Files Changed                          | Format                           | Example                                  |
| -------------------------------------- | -------------------------------- | ---------------------------------------- |
| `profiles/<hostname>.nix`              | `profiles/<hostname>`            | `profiles/terra: add service`            |
| `nixos/config/<dir1>/<dir2>`           | `nixos/<dir1>/<dir2>`            | `nixos/system/nvidia: enable nvidia`     |
| `nixos/modules/<dir1>/<dir2>`          | `nixos/<dir1>/<dir2>`            | `nixos/system/vfio: fix option`          |
| `nixos/lib/<dir>`                      | `nixos/lib/<dir>`                | `nixos/lib/mk-microvm: fix`              |
| `home-manager/config/<dir1>/<dir2>`    | `home-manager/<dir1>/<dir2>`     | `home-manager/programs/bash: add alias`  |
| `home-manager/modules/<dir1>/<dir2>`   | `home-manager/<dir1>/<dir2>`     | `home-manager/programs/bash: add option` |
| `home-manager/lib/<dir>`               | `home-manager/lib/<dir>`         | `home-manager/lib/helpers: fix`          |
| `pkgs/<dir1>/<dir2>`                   | Omit `<dir1>`, use `pkgs/<dir2>` | `pkgs/gemini-cli-bin: update`            |
| Top-level (`flake.nix`, `README.md`)   | Use as-is                        | `README.md: update install`              |
| Other directories (`etc`, `var`, etc.) | Use as-is                        | `etc/emacs: fix path`                    |

Multiple files:

- Same directory: `nixos/{system,services}: ...`
- Different directories: `{nixos/system,home-manager/bash}: ...`

If too many files, use common ancestor or wildcard:

- `nixos/*: ...`
- `{nixos,home-manager}/*: ...`
- `*: ...`
