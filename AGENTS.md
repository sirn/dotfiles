# Dotfiles Agent Instructions

This is a Nix/Home Manager dotfiles repository. Read `README.md` before doing any work.

## Project Structure

- `profiles/<hostname>.nix`: Machine profiles. NixOS hosts define `{ nixos = { ... }; home = { ... }; }`; Darwin hosts are Home Manager only.
- `home-manager/`: Home Manager config, modules, and helpers.
- `nixos/`: NixOS config, modules, and helpers.
- `pkgs/`: Custom packages and overlays.
- `secrets/`: sops-nix secrets; do not format or expose secret contents.

## Profiles

- NixOS: `phoebe`, `polaris`, `system76`, `terra`, `ws`.
- macOS/Home Manager only: `theia` (`aarch64-darwin`).

## Development

- Always use `path:.` instead of `.#` for Nix flake commands in this repo.
- Profiles are defined in `profiles/<hostname>.nix` and referenced by `flake.nix`.
- Verify NixOS/Home Manager options against official docs when changing Nix config.
- Do not run `home-manager switch` on NixOS; Home Manager is managed by the system config there.
- Use `nix run nixpkgs#python3 -- ...` for ad-hoc Python.
- Use `jaq` for JSON/YAML/TOML/XML processing when available.

## Quick Reference

- Format: `nix run path:.#treefmt`.
- Parse profile: `nix-instantiate --parse profiles/<hostname>.nix`.
- Build flake attr: `nix build --no-link path:.#<attr>`.
- Check flake: `nix flake check path:.`.
- Build NixOS profile: `nix build --no-link path:.#nixosConfigurations.<hostname>.config.system.build.toplevel`.
- Apply Home Manager on non-NixOS: `home-manager switch --flake path:.#<hostname>`.
- Check OS: `[ -f /etc/NIXOS ] && echo NixOS || echo Not NixOS`.

## Commit Messages

Keep messages concise and use these scope prefixes:

- `profiles/<hostname>` for `profiles/<hostname>.nix`, e.g. `profiles/terra: ...`.
- `nixos/<component>` for files under `nixos/config`, `nixos/modules`, or `nixos/lib`. Skip `modules/programs`, e.g. `nixos/system: ...`
- `home-manager/<component>` for files under `home-manager/config`, `home-manager/modules`, or `home-manager/lib`, e.g. `home-manager/pi-coding-agent: ...`
- `pkgs/<package>` for packages under `pkgs/<group>/<package>`, e.g. `pkgs/web-cli: ...`.
- Use the top-level filename for top-level files, and the directory path for other areas.
- For multiple areas, use brace scopes like `nixos/{system,services}: ...` or `{nixos,home-manager}/*: ...`.
