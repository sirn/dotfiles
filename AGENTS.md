# Dotfiles Agent Instructions

This is a Nix/Home Manager dotfiles repository consumed as a flake input by the private configuration repository. Read `README.md` before doing any work.

## Project Structure

- `profiles/<hostname>.nix`: Base machine profiles. NixOS hosts define `{ nixos = { ... }; home = { ... }; }`; Darwin hosts define `{ home = { ... }; }`.
- `lib/`: Flake builders (`mkNixOS`, `mkHomeManagerLinux`, `mkHomeManagerDarwin`, `mkMicroVM`) and shared helpers (`caches.nix`).
- `home-manager/`: Home Manager config and modules (`config/`, `modules/`).
- `nixos/`: NixOS config and modules (`config/`, `modules/`).
- `pkgs/`: Custom packages (`pkgs/by-name/<name>/package.nix`) and overlays.

## Profiles

- NixOS: `polaris`, `system76`, `terra`, `ws` (MicroVM base profile).
- macOS/Home Manager only: `theia` (`aarch64-darwin`).

## Development

- Base profiles are defined in `profiles/<hostname>.nix` and exposed via `lib.*` builders (and `nixosConfigurations` / `homeConfigurations` for local testing) in `flake.nix`.
- Live machine deployments happen from the private entrypoint repository (`/etc/nixos` on NixOS, `~/.dotfiles` on Darwin), which consumes this flake.
- Verify NixOS/Home Manager options against official docs when changing Nix config.
- Do not run `home-manager switch` on NixOS; Home Manager is managed by the system config there.
- Use `nix run nixpkgs#python3 -- ...` for ad-hoc Python.
- Use `jaq` for JSON/YAML/TOML/XML processing when available.
- NEVER revert treefmt changes.

## Quick Reference

- Format: `nix run .#treefmt`.
- Parse profile: `nix-instantiate --parse profiles/<hostname>.nix`.
- Build flake attr: `nix build --no-link .#<attr>`.
- Build a local package: `nix build --no-link .#<name>` (e.g. `nix build --no-link .#tiler`).
- Check flake: `nix flake check`.
- Test base NixOS profile: `nix build --no-link .#nixosConfigurations.<hostname>.config.system.build.toplevel`.
- Test base Home Manager profile: `nix build --no-link .#homeConfigurations.<hostname>.activationPackage`.
- Check OS: `[ -f /etc/NIXOS ] && echo NixOS || echo Not NixOS`.

## Commit Messages

Keep messages concise and use these scope prefixes:

- `profiles/<hostname>` for `profiles/<hostname>.nix`, e.g. `profiles/terra: ...`.
- `nixos/<component>` for files under `nixos/config` or `nixos/modules`. Skip `modules/programs`, e.g. `nixos/system: ...`
- `hm/<component>` for files under `home-manager/config` or `home-manager/modules`, e.g. `hm/pi-coding-agent: ...`
- `pkgs/<package>` for packages under `pkgs/by-name/<package>`, e.g. `pkgs/tiler: ...`.
- Use the top-level filename for top-level files, and the directory path for other areas.
- For multiple areas, use brace scopes like `nixos/{system,services}: ...` or `{nixos,hm}/*: ...`.
