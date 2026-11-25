# Dotfiles Agent Instructions

This is a Nix/Home Manager dotfiles repository consumed as a flake input by the private configuration repository. Read `README.md` before doing any work.

## Project Structure

- `lib/`: Flake builders (`mkNixOS`, `mkHomeManagerLinux`, `mkHomeManagerDarwin`, `mkMicroVM`) and shared helpers (`caches.nix`, `modules.nix`).
- `home-manager/`: Home Manager config and modules (`config/`, `modules/`).
- `nixos/`: NixOS config and modules (`config/`, `modules/`).
- `pkgs/`: Custom packages (`pkgs/by-name/<name>/package.nix`) and overlays.

## Modules

- Machine profiles live in the private configuration repository (`/etc/nixos`), which imports this flake.
- `lib.modules` exposes every shared module as an attrset, e.g. `dotfiles.lib.modules.nixos.config.roles.laptop`.
- The builders pass `lib.modules.nixos.config` and `lib.modules.homeManager.config` to the module systems as the `nixosConfig` and `hmConfig` special arguments.
- Generic `homeConfigurations.stub` and `nixosConfigurations.stub` outputs exist for local evaluation.

## Development

- This flake is a module library. The private entrypoint repository (`/etc/nixos` on NixOS, `~/.dotfiles` on Darwin) owns the profiles and passes them through `extraModules` and `extraHomeModules`.
- Live machine deployments happen from that private repository.
- Verify NixOS/Home Manager options against official docs when changing Nix config.
- Do not run `home-manager switch` on NixOS; Home Manager is managed by the system config there.
- Use `nix run nixpkgs#python3 -- ...` for ad-hoc Python.
- Use `jaq` for JSON/YAML/TOML/XML processing when available.
- NEVER revert treefmt changes.

## Quick Reference

- Format: `nix run .#treefmt`.
- Parse module: `nix-instantiate --parse <path>.nix`.
- Print module path: `nix eval --raw .#lib.modules.nixos.config.roles.laptop`.
- Build flake attr: `nix build --no-link .#<attr>`.
- Build a local package: `nix build --no-link .#<name>` (e.g. `nix build --no-link .#tiler`).
- Check flake: `nix flake check`.
- Test stub NixOS: `nix build --no-link .#nixosConfigurations.stub.config.system.build.toplevel`.
- Test stub Home Manager: `nix build --no-link .#homeConfigurations.stub.activationPackage`.
- Check OS: `[ -f /etc/NIXOS ] && echo NixOS || echo Not NixOS`.

## Commit Messages

Keep messages concise and use these scope prefixes:

- `lib/<name>` for files under `lib/`, e.g. `lib/modules: ...`.
- `nixos/<component>` for files under `nixos/config` or `nixos/modules`. Skip `modules/programs`, e.g. `nixos/system: ...`
- `hm/<component>` for files under `home-manager/config` or `home-manager/modules`, e.g. `hm/pi-coding-agent: ...`
- `pkgs/<package>` for packages under `pkgs/by-name/<package>`, e.g. `pkgs/tiler: ...`.
- Use the top-level filename for top-level files, and the directory path for other areas.
- For multiple areas, use brace scopes like `nixos/{system,services}: ...` or `{nixos,hm}/*: ...`.
