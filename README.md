# Dotfiles

Public NixOS and Home Manager modules and packages across Linux and macOS machines.

This flake is a module library. The private configuration repository is the entry point for each machine: it owns every profile and consumes this flake for the builders, shared modules, and packages.

## Project Structure

- `flake.nix`: Exposes the MicroVM and Home Manager builders (`lib.*`), the module registry (`lib.modules`), generic `homeConfigurations` and `nixosConfigurations` stubs for local evaluation, `packages` for the local and `llm-agents` package sets, and `apps.treefmt`.
- `lib/`: Flake builders (`mkNixOS`, `mkHomeManagerLinux`, `mkHomeManagerDarwin`, `mkMicroVM`) and shared helpers (`caches.nix`, `modules.nix`).
- `home-manager/`: Home Manager configuration and modules.
  - `config/`: Configuration files organized by role, home, program, or service.
  - `modules/`: Home Manager module definitions.
- `nixos/`: NixOS configuration and modules.
  - `config/`: Configuration files organized by role, system, program, or service.
  - `modules/`: NixOS module definitions.
- `pkgs/`: Custom package definitions (`pkgs/by-name/`) and overlays.

## Modules

`lib.modules` exposes every module as an attrset that mirrors the directory tree. The `.nix` suffix is dropped, and a directory with a `default.nix` appears as its own path:

```nix
dotfiles.lib.modules.nixos.config.roles.laptop
dotfiles.lib.modules.nixos.config.programs."1password"
dotfiles.lib.modules.homeManager.config.common
```

The builders pass the tree into the module systems as special arguments. `nixosConfig` is `lib.modules.nixos.config` and `hmConfig` is `lib.modules.homeManager.config`, so a module imports shared modules without an argument from its caller:

```nix
# profiles/terra/default.nix in the private repository
{
  nixos =
    { nixosConfig, ... }:
    {
      imports = [
        nixosConfig.common
        nixosConfig.roles.laptop
        ./nixos/hardware.nix
      ];
      networking.hostName = "terra";
    };

  home =
    { hmConfig, ... }:
    {
      imports = [ hmConfig.common hmConfig.common-linux ];
    };
}
```

The private repository imports each profile and passes its parts to a builder:

```nix
outputs = { dotfiles, ... }:
let
  profile = hostname: import ./profiles/${hostname};
in
{
  nixosConfigurations.terra = dotfiles.lib.mkNixOS {
    extraModules = [ (profile "terra").nixos ];
    extraHomeModules = [ (profile "terra").home ];
  };

  homeConfigurations.theia = dotfiles.lib.mkHomeManagerDarwin {
    extraModules = [ (profile "theia").home ];
  };
}
```

A `mkMicroVM` guest receives its base modules through `extraModules` and `extraHomeModules` in the same way. Each guest is its own system, so `mkMicroVM` passes `nixosConfig` and `hmConfig` to it again.

## Setting Up

### Standalone Home Manager

Ensure Nix is installed. If not, install it with:

```shell
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```

Enable flakes in `~/.config/nix/nix.conf`:

```ini
experimental-features = nix-command flakes
```

Clone the private repository to `~/.dotfiles`:

```shell
$ git clone <private repository> ~/.dotfiles
$ cd ~/.dotfiles
```

Bootstrap Home Manager:

```shell
$ HM_PROFILE=$(hostname -s)
$ nix build --no-link .#homeConfigurations.$HM_PROFILE.activationPackage
$ $(nix path-info .#homeConfigurations.$HM_PROFILE.activationPackage)/activate
```

On subsequent updates, run:

```shell
$ home-manager switch --flake ~/.dotfiles#$HM_PROFILE
```

### NixOS

Clone the private repository into `/etc/nixos`:

```shell
$ git clone <private repository> /etc/nixos
```

Apply the configuration with `nixos-rebuild`:

```shell
$ PROFILE=$(hostname -s)
$ nixos-rebuild --sudo --flake /etc/nixos#$PROFILE switch
```

## Development & Maintenance

### Formatting

Format all files:

```shell
$ nix run .#treefmt
```

### Testing Locally

Parse-check a module:

```shell
$ nix-instantiate --parse nixos/config/<path>.nix
```

Print a registered module path:

```shell
$ nix eval --raw .#lib.modules.nixos.config.roles.laptop
```

Build a local package from the overlay:

```shell
$ nix build --no-link .#<package>
```

The generic stubs import the common configuration and no machine hardware. Build them to check that the modules still evaluate:

```shell
$ nix build --no-link .#homeConfigurations.stub.activationPackage
$ nix build --no-link .#nixosConfigurations.stub.config.system.build.toplevel
```

On macOS, the Darwin stub replaces the Linux one:

```shell
$ nix build --no-link .#homeConfigurations.stub-darwin.activationPackage
```

## Configuration

### Terminal Palettes

Themes in `home-manager/config/home/colors/` define 16 ANSI colors, a background, and a foreground. `generate-palette.py` uses CIELAB interpolation to generate extended colors (`16–255`) and shared semantic color roles.

### Binary Caches

Public binary caches (`cache.nixos.org`, `numtide`, `noctalia`, `microvm`) are defined in `lib/caches.nix` and configured declaratively:

- NixOS hosts via `nix.settings` in `nixos/config/system/nix.nix`.
- Standalone Home Manager hosts via `nix.settings` in `home-manager/config/home/nix.nix`, which writes `~/.config/nix/nix.conf` on `switch`.

These are set at the system/user level rather than in `flake.nix`'s `nixConfig`, which Nix ignores unless `accept-flake-config = true` is set.

On standalone Home Manager hosts (`theia`), the user must be trusted in `/etc/nix/nix.conf` so the Nix daemon honours user-level `substituters` and `trusted-public-keys`:

```ini
trusted-users = root <username>
```

Restart the Nix daemon after editing `/etc/nix/nix.conf`.
