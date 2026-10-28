# Dotfiles

Public NixOS and Home Manager configurations, modules, and packages across Linux and macOS machines.

This flake is consumed as an input by the private configuration repository, which serves as the entry point for each machine and supplies host-specific hardware, boot settings, secrets, and private modules.

## Project Structure

- `flake.nix`: Exposes host and MicroVM builders (`lib.*`), base `homeConfigurations` and `nixosConfigurations` for local testing, local `packages`, and `apps.treefmt`.
- `profiles/<hostname>.nix`: Base machine profiles (`{ nixos = ...; home = ...; }` on NixOS, `{ home = ...; }` on Darwin).
- `lib/`: Flake builders (`mkNixOS`, `mkHomeManagerLinux`, `mkHomeManagerDarwin`, `mkMicroVM`) and shared helpers (`caches.nix`).
- `home-manager/`: Home Manager configuration and modules.
  - `config/`: Configuration files organized by role, home, program, or service.
  - `modules/`: Home Manager module definitions.
- `nixos/`: NixOS configuration and modules.
  - `config/`: Configuration files organized by role, system, program, or service.
  - `modules/`: NixOS module definitions.
- `pkgs/`: Custom package definitions (`pkgs/by-name/`) and overlays.

## Machine Profiles

`profiles/<hostname>.nix` defines the public half of each profile. The private repository imports this flake and extends each profile via `dotfiles.lib.*`:

- `polaris` (NixOS - `x86_64-linux`)
- `system76` (NixOS - `x86_64-linux`)
- `terra` (NixOS - `x86_64-linux`)
- `theia` (macOS Home Manager - `aarch64-darwin`)
- `ws` (MicroVM base profile - `x86_64-linux`)

Example usage in the private repository's `flake.nix`:

```nix
{
  inputs.dotfiles.url = "github:sirn/dotfiles";

  outputs = { dotfiles, ... }: {
    homeConfigurations.theia = dotfiles.lib.mkHomeManagerDarwin {
      hostname = "theia";
      extraModules = [ (import ./profiles/theia).home ];
    };

    nixosConfigurations.terra = dotfiles.lib.mkNixOS {
      hostname = "terra";
      extraModules = [ (import ./profiles/terra).nixos ];
      extraHomeModules = [ (import ./profiles/terra).home ];
    };
  };
}
```

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

Parse-check a profile:

```shell
$ nix-instantiate --parse profiles/<hostname>.nix
```

Build a local package from the overlay:

```shell
$ nix build --no-link .#<package>
```

Test a base Home Manager profile without applying it:

```shell
$ HM_PROFILE=$(hostname -s)
$ nix build --no-link ".#homeConfigurations.$HM_PROFILE.activationPackage"
```

Test a base NixOS profile without switching to it:

```shell
$ PROFILE=$(hostname -s)
$ nix build --no-link ".#nixosConfigurations.$PROFILE.config.system.build.toplevel"
```

## Configuration

### Terminal Palettes

Themes in `home-manager/config/home/colors/` define 16 ANSI colors, a background, and a foreground. `generate-palette.py` uses CIELAB interpolation to generate extended colors (`16–255`) and shared semantic color roles.

### Binary Caches

Public binary caches (`cache.nixos.org`, `numtide`, `noctalia`, `microvm`) are defined in `lib/caches.nix` and configured declaratively:

- NixOS hosts via `nix.settings` in `nixos/config/system/nix.nix`.
- Standalone Home Manager hosts (`theia`) via `nix.settings` in `home-manager/config/home/nix.nix`, which writes `~/.config/nix/nix.conf` on `switch`.

These are set at the system/user level rather than in `flake.nix`'s `nixConfig`, which Nix ignores unless `accept-flake-config = true` is set.

On standalone Home Manager hosts (`theia`), the user must be trusted in `/etc/nix/nix.conf` so the Nix daemon honours user-level `substituters` and `trusted-public-keys`:

```ini
trusted-users = root <username>
```

Restart the Nix daemon after editing `/etc/nix/nix.conf`.
