# Dotfiles

Dotfiles repository for managing configurations across Linux and macOS machines.

## Project Structure

- `flake.nix`: The entry point defining Home Manager configurations and NixOS modules.
- `home-manager/`: Home Manager configuration and modules.
  - `config/`: Configuration files organized by program or service.
  - `modules/`: Home Manager module definitions.
  - `lib/`: Helper functions and utilities.
- `nixos/`: NixOS configuration and modules.
  - `config/`: Configuration files organized by program or service.
  - `modules/`: NixOS module definitions.
  - `lib/`: Helper functions and utilities.
- `pkgs/`: Custom package definitions and overlays.
- `secrets/`: Secrets managed with sops-nix.

## Machine Profiles

The following hostnames are defined as machine profiles in `flake.nix`:

- `phoebe` (Linux)
- `polaris` (Linux)
- `system76` (Linux)
- `terra` (Linux)
- `theia` (macOS - `aarch64-darwin`)
- `ws` (Linux)

## Setting Up

### Standalone Home Manager

Ensure Nix is installed. If not, install it with:

```shell
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```

Configure nix, edit `~/.config/nix/nix.conf` to enable flakes:

```ini
experimental-features = nix-command flakes
```

Clone the Repository:

```shell
$ git clone git@git.sr.ht:~sirn/dotfiles ~/.dotfiles
```

Setup Home Manager:

```shell
$ HM_PROFILE=$(hostname -s)
$ nix build --no-link path:.#homeConfigurations.$HM_PROFILE.activationPackage
$ $(nix path-info path:.#homeConfigurations.$HM_PROFILE.activationPackage)/activate
```

On subsequent updates, use:

```shell
$ home-manager switch --flake path:.#$HM_PROFILE
```

### NixOS

On a NixOS system, clone this repository into `/etc/nixos`:

```shell
$ git clone git@git.sr.ht:~sirn/nixos /etc/nixos
```

Generate hardware-configuration:

```shell
$ nixos-generate-config --root /etc/nixos
```

Edit `configuration.nix` and get rid of most configurations as the actual configuration belongs in the machine profiles. After done, rebuild NixOS with `nixos-rebuild`:

```shell
$ PROFILE=$(hostname -s)
$ nixos-rebuild --flake path:.#$PROFILE boot
```

## Development & Maintenance

### Formatting

To format all files consistently:

```shell
nix run path:.#treefmt
```

### Testing Home Manager Locally

Test a Home Manager build locally without applying the configuration:

```shell
$ HM_PROFILE=$(hostname -s)
$ nix build "path:.#homeConfigurations.$HM_PROFILE.activationPackage"
```

## Configuration

### Local Home Manager Configuration

Create a file named `local/home.nix` to have a machine-specific configuration that is not committed to the repository.

```nix
{
  imports = [
    ../home-manager/config/programs/bitwarden.nix
    ../home-manager/config/services/languagetool.nix
  ];

  # When running on a non-NixOS Linux:
  targets.genericLinux.enable = true;
}
```

### Binary Caches

Binary caches (numtide, noctalia, microvm) are configured declaratively:

- NixOS hosts via `nix.settings` in `nixos/config/system/nix.nix` (applies to all users on rebuild; `cache.nixos.org` is appended automatically by the NixOS nix module).
- The `theia` macOS host via `nix.settings` in `home-manager/config/home/nix.nix`, which Home Manager writes to `~/.config/nix/nix.conf` on `switch`.

These are set at the system/user level rather than in `flake.nix`'s `nixConfig`, which Nix ignores unless `accept-flake-config = true` is set.

On the standalone Home Manager host (`theia`), the user must also be trusted so Nix honours user-level `substituters`/`trusted-public-keys`. Add to `/etc/nix/nix.conf` once:

```ini
trusted-users = root <username>
```

then restart the Nix daemon before running `home-manager switch` or `nixos-rebuild`.
