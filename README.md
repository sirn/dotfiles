# Dotfiles

Dotfiles repository for managing configurations across Linux and macOS machines.

## Project Structure

- `flake.nix`: The entry point defining Home Manager configurations and NixOS modules.
- `home-manager/`: Home Manager configuration and modules.
  - `config/`: Configuration files organized by program or service.
  - `modules/`: Home Manager module definitions.
  - `lib/`: Helper functions and utilities.
- `pkgs/`: Custom package definitions and overlays.
- `local.nix`: A machine-specific configuration file (gitignored) for local overrides.
- `secrets/`: Secrets managed with sops-nix.

## Machine Profiles

The following hostnames are defined as machine profiles in `flake.nix`:

- `phoebe` (Linux)
- `polaris` (Linux)
- `system76` (Linux)
- `terra` (Linux)
- `theia` (macOS - `aarch64-darwin`)
- `ws` (Linux)

## Getting Started

### Verify Nix Installation

Ensure Nix is installed. If not, install it with:

```shell
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```

Configure nix, edit `~/.config/nix/nix.conf` to enable flakes:

```ini
experimental-features = nix-command flakes
```

### Clone the Repository

```shell
$ git clone git@git.sr.ht:~sirn/dotfiles ~/.dotfiles
```

### Setup Home Manager (Standalone)

For macOS or generic Linux (non-NixOS), apply the Home Manager configuration using the machine profile name.

```shell
$ HM_PROFILE=$(hostname -s)
$ nix build --no-link path:.#homeConfigurations.$HM_PROFILE.activationPackage
$ $(nix path-info path:.#homeConfigurations.$HM_PROFILE.activationPackage)/activate
```

On subsequent updates, use:

```shell
$ home-manager switch --flake path:.#$HM_PROFILE
```

### Setup as NixOS Module

This repository provides a module to be used directly with the Home Manager NixOS module. In your NixOS configuration (`configuration.nix`), add:

```nix
modules = [
  inputs.home-manager.nixosModules.home-manager
  inputs.dotfiles.nixosModules.${hostname}
];
```

## Development & Maintenance

### Formatting

To format all files consistently:

```shell
nix run path:.#treefmt
```

### Testing Builds Locally

Test a build locally without applying the configuration:

```shell
$ HM_PROFILE=$(hostname -s)
$ nix build "path:.#homeConfigurations.$HM_PROFILE.activationPackage"
```

## Configuration

### Local Configuration

Create a file named `local.nix` to have a machine-specific configuration that is not committed to the repository.

```nix
{
  imports = [
    ./home-manager/config/programs/bitwarden.nix
    ./home-manager/config/services/languagetool.nix
  ];

  # When running on a non-NixOS Linux:
  targets.genericLinux.enable = true;
}
```

For NixOS, this needs to be done as part of the system's `configuration.nix` instead:

```nix
{
  # ...

  home-manager.users.sirn = {
    imports = [
      "${dotfiles}/home-manager/config/programs/bitwarden.nix"
      "${dotfiles}/home-manager/config/services/languagetool.nix"
    ];
  };
}
```

## Application-specific Notes

### Firefox

Application launchers are automatically generated for each Firefox profile defined in `programs.firefox.profiles`.

**macOS**: Proper `.app` bundles are created for each profile (e.g., "Firefox (main).app") and copied to `~/Applications/Home Manager Apps/`. These appear in Spotlight, Dock, and LaunchServices. Raycast scripts are also generated as a secondary option (see Raycast section).

**Linux**: An XDG desktop entry named "Firefox (profile)" is automatically generated.

### Raycast

Generated scripts are stored at `~/.local/libexec/raycast`.

To use Raycast script commands:

1. Apply configuration: `home-manager switch --flake .#$HM_PROFILE`
2. Open Raycast Preferences (⌘ + ,)
3. Go to Extensions → Script Commands
4. Click "Add Directories" and add `~/.local/libexec/raycast`
5. Search "Firefox" in Raycast to launch profiles
