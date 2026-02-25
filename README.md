# Dotfiles

## Instructions

Clone the repository:

```shell
$ git clone git@git.sr.ht:~sirn/dotfiles ~/.dotfiles
```

Install Nix:

```shell
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```

Configure nix, edit `~/.config/nix/nix.conf`:

```ini
experimental-features = nix-command flakes
```

Setup home directory with Home Manager:

```shell
$ HM_PROFILE=$(hostname -s)
$ nix build --no-link .#homeConfigurations.$HM_PROFILE.activationPackage
$ $(nix path-info .#homeConfigurations.$HM_PROFILE.activationPackage)/activate
```

On subsequent updates, use:

```shell
$ home-manager switch --flake path:.#$HM_PROFILE
```

## NixOS Module

This repository also provides a module to be used with Home Manager NixOS Module:

```nix
modules = [
  inputs.home-manager.nixosModules.home-manager
  inputs.dotfiles.nixosModules.${hostname}
];
```

To test building locally, use:

```shell
$ HM_PROFILE=$(hostname -s)
$ nix build ".#homeConfigurations.$HM_PROFILE.activationPackage"
```

## Configuration

### Local Configuration

Create a file named `local.nix` to have a machine-specific configuration that is not committed a machine profile.

```nix
{
  import = [
    ./config/programs/bitwarden.nix
    ./config/services/languagetool.nix
  ];

  # When running on a non-NixOS
  targets.genericLinux.enable = true;
}
```

For NixOS, this needs to be done as part of NixOS `configuration.nix`:

```nix
{
  # ...

  home-manager.users.sirn = {
    imports = [
      "${dotfiles}/config/programs/bitwarden.nix"
      "${dotfiles}/config/programs/languagetool.nix"
    ];
  };
}
```

## Application-specific Notes

### Firefox

Application launchers are automatically generated for each Firefox profile defined in `programs.firefox.profiles`.

For macOS, see Raycast section.

For Linux, an XDG application named "Firefox (profile)" is automatically generated.

### Raycast

Generated scripts are stored at `~/.local/libexec/raycast`.

To use Raycast script commands:
1. Apply configuration: `home-manager switch --flake .#$HM_PROFILE`
2. Open Raycast Preferences (⌘ + ,)
3. Go to Extensions → Script Commands
4. Click "Add Directories" and add `~/.local/libexec/raycast`
5. Search "Firefox" in Raycast to launch profiles
