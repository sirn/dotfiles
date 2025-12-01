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
$ home-manager switch --flake .#$HM_PROFILE
```

## Configuration

### Local Configuration

Create a file named `local.nix` to have a machine-specific configuration that is not committed a machine profile.

```nix
{
  import = [
    ./modules/programs/bitwarden.nix
    ./modules/services/languagetool.nix
  ];

  # When running on a non-NixOS
  targets.genericLinux.enable = true;
}
```
