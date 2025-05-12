# Dotfiles

## Instructions

Clone the repository:

``` shell
$ git clone git@git.sr.ht:~sirn/dotfiles ~/.dotfiles
```

Install Nix:

``` shell
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```

Configure nix, edit `~/.config/nix/nix.conf`:

``` ini
experimental-features = nix-command flakes
```

Setup home directory with Home Manager:

``` shell
$ HM_PROFILE=$(hostname -s)
$ nix build --no-link .#homeConfigurations.$HM_PROFILE.activationPackage
$ $(nix path-info .#homeConfigurations.$HM_PROFILE.activationPackage)/activate
```

On subsequent updates, use:

```shell
$ home-manager switch --flake .#$HM_PROFILE
```

## Configuration

### Non-NixOS Linux

On a non-NixOS systems, a filename called `nix.generic` should be created to instruct Nixpkgs to not install packages that are known to depend on the host library (such as OpenGL).

``` shell
$ touch nix.generic
```
