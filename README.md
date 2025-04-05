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

Setup Home Manager:

``` shell
$ nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
$ nix-channel --update
```

Setup home directory with Home Manager:

``` shell
$ ~/.dotfiles/bin/home-manager-switch
```

## Configuration

### Profile

By default, `home-manager-switch` script will use the current hostname as the profile name. To override, put a profile name in `nix.profile` file:

``` shell
$ echo ws > nix.profile
```

### Non-NixOS Linux

On a non-NixOS systems, a filename called `nix.generic` should be created to instruct Nixpkgs to not install packages that are known to depend on the host library (such as OpenGL).

``` shell
$ touch nix.generic
```
