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

Setup Home Manager:

```shell
$ nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
$ nix-channel --update
```

Setup home directory with Home Manager:

```shell
$ ~/.dotfiles/bin/home-manager-switch
````
