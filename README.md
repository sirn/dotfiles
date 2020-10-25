# Dotfiles

This repository hosts my personal dotfiles as well as machine provisioning and workflow scripts. This repository come with no warranty. It may wipe your boot disk, eat your pet, etc. I like to experiment with my setup and this repository reflect that. This thing is also way too complicated than it should be, so use it at your own risk.

-   FreeBSD: [![builds.sr.ht status](https://builds.sr.ht/~sirn/dotfiles/commits/freebsd.yml.svg)](https://builds.sr.ht/~sirn/dotfiles/commits/freebsd.yml?)

## Usage

```shell
$ mkdir -p $HOME/.dotfiles
$ cd $HOME/.dotfiles
$ curl -sSL https://git.sr.ht/~sirn/dotfiles/archive/master.tar.gz | tar -xvzf - --strip-components=1
$ ./bootstrap.sh
```

## License

Public domain
