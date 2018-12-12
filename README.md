# Dotfiles

This repository hosts my personal dotfiles as well as machine provisioning and workflow scripts. This repository come with no warranty. It may wipe your boot disk, eat your pet, etc. I like to experiment with my setup and this repository reflect that. Use it at your own risk!

## Usage

Darwin:

```shell
$ curl -sL "https://git.sr.ht/~sirn/dotfiles/blob/master/bootstrap.sh" | sh -s - -r \
    -p console \
    -p desktop \
    -p services
```

FreeBSD:

```shell
$ fetch -o - "https://git.sr.ht/~sirn/dotfiles/blob/master/bootstrap.sh" | sh -s - -r \
    -p console \
    -p desktop \
    -p services
```

Subsequent run, use `./dotfiles/bootstrap.sh`.

## License

Public domain
