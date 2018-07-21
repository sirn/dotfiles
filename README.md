# Dotfiles

This repository hosts my personal dotfiles as well as machine provisioning and workflow scripts.

## Usage

Darwin:

```shell
$ curl -sL https://raw.githubusercontent.com/sirn/dotfiles/master/bootstrap.sh | sh -s - -r \
    -p console \
    -p desktop \
    -p services
```

FreeBSD:

```shell
$ fetch -o - https://raw.githubusercontent.com/sirn/dotfiles/master/bootstrap.sh | sh -s - -r \
    -p console \
    -p desktop \
    -p services
```

Subsequent run, use `./dotfiles/bootstrap.sh`.

## License

Public domain
