#!/bin/sh
# This file is intended to be included from bootstrap.sh
# shellcheck disable=SC2034

_ansible_version="2.7.0"

case "$(uname)" in
    Darwin)
        _ansible_opts="-K"
        _ansible_python=/usr/local/bin/python3

        # Python on a Mac is weird.
        # See also: https://docs.brew.sh/Homebrew-and-Python
        _py_ver=$("$_ansible_python" -c 'import sys;print(".".join(map(str, sys.version_info[:2])))')
        _ansible_bin="$HOME/Library/Python/$_py_ver/bin/ansible-playbook"
        ;;

    FreeBSD)
        _ansible_opts=""
        _ansible_python=/usr/local/bin/python3.6
        _ansible_bin=$HOME/.local/bin/ansible-playbook
        ;;
esac
