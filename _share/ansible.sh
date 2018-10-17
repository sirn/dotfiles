#!/bin/sh
# This file is intended to be included from bootstrap.sh
# shellcheck disable=SC2034

case "$(uname)" in
    Darwin)
        _ansible_opts="-K"
        _ansible_python=/usr/local/bin/python3
        _ansible_bin=/usr/local/bin/ansible-playbook
        ;;

    FreeBSD)
        _ansible_opts=""
        _ansible_python=/usr/local/bin/python3.6
        _ansible_bin=/usr/local/bin/ansible-playbook-3.6
        ;;
esac
