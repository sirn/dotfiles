#!/bin/sh

BASE=$(pwd -P) || exit 1

if [ -f "$BASE/pyproject.toml" ] && grep -q poetry "$BASE/pyproject.toml"; then
    exec poetry run pyls "$@"
    exit
fi

exec pyls "$@"
