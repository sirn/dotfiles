#!/bin/sh
#
# Wrapper over pyls to run pyls within a project if pyproject.toml is define
# and configured with poetry.
#

BASE=$(pwd -P) || exit 1

if [ -f "$BASE/pyproject.toml" ] && grep -q poetry "$BASE/pyproject.toml"; then
    exec poetry run pyls "$@"
    exit
fi

exec pyls "$@"
