#!/bin/sh
#
# Wrapper over pyls to run pyls within a project if pyproject.toml is define
# and configured with poetry.
#

BASE=$(pwd -P) || exit 1

# A hack to use pyls inside a project inside Vagrant. This doesn't always make
# sense so we will need to touch .vagrant/use_vagrant_pyls to activate this
# hack.
if [ -f "$BASE/Vagrantfile" ] && [ -f "$BASE/.vagrant/use_vagrant_pyls" ]; then
    exec vagrant ssh -c "
if [ -f ~/.profile ]; then
   . ~/.profile
fi

if [ -f /vagrant/pyproject.toml ] && grep -q poetry /vagrant/pyproject.toml; then
   cd /vagrant
   exec poetry run pyls $*
   exit
fi

exec pyls $*
exit
" # END-COMMAND
    exit
fi

if [ -f "$BASE/pyproject.toml" ] && grep -q poetry "$BASE/pyproject.toml"; then
    exec poetry run pyls "$@"
    exit
fi

exec pyls "$@"
