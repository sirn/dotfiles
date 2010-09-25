# Path
# $PYTHON_HOME usually different by each packaging manager method
# +-----------+----------------------------------------------------------
# | Homebrew  | `brew --prefix python`
# | MacPython | /Library/Frameworks/Python.framework/Versions/0.0
# | OSX       | /System/Library/Frameworks/Python.framework/Versions/0.0
# | Linux     | /usr/
# +-----------+----------------------------------------------------------
# Only setup Homebrew for the time being.
_add_python() {
  if [[ -e $1 ]]; then
    export PATH=$1/bin:$PATH
    if [[ -z $PYTHON_PATH ]]; then
      export PYTHON_PATH=$1/bin
    fi
  fi
}

if [[ -n `brew 2>/dev/null` ]]; then
  brew --prefix python python3 | while read prefix; do
    _add_python $prefix
  done
fi

# VirtualEnv
if [[ -e $PYTHON_PATH/virtualenvwrapper.sh ]]; then
    # Avoid loading VirtualEnvWrapper twice, since it is extremely slow
    if [[ -z $VIRTUALENVWRAPPER_PYTHON ]]; then
        export WORKON_HOME=$HOME/.virtualenvs
        export VIRTUAL_ENV_DISABLE_PROMPT=yes
        source $PYTHON_PATH/virtualenvwrapper.sh
        workon default
    fi
fi

# vim:ft=zsh