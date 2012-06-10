# Path
# $PYTHON_HOME usually different by each packaging manager method
# +-----------+----------------------------------------------------------
# | Homebrew  | `brew --prefix python`
# | MacPython | /Library/Frameworks/Python.framework/Versions/0.0
# | OSX       | /System/Library/Frameworks/Python.framework/Versions/0.0
# | Linux     | /usr/
# +-----------+----------------------------------------------------------

# Default path
export VIRTUALENV_PATH=/usr/local/bin

# Homebrew
if [[ -n `brew 2>/dev/null` ]]; then
  export VIRTUALENV_PATH=/usr/local/share/python
  export PATH=/usr/local/share/python:$PATH
fi

# VirtualEnv
if [[ -e $VIRTUALENV_PATH/virtualenvwrapper.sh ]]; then
    if [[ -z $VIRTUALENVWRAPPER_PYTHON ]]; then
        export WORKON_HOME=$HOME/.virtualenvs
        export VIRTUAL_ENV_DISABLE_PROMPT=yes
        source $VIRTUALENV_PATH/virtualenvwrapper.sh
        workon default
    fi
fi

# vim:ft=zsh
