# Paths
if [[ -n `brew 2>/dev/null` ]]; then
  local python_home=`brew --prefix python`
else
  local python_home=/Library/Frameworks/Python.framework/Versions/2.7
fi

if [[ -e $python_home ]]; then
    export PYTHON_PATH=$python_home/bin
    export PATH=$PYTHON_PATH:$PATH
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
