# Paths
if [[ -n `brew 2>/dev/null` ]]; then
  local python_home=`brew --prefix python`
  local python3_home=`brew --prefix python3`
else
  local python_home=/Library/Frameworks/Python.framework/Versions/2.7
fi

if [[ -e $python_home ]]; then
    export PYTHON_PATH=$python_home/bin
    export PATH=$PYTHON_PATH:$PATH
fi

# Python 3
if [[ -e $python3_home ]]; then
    export PATH=$python3_home/bin:$PATH
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
