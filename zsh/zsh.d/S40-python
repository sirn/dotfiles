# Paths
if [ -e /Library/Frameworks/Python.framework/Versions/2.6 ]; then
    export PYTHON_PATH=/Library/Frameworks/Python.framework/Versions/2.6/bin
    export PATH=$PYTHON_PATH:$PATH
fi

# VirtualEnv
if [ -e $PYTHON_PATH/virtualenvwrapper.sh ]; then
    export WORKON_HOME=$HOME/.virtualenvs
    source $PYTHON_PATH/virtualenvwrapper.sh
    workon default
fi

# Prompt
function _prompt_virtualenv {
    if [ $VIRTUAL_ENV ]; then
        echo " (py:%U`basename \"$VIRTUAL_ENV\"`%u)"
    fi
}

# vim:ft=zsh
