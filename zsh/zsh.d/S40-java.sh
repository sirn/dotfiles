# Paths
local java_local=~/.local/java

if [[ -e $java_local/bin ]]; then export JAVA_HOME=$java_local
elif [[ -n $IS_OSX ]]; then       export JAVA_HOME=/Library/Java/Home
elif [[ -e /usr/java ]]; then     export JAVA_HOME=/usr/java
fi

export CLASSPATH=$JAVA_HOME/lib/\*
if [[ $java_local != $JAVA_HOME ]]; then
    export CLASSPATH=$CLASSPATH:$java_local/lib/\*
fi

# Brew-installed stuffs
function _add_brew_classpath {}
if [[ -n `brew 2>/dev/null` ]]; then
    function _add_brew_classpath {
        brew --prefix $* | while read prefix; do
            if [[ -e $prefix ]]; then
                export CLASSPATH=$CLASSPATH:$prefix/\*
            fi
        done
    }
fi
_add_brew_classpath rlwrap
_add_brew_classpath clojure clojure-contrib
_add_brew_classpath jython

# vim:ft=zsh
