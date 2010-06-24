if [ -e ~/.local/java ]; then
    export JAVA_HOME=~/.local/java
else
    if [ $is_osx ]; then
        export JAVA_HOME="/Library/Java/Home"
    else
        export JAVA_HOME="/usr/java"
    fi
fi

# Clojure stuff

if [ -e /usr/local/Cellar/clojure-contrib/1.1.0 ]; then
    export CLASSPATH="$CLASSPATH:/usr/local/Cellar/clojure-contrib/1.1.0/clojure-contrib.jar"
fi

# vim:ft=zsh
