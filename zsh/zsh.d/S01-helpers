# Check if the home path exist, then append $PATH
_add_path() {
    if [ -e $1 ]; then
        export PATH="$1/bin:$PATH"
        if [ -e "$1/sbin" ]; then
            export PATH="$1/sbin:$PATH"
        fi
    fi
}
