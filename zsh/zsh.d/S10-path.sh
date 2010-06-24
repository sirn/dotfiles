# Check if the provided directory exist and append $PATH if they do.
_add_to_exec() {
    if [[ -e $1 ]]; then
        export PATH="$1/bin:$PATH"
        if [ -e "$1/sbin" ]; then
            export PATH="$1/sbin:$PATH"
        fi
    fi
}

# Reset original path
export PATH="/bin:/sbin"

# System paths
_add_to_exec /usr
_add_to_exec /usr/local
_add_to_exec /opt/local
_add_to_exec /Developer/usr

# Local paths
_add_to_exec ~/.local
_add_to_exec ~/.gem/ruby/1.8

# Application-specific
_add_to_exec /Library/PostgreSQL/8.4

# vim:ft=zsh