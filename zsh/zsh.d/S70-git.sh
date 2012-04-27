if [[ -n `brew 2>/dev/null` ]]; then
    if [[ -e `brew --prefix hub` ]]; then
        alias git=hub
    fi
fi
